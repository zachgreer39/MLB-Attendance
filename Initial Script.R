library(tidyverse)
library(reclin)

##pull stadiums from baseballr game_info data and match to Lahman to get cities
##current Year (2021) stadiums along with 5 years from 2015 through 2019
##toronto and neutral site stadiums were excluded manually
##city & team names not in dataset so unable to declare home team
##because of ATL & TEX opening new stadiums, neutral sites and toronto excluded
##couldn't match 3 stadiums in Lahman dataset so manually added

df_stadiums=rbind(baseballr::get_game_info_sup_petti() %>% 
                    mutate(year=lubridate::year(game_date)) %>%
                    filter(year %in% c(2015:2019, 2021) & game_type=="R") %>%
                    filter(!(venue_id %in% c(
                      14,	##Rogers Centre
                      2397,	##Tokyo Dome
                      2535,	##Hiram Bithorn Stadium
                      2536,	##TD Ballpark
                      2701,	##Estadio de Beisbol Monterrey
                      2735,	##BB&T Ballpark
                      2756,	##Sahlen Field
                      5010,	##Fort Bragg Field
                      5365,	##TD Ameritrade Park
                      5381,	##London Stadium
                      5445))) %>% ##Field of Dreams
                    count(venue_id, venue_name) %>% group_by(venue_id) %>% 
                    filter(n==max(n)) %>% select(-n) %>% ungroup() %>%
                    pair_blocking(Lahman::Parks %>% mutate(venue_name=park.name)) %>%
                    compare_pairs(by="venue_name", default_comparator=lcs()) %>%
                    score_problink() %>% select_n_to_m() %>% link() %>%
                    filter(!is.na(venue_id) &
                             venue_id!=17 & 
                             ##Wrigley Field - matched to Los Angeles Wrigley
                             venue_id!=2680 & 
                             ##Petco Park - matched to Perry Field
                             venue_id!=5325) %>%
                             ##Globe Life Field - not in Lahman Parks dataset
                    transmute(venue_id, venue_key=park.key, 
                            venue_name=venue_name.x, city, state),
                  c(17, "CHI11", "Wrigley Field", "Chicago", "IL"),
                  c(2680, "SAN02", "Petco Park", "San Diego", "CA"), 
                  c(5325, "ARL03", "Globe Life Field", "Arlington", "TX")) %>% 
  mutate(venue_id=as.numeric(venue_id)) %>% arrange(venue_id)

##30 teams
##2 Stadiums opened (Truist, Globe Life)
##matches 31 stadiums in data frame - Rogers Centre



##JSON dataset of baseball stadiums & latlong coordinates from github
##slightly outdated & no cities in dataset 
##dataframe created by extracting the venue_name and coordinates

url=paste0("https://raw.githubusercontent.com/cageyjames/", 
           "GeoJSON-Ballparks/master/ballparks.geojson");
json_stadium_coords=RJSONIO::fromJSON(url)[['features']];
rm(url);

##null vector declared to include in for loop
x=NULL;

for(i in 1:length(json_stadium_coords)){
  if(length(json_stadium_coords[[i]][["properties"]][["Teams"]])==0) {
    x;
  }
  else if(json_stadium_coords[[i]][["properties"]][["Teams"]][[1]][["Class"]]==
          "Majors") {
    x=as.data.frame(
      rbind(
        x, 
        c(venue_name=json_stadium_coords[[i]][["properties"]]$Ballpark, 
          lat=json_stadium_coords[[i]][["geometry"]][["coordinates"]][2],
          long=json_stadium_coords[[i]][["geometry"]][["coordinates"]][1]
        )
      )
    );
  }
};
rm(i, json_stadium_coords);



##rename stadiums from JSON dataset to match Lahman source
##Lahman chosen instead of initial baseballr because of city 
##sanity check on coordinates
##link via venue_name and then add coordinates for Globe Life & Turner

df_stadiums=pair_blocking(
  df_stadiums, 
  x %>% 
    mutate(venue_name=
             ifelse(venue_name=="American Family Field", "Miller Park", 
                    ifelse(venue_name=="T-Mobile Park", "Safeco Field", 
                           ifelse(venue_name=="Oracle Park", "AT&T Park", 
                                  ifelse(venue_name=="loanDepot Park", 
                                         "Marlins Park", venue_name)))))) %>%
  compare_pairs(by="venue_name", default_comparator=lcs()) %>%
  score_problink() %>% select_n_to_m() %>% link() %>% 
  filter(!is.na(venue_id)) %>% 
  transmute(venue_id, venue_key, venue_name=venue_name.x, city, state, 
            lat=ifelse(venue_id==13, 32.45299, ##Globe Life Park
                       ifelse(venue_id==16, 33.44419, lat)), ##Turner Field
            long=ifelse(venue_id==13, -97.04348, 
                        ifelse(venue_id==16, -84.23132, long))) %>% 
  arrange(venue_id)
rm(x);


library(MazamaSpatialUtils)
setSpatialDataDir("~/Desktop")
installSpatialData("USCensusCounties")
loadSpatialData("USCensusCounties")

df_stadiums=df_stadiums %>% 
  left_join(as.data.frame(cbind(state=state.abb, state_full=state.name))) %>% 
  mutate(lat=as.numeric(lat), long=as.numeric(long), 
         county=getUSCounty(long, lat)) %>% 
  pair_blocking(totalcensus::dict_cbsa) %>%
  compare_pairs(by=c("county","state_full"), default_comparator=lcs()) %>%
  score_problink() %>% select_n_to_m() %>% link() %>% arrange(venue_id) %>% 
  filter(!is.na(venue_id)) %>% 
  transmute(venue_id, venue_key, venue_name, lat, long, city, state, 
            county=paste(county.x, "County"), 
            CBSA=as.numeric(ifelse(venue_id==4, 16980, CBSA)), 
            CBSA_title=ifelse(venue_id==4, 
                              "Chicago-Naperville-Elgin, IL-IN-WI", 
                              CBSA_title));
rm(USCensusCounties, USCensusCounties_01, 
   USCensusCounties_02, USCensusCounties_05)



time_period="cbsas";
api_key="b74a4767b1344039a7bc893349a88f2c";


df_covid=read.csv(paste0("https://api.covidactnow.org/v2/", time_period, 
                         ".timeseries.csv?apiKey=", api_key)) %>% 
  semi_join(df_stadiums, by=c("fips"="CBSA")) %>% 
  transmute(date, CBSA=fips, cases=actuals.cases, deaths=actuals.deaths,
            density100k_cases=metrics.caseDensity, 
            infection_rate=metrics.infectionRate, 
            positive_rate=metrics.testPositivityRatio, 
            pop_vaxed=actuals.vaccinationsInitiated, 
            pop_full_vaxed=actuals.vaccinationsCompleted, 
            injections=actuals.vaccinesAdministered, 
            current_hospital=actuals.hospitalBeds.currentUsageTotal, 
            current_hospital_covid=actuals.hospitalBeds.currentUsageCovid, 
            current_icu=actuals.icuBeds.currentUsageTotal, 
            current_icu_covid=actuals.icuBeds.currentUsageCovid, 
            capacity_icu=actuals.icuBeds.capacity)
rm(api_key, time_period)





df_games=baseballr::get_game_info_sup_petti() %>% 
  mutate(year=lubridate::year(game_date)) %>%
  filter(year %in% c(2015:2019, 2021) & game_type!="E" & 
           game_type!="S" & game_type!="A") %>% 
  semi_join(df_stadiums, by=c("venue_id")) %>% 
  transmute(game_date, game_pk, home=str_to_upper(str_sub(game_id, 19, 21)), 
            away=str_to_upper(str_sub(game_id, 12, 14)), venue_id, game_type, 
            start_time, temperature, other_weather, wind, attendance)




