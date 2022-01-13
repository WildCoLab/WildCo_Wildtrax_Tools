# This script is meant to turn the raw data downloads into detection data

#### Part I - ARU data ####


##### 0. Set up data and workspace ####

library(dplyr)
library(tidyr)
library(stringr)

version<-"v2"

setwd("C:/Users/laura/Documents/Wildco/3. Data and scripts/1. Master data")

dat1<-read.csv("../4. Raw data downloads/ECCC_Boreal_Monitoring_Strategy_CWS_Northern_Region_2020_-_Tuyeta_report.csv")
dat2<-read.csv("../4. Raw data downloads/ECCC_Tuyeta_PA_CWS_Northern_Region_2020_report.csv")
dat2$location_2<-str_pad(dat2$location_2,2,side="left",pad="0")
dats<-rbind(dat1,dat2)

str(dats)
dats$location_3<-str_pad(dats$location_3,3,side = "left",pad="0")
dats$location_4<-str_pad(dats$location_4,2,side="left",pad="0")
dats$Location<-str_c(dats$location_1,dats$location_2,dats$location_3,dats$location_4,sep="-")
head(dats$Location)


cov<-read.csv("Tuyeta_Station_Covariates.csv")
latlongs<-cov[,c("Location","Latitude","Longitude")]
head(latlongs)


dats<-subset(dats, select = -c(latitude, longitude))
birddata<-merge(latlongs,dats, all.y=T, by = "Location")
birddata$Latitude<-as.numeric(birddata$Latitude)
summary(birddata$Latitude)

birddata2=birddata%>%
  select(-project,-organization,-status,-buffer_radius_m, -scientific_name,
         -daily_weather_station_nm,-daily_weather_station_elevation,-daily_weather_station_distance,
         -daily_min_temp,-daily_max_temp,-daily_mean_temp,-daily_total_rain_mm,
         -daily_total_snow_cm,-daily_precipitation_mm,-daily_snow_on_ground_cm,
         -hourly_weather_station_nm,-hourly_weather_station_elevation,-hourly_weather_station_distance,
         -hourly_temp,-hourly_dew_point,-hourly_rel_humidity,-hourly_station_pressure,
         -hourly_humidex,-hourly_wind_chill,-hourly_wind_direction,-hourly_weather_attributes,
         -hourly_precipitation_mm,-hourly_wind_speed,-hourly_visibility_km,-land_features,
         -equipment_used,-tagged_in_wildtrax,-location,-location_1,-location_2,-location_3,-location_4)

write.csv(birddata,paste0("Tuyeta_ARU_Data_", version, ".csv"),row.names = F)
