# This script is to make Wildtrax ARU data output cleaner


# TO DO with this one
# independent detections per station
# deployment data


# Created by Laura Nicole Stewart
# laura.nicole.stewart@gmail.com 


##### 0. Set up workspace ####

rm(list=ls())


library(dplyr)
library(tidyr)
library(stringr)

version<-"v3"
project<-"Tuyeta" # version and project for naming saved csvs


setwd("C:/Users/laura/Documents/Wildco/3. Data and scripts/1. Master data")


#### 1. Load and bind data ####

cov<-read.csv("Tuyeta_Station_Covariates.csv")
latlongs<-cov[,c("location","latitude","longitude")]
head(latlongs)

dat1<-read.csv("../4. Raw data downloads/ECCC_Boreal_Monitoring_Strategy_CWS_Northern_Region_2020_-_Tuyeta_report.csv")
dat2<-read.csv("../4. Raw data downloads/ECCC_Tuyeta_PA_CWS_Northern_Region_2020_report.csv")
dat3<-read.csv("../4. Raw data downloads/ECCC_Tuyeta_PA_CWS_Northern_Region_2021_report.csv")
dat2$location_2<-str_pad(dat2$location_2,2,side="left",pad="0")
names(dat3)[! names(dat3) %in% names(dat2)]
dat4<-dat3[,names(dat3)[names(dat3) %in% names(dat2)]]
dats<-rbind(dat1,dat2,dat4)

str(dats)
dats$location_3<-str_pad(dats$location_3,3,side = "left",pad="0")
dats$location_4<-str_pad(dats$location_4,2,side="left",pad="0")
dats$location<-str_c(dats$location_1,dats$location_2,dats$location_3,dats$location_4,sep="-")
head(dats$location)

# merge with lat longs
dats<-subset(dats, select = -c(latitude, longitude))
birddata<-merge(latlongs,dats, all.y=T, by = "location")
birddata$latitude<-as.numeric(birddata$latitude)
summary(birddata$latitude)
summary(as.factor(birddata[is.na(birddata$latitude),"location"]))
# these are stations that have no lat long

# get rid of columns that we don't need
birddata2=birddata%>%
  select(-project,-organization,-status,-buffer_radius_m, -scientific_name,
         -daily_weather_station_nm,-daily_weather_station_elevation,-daily_weather_station_distance,
         -daily_min_temp,-daily_max_temp,-daily_mean_temp,-daily_total_rain_mm,
         -daily_total_snow_cm,-daily_precipitation_mm,-daily_snow_on_ground_cm,
         -hourly_weather_station_nm,-hourly_weather_station_elevation,-hourly_weather_station_distance,
         -hourly_temp,-hourly_dew_point,-hourly_rel_humidity,-hourly_station_pressure,
         -hourly_humidex,-hourly_wind_chill,-hourly_wind_direction,-hourly_weather_attributes,
         -hourly_precipitation_mm,-hourly_wind_speed,-hourly_visibility_km,-land_features,
         -equipment_used,-tagged_in_wildtrax,-location_1,-location_2,-location_3,-location_4)

birddata2$recording_date<-(birddata2$recording_date)
head(birddata2$recording_date)
? as.POSIXct.date

write.csv(birddata,paste0(project,"_ARU_Detection_Data_", version, ".csv"),row.names = F)


#### 2. Independent detections ####
str(birddata2)


birddata2 %>%
  group_by(location) %>%
  summarise()
