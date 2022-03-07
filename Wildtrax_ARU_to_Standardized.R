# This script is to make Wildtrax ARU data output cleaner


# TO DO with this one
# independent detections per station
# deployment data

# This script takes raw detection data from Wildtrax data download
# and it saves four data frames

# -1- Project_ARU_Species_List_v#.csv : A basic species list
# -2- Project_ARU_Deployment_Data_v#.csv :  Deployment information with start date, end date, and duration (effort)
# -3- Project_ARU_Detection_Data_v#.csv : Detection data. Just the raw detection data, but a bit cleaned
# -4- Project_ARU_Independent_Detections_v#.csv : Independent detections, summarized with group count, based on a time threshold you specify




# Created by Laura Nicole Stewart
# laura.nicole.stewart@gmail.com 


##### 0. Set up workspace ####

rm(list=ls())


library(dplyr)
library(tidyr)
library(stringr)

version<-"v4"
project<-"Tuyeta" # version and project for naming saved csvs


setwd("C:/Users/laura/Documents/Wildco/3. Data and scripts/1. Master data")


#### 1. Load, clean, and merge detection data ####

cov<-read.csv("Tuyeta_Station_Covariates.csv")
latlongs<-cov[,c("location","latitude","longitude")]
head(latlongs)

# In my project, the location names are not correct.
# Need to fix them

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
tail(dats$location)

# merge with lat longs
dats<-subset(dats, select = -c(latitude, longitude))
birddata<-merge(latlongs,dats, all.y=T, by = "location")
birddata$latitude<-as.numeric(birddata$latitude)
summary(birddata$latitude)
# check for stations that have no lat long:
summary(as.factor(birddata[is.na(birddata$latitude),"location"]))
# this is because BMS-MMO-032-28 is the wron location name :eyeroll:

# get rid of columns that we don't need
birddata2=birddata%>%
  select(-project,-organization,-status,-buffer_radius_m,
         -daily_weather_station_nm,-daily_weather_station_elevation,-daily_weather_station_distance,
         -daily_min_temp,-daily_max_temp,-daily_mean_temp,-daily_total_rain_mm,
         -daily_total_snow_cm,-daily_precipitation_mm,-daily_snow_on_ground_cm,
         -hourly_weather_station_nm,-hourly_weather_station_elevation,-hourly_weather_station_distance,
         -hourly_temp,-hourly_dew_point,-hourly_rel_humidity,-hourly_station_pressure,
         -hourly_humidex,-hourly_wind_chill,-hourly_wind_direction,-hourly_weather_attributes,
         -hourly_precipitation_mm,-hourly_wind_speed,-hourly_visibility_km,-land_features,
         -equipment_used,-tagged_in_wildtrax,-location_1,-location_2,-location_3,-location_4)

birddata2[birddata2 == " "] <- NA

write.csv(birddata2,paste0(project,"_ARU_Detection_Data_", version, ".csv"),row.names = F)

#### 2. Species List ####


spp<-birddata2 %>% 
  select(species_english_name, scientific_name, species_code,species_class) %>%
  unique() %>%
  filter(!is.na(scientific_name)) %>%
  arrange(species_class, species_english_name)

spp

write.csv(spp, paste0(project, "_ARU_Species_List_",version,".csv"), row.names = F )


#### 3. Camera effort ####

str(birddata2)

eff<-birddata2[,c("location","recording_date","recording_time","method")]
eff<-unique(eff)
table(eff$location)
no_of_recordings<-as.data.frame(table(eff$location))
head(eff)
eff$recording_begin<-paste(eff$recording_date, eff$recording_time)

strptime(eff$recording_begin[1], "%Y-%m-%d %H:%M:%S", tz="UTC")
eff$recording_begin<-as.POSIXct(strptime(eff$recording_begin, "%Y-%m-%d %H:%M:%S", tz="UTC"))

write.csv(eff, paste0(project,"_CAM_Deployment_Data_", version, ".csv"), row.names = F)



#### 3. Independent detections ####

# group count here is tricky
# first, we need to pull the frog call indexes (CI #) separately
# then, we need to deal with the TMTC (to many to count) situations

str(birddata2)
birddata2$year = str_sub(birddata2$recording_date, start = 1, end = 4)
birddata2$year = as.numeric(birddata2$year)

# first, deal with call indexes

amphibs<- birddata2[birddata2$species_class == "AMPHIBIA",] #pull amphibians
amphibs$abundance
amphibs[amphibs$abundance == "1","abundance"] <- "CI 1" # change 1 to CI 1
amphibs$abundance <- factor(amphibs$abundance, ordered = T)

summary(amphibs$abundance)
as.integer(amphibs$abundance)
amphibs$abundance

amphib.ind = amphibs %>%
  arrange(location, recording_date, recording_time) %>%
  group_by(location, species_english_name, year) %>%
  summarise(index = max(abundance)) %>%
  mutate(abundance = as.integer(index))



# now deal with TMTC and abundance estimation

tpos<-str_locate(birddata2$species_comment, "AE")[,2]
tpos2<-str_locate(birddata2$species_comment, "-")[,2]
tmptag=str_sub(birddata2$species_comment, start = tpos+3, end = tpos2-1)
tmptag
as.numeric(tmptag)
birddata2$abundance_estimation<- as.numeric(tmptag)

summary(birddata2$abundance)
birddata2$abundance <- factor(birddata2$abundance, ordered = T)
summary(birddata2$abundance) 
max(birddata2$abundance) # TMTC is the highest position




bird.ind=birddata2 %>%
  filter(species_class != "AMPHIBIA") %>%
  arrange(location, recording_date, recording_time) %>%
  group_by(location, species_english_name, year) %>%
  summarise(index = max(abundance) ,
            max_count = n_distinct(species_individual_name),
            max_abundance_estimation = max(abundance_estimation))

bird.ind2=bird.ind%>%
  group_by(location, species_english_name) %>%
  mutate(abundance = max(max_count, max_abundance_estimation, na.rm = T)) %>%
  select(-max_count, -max_abundance_estimation)

summary(bird.ind2$index)
bird.ind2[!bird.ind2$index %in% c("TMTT", "CI 1", "CI 2", "CI 3"), "index"] <- NA



# Now merge the two

# amphib.ind$index = factor(amphib.ind$index, ordered = T,levels = c("CI 1", "CI 2", "CI 3", "TMTT"))

amphib.ind$index = as.character(amphib.ind$index)
bird.ind2$index = as.character(bird.ind2$index)

ind=rbind(bird.ind2, amphib.ind)


sppfull<-birddata2 %>% 
  select(species_english_name, scientific_name, species_code,species_class) %>%
  unique()

ind2<-merge(ind,sppfull)
ind2 = arrange(ind2,location,species_english_name)
ind2 = select(ind2,2,1,2,3,5,6,7)

head(ind2)

write.csv(ind2, paste0(project,"_ARU_Independent_Detections_",version,".csv"),
          row.names = F)


