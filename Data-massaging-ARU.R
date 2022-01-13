# This script is meant to turn the raw data downloads into detection data

#### Part I - ARU data ####


##### 0. Set up data and workspace ####

library(dplyr)
library(tidyr)
library(stringr)

setwd("C:/Users/laura/Documents/Wildco/3. Data and scripts/4. Raw data downloads")

dat1<-read.csv("ECCC_Boreal_Monitoring_Strategy_CWS_Northern_Region_2020_-_Tuyeta_report.csv")
dat2<-read.csv("ECCC_Tuyeta_PA_CWS_Northern_Region_2020_report.csv")
dat2$location_2<-str_pad(dat2$location_2,2,side="left",pad="0")
dats<-rbind(dat1,dat2)

str(dats)
nchar(dats$location_3)
dats$location_3<-str_pad(dats$location_3,3,side = "left",pad="0")
dats$location_4<-str_pad(dats$location_4,2,side="left",pad="0")
dats$location<-str_c(dats$location_1,dats$location_2,dats$location_3,dats$location_4,sep="-")





# change this here. Need to make a station covariates sheet rather than glean it
# from this like a n00b

camdat<-read.csv("Tuyeta_detection_data_v3.csv")
latlongs<-camdat[,c("location","Latitude","Longitude")]
latlongs<-unique(latlongs)

newloc<-c("BMS-FMP-002-20", "TTEA-01-6084-01") #locations that don't have cameras
newlat<-c(66.24281981, 65.604273 )
newlong<-c(-129.3581903,-129.695867)
newlocs<-as.data.frame(cbind(newloc,newlat,newlong))
names(newlocs)<-names(latlongs)
latlongs<-rbind(latlongs,newlocs)

latlongs<-unique(latlongs)

dats<-subset(dats, select = -c(latitude, longitude))
birddata<-merge(latlongs,dats, all.y=T)
birddata$Latitude<-as.numeric(birddata$Latitude)
summary(birddata$Latitude)

write.csv(birddata,"Tuyeta_ARU_Data_v1.csv",row.names = F)
