# This script is to make Wildtrax ARU data output cleaner


# This script takes raw detection data from Wildtrax data download
# and it saves four data frames

# IMPORTANT!
# This script removes all species detections unless they are marked
# as "Confident" or "Confirmed"

# -1- Project_ARU_Species_List_v#.csv : A basic species list
# -2- Project_ARU_Deployment_Data_v#.csv :  Deployment information with start date, end date, and duration (effort)
# -3- Project_ARU_Detection_Data_v#.csv : Detection data. Just the raw detection data, but a bit cleaned
# -4- Project_ARU_Independent_Detections_v#.csv : Independent detections, summarized with group count, based on a time threshold you specify


# Created by Laura Nicole Stewart
# laura.nicole.stewart@gmail.com 


##### 0. Set up workspace ####

rm(list=ls())

library(tidyr)
library(dplyr)
library(stringr)

version<-"v5"
project<-"Tuyeta" # version and project for naming saved csvs


setwd("C:/Users/laura/Documents/Wildco/3. Data and scripts/1. Master data")


#### 1. Load, clean, and merge detection data ####

# Load station covariates
cov<-read.csv("Tuyeta_Station_Covariates_v4.csv")
latlongs<-cov[,c("location","latitude","longitude")]
head(latlongs)

# In my project, the location names are not correct.
# Need to fix them

dat1<-read.csv("../4. Raw data downloads/Tuyeta_BMS_basic_summary.csv")
dat2<-read.csv("../4. Raw data downloads/Tuyeta_TTEA_basic_summary.csv")
dat3<-read.csv("../4. Raw data downloads/Tuyeta_TUY_basic_summary.csv")

head(unique(dat1$location)) # needs left pad 0s cluster and station
head(unique(dat2$location)) # looks good
head(unique(dat3$location)) # needs left pad 0s on cluster and station

dat13 = rbind(dat1, dat3)

dat13$location_1 <- rep(NA, nrow(dat13))
dat13$location_2 <- rep(NA, nrow(dat13))
dat13$location_3 <- rep(NA, nrow(dat13))
dat13$location_4 <- rep(NA, nrow(dat13))

i=1
for (i in 1: nrow(dat13)){
  dat13$location_1[i] = str_split(dat13$location[i],pattern = "-")[[1]][1]
  dat13$location_2[i] = str_split(dat13$location[i],pattern = "-")[[1]][2]
  dat13$location_3[i] = str_split(dat13$location[i],pattern = "-")[[1]][3]
  dat13$location_4[i] = str_split(dat13$location[i],pattern = "-")[[1]][4]
}

for (i in 1:nrow(dat2)){
  dat2$location_1[i] = str_split(dat2$location[i],pattern = "-")[[1]][1]
  dat2$location_2[i] = str_split(dat2$location[i],pattern = "-")[[1]][2]
  dat2$location_3[i] = str_split(dat2$location[i],pattern = "-")[[1]][3]
  dat2$location_4[i] = str_split(dat2$location[i],pattern = "-")[[1]][4]
}

dat13$location_3<-str_pad(dat13$location_3,3,side="left",pad="0")
dat13$location_4<-str_pad(dat13$location_4,2,side="left",pad="0")

dats<-rbind(dat13,dat2)

str(dats)
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
# noice

# get rid of columns that we don't need
names(birddata)

birddata2=birddata%>%
  select(-project_name,-`ï..organization`,-location_1,-location_2,-location_3,-location_4)

birddata2[birddata2 == " "] <- NA

birddata2$is_verified <- as.factor(birddata2$is_verified)
summary(birddata2$is_verified) # okie dokie, don't know what the deal is there
# probably need to verify species IDs straight on Wildtrax

write.csv(birddata2,paste0(project,"_ARU_Detection_Data_", version, ".csv"),row.names = F)


#### 2. Species List ####

# Extract non-species tags

speciestrue = str_extract_all(birddata2$species_common_name, pattern = "Unidentified*", simplify = T) != "Unidentified"
speciestrue2 = str_extract_all(birddata2$species_common_name, pattern = "Light*", simplify = T) != "Light"
speciestrue3 = str_extract_all(birddata2$species_common_name, pattern = "Moderate*", simplify = T) != "Moderate"
speciestrue4 = str_extract_all(birddata2$species_common_name, pattern = "Heavy*", simplify = T) != "Heavy"
summary(speciestrue2)

spp <- birddata2[speciestrue & speciestrue2 & speciestrue3 & speciestrue4, ] # subset

spp2 <- spp %>% # take just the species and their associated codes
  select(species_common_name, species_code) %>%
  unique() %>%
  arrange(species_common_name)

spp2

write.csv(spp2, paste0(project, "_ARU_Species_List_",version,".csv"), row.names = F )


#### 3. Camera effort ####

str(birddata2)

eff<-birddata2[,c("location","recording_date","method")]
eff<-unique(eff)
table(eff$location)
no_of_recordings<-as.data.frame(table(eff$location))
head(eff)
names(eff)[2] <- "recording_begin"

strptime(eff$recording_begin[1], "%Y-%m-%d %H:%M:%S", tz="UTC")
eff$recording_begin<-as.POSIXct(strptime(eff$recording_begin, "%Y-%m-%d %H:%M:%S", tz="UTC"))

write.csv(eff, paste0(project,"_ARU_Deployment_Data_", version, ".csv"), row.names = F)



#### 3. Independent detections ####

# group count here is tricky
# first, we need to pull the frog call indexes (CI #) separately
# then, we need to deal with the TMTC (to many to count) situations

str(birddata2)
birddata2$year = str_sub(birddata2$recording_date, start = 1, end = 4)
birddata2$year = as.numeric(birddata2$year)
head(birddata2$year)

# first, deal with call indexes

wofr <- birddata2[birddata2$species_code == "WOFR",] # pull all wood frogs
callinds<- birddata2[birddata2$species_code != "WOFR" &  # pull rest of call indexes
                       str_extract_all(birddata2$abundance, pattern = "CI*", simplify = T)=="CI",] #pull call indexes
amphibs = rbind(wofr, callinds)



amphibs$abundance
amphibs[amphibs$abundance == "1","abundance"] <- "CI 1" # change 1 to CI 1
amphibs$abundance <- factor(amphibs$abundance, ordered = T)

summary(amphibs$abundance)
as.integer(amphibs$abundance)
amphibs$abundance

amphib.ind = amphibs %>%
  arrange(location, recording_date) %>%
  group_by(location, species_common_name, year) %>%
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
  filter(species_code != "WOFR" & str_extract_all(birddata2$abundance, pattern = "CI*", simplify = T) != "CI") %>%
  arrange(location, recording_date) %>%
  group_by(location, species_common_name, year) %>%
  summarise(index = max(abundance) ,
            max_unique_name = n_distinct(unique_name),
            max_abundance_estimation = max(abundance_estimation))

bird.ind2=bird.ind%>%
  group_by(location, species_common_name) %>%
  mutate(abundance = max(max_unique_name, max_abundance_estimation, na.rm = T)) %>%
  select(-max_unique_name, -max_abundance_estimation)

summary(bird.ind2$index)
bird.ind2[!bird.ind2$index %in% c("TMTT", "CI 1", "CI 2", "CI 3"), "index"] <- NA



# Now merge the two

# amphib.ind$index = factor(amphib.ind$index, ordered = T,levels = c("CI 1", "CI 2", "CI 3", "TMTT"))

amphib.ind$index = as.character(amphib.ind$index)
bird.ind2$index = as.character(bird.ind2$index)

ind=rbind(bird.ind2, amphib.ind)


sppfull<-birddata2 %>% 
  select(species_common_name, species_code) %>%
  unique()

ind2<-merge(ind,sppfull)
ind2 = arrange(ind2,location,species_common_name)
ind2 = select(ind2,2,1,3,4,5,6)

head(ind2)

write.csv(ind2, paste0(project,"_ARU_Independent_Detections_",version,".csv"),
          row.names = F)


