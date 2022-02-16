# This script is to make Wildtrax camera data output cleaner
# and create a deployment spreadsheet, pull group count from the comments, 
# and create an independent detections spreadsheet

# Created by Laura Nicole Stewart
# And Chris Beirne
# laura.nicole.stewart@gmail.com 


#### 0. Set up and load data ####

library(lubridate)
library(dplyr)
library(tidyr)
library(mefa4)
library(stringr)

version<-"v7"
project<-"Tuyeta" # version and project for naming saved csvs

independent <- 30 # Set the "independence" interval in minutes

setwd("C:/Users/laura/Documents/Wildco/3. Data and scripts/1. Master data")

data<-read.csv("../4. Raw data downloads/NWTBM_Tuyeta_Biodiversity_Project_report.csv")

head(data) 
tail(data) # the way this is ordered makes no sense to me but it doesn't really matter
str(data) # pretty much everything is a character "chr"



#### 1. Clean data and save species list ####

data$location<-as.factor(data$location) # tell R location is a factor not a character
data$field_of_view<-as.factor(data$field_of_view)
data$common_name<-as.factor(data$common_name)
data$age_class<-as.factor(data$age_class)
data$sex<-as.factor(data$sex)
data$count<-as.numeric(data$number_individuals) # as NUMERIC this time
data$number_individuals<-NULL
data$field_of_view<-as.factor(data$field_of_view)
summary(data$field_of_view)

data[data == "VNA"]<-NA

as.POSIXct(head(data$date_detected))
as.POSIXct(head(data$date_detected), tz = "MST") # these were deployed in MST
data$date_detected = as.POSIXct(data$date_detected, tz = "MST")

str(data)
summary(data)


data=arrange(data,location,date_detected)

spp <- data %>% 
  filter(species_rank %in% c("Species", "Subspecies")) %>%
  select(common_name, scientific_name, species_class) %>%
  unique() %>%
  arrange(desc(species_class), common_name) %>%
  as.data.frame()

write.csv(spp, "Tuyeta_CAM_Species_List.csv", row.names = F)


#### 2. Create deployment dataframe using START and END tags ####


deployment_data = data[,c("project", "location","date_detected","field_of_view")]
deployment_data = deployment_data %>%
  subset(field_of_view == "START - First Good Image in FOV" | field_of_view == "END - Last Good Image in FOV")
deployment_data=arrange(deployment_data,location,date_detected)  
head(deployment_data)
deployment_data=unique(deployment_data) #START and END will be duplicated if there
#                                         is more than one tag on those images

# now bring in the first and last images into this data frame
# in case they haven't been tagged as START or END

# start with the first row
firstnlast=data[1,c("project", "location","date_detected","field_of_view")]

# now add every row where the station name changes over
f.l.data<-data[data$field_of_view != "Out of Range",] #except not the out of range images
i=1
for (i in 2: nrow(f.l.data)){
  if (f.l.data$location[i] != f.l.data$location[i-1]){
    firstnlast=rbind(firstnlast,f.l.data[(i-1):i,c("project", "location","date_detected","field_of_view")])
  }
}
# now add the last last row
firstnlast=rbind(firstnlast,f.l.data[nrow(f.l.data),c("project", "location","date_detected","field_of_view")]) # last row

firstnlast=firstnlast %>% 
  mutate(new.FOV = rep(c("START - First Good Image in FOV", "END - Last Good Image in FOV"),(nrow(firstnlast))/2)) %>%
  mutate(field_of_view = new.FOV)%>%
  select(-new.FOV)


summary(firstnlast)
summary(table(firstnlast$location)==2) # should all be TRUE

deploy_full=rbind(deployment_data,firstnlast)
head(deploy_full)
deploy_full=arrange(deploy_full,location,date_detected)

dups=deploy_full[duplicated(deploy_full),]
deploy_full=unique(deploy_full)

# time to look for mistakes again

mistakes.fieldofview=deploy_full[1,]
mistakes.fieldofview[1,]<-rep(NA,4)

for (i in 2: nrow(deploy_full)){
  if (deploy_full$field_of_view[i] == deploy_full$field_of_view[i-1]){
    mistakes.fieldofview=rbind(mistakes.fieldofview,deploy_full[(i-1):i,])
  }
}

mistakes.fieldofview

# add a deployment id in order to turn long format into wide format

deploy_full$deployment_id = rep(NA,nrow(deploy_full))
sort(rep(paste0("d",1:(nrow(deploy_full)/2)),2)) # sure, whatever
ident<-sort(rep(paste0("d",1:(nrow(deploy_full)/2)),2))
deploy_full$deployment_id = ident

summary(table(deploy_full$location)%%2 == 0) # do all stations have an even number of rows?
# if any are false, then the answer is no, and there is a problem
m=table(deploy_full$location)%%2 == 0
m[m==FALSE] #which ones have errors?

head(spread(deploy_full,field_of_view,date_detected))
str(deploy_full)

deploy_wide=deploy_full%>%
  group_by(field_of_view)%>%
  mutate(row=row_number())%>%
  pivot_wider(
    names_from = c(field_of_view),
    values_from = date_detected
  ) %>%
  select(-row)%>%
  select(c(project,location,`START - First Good Image in FOV`,`END - Last Good Image in FOV`,
           deployment_id))%>%
  as.data.frame()

str(deploy_wide)
deploy_wide$Camera.Failure.Details<-rep(NA,nrow(deploy_wide))
# change variable names to standardized
names(deploy_wide)<- c("Project.ID",	"location",
                       "Camera.Deployment.Begin.Date",	"Camera.Deployment.End.Date",
                       "Deployment.ID",	"Camera.Failure.Details")

deploy_wide = deploy_wide %>% mutate(duration = Camera.Deployment.End.Date - Camera.Deployment.Begin.Date)
class(deploy_wide$duration)

write.csv(deploy_wide, paste0(project,"_CAM_Deployment_Data_", version, ".csv"),row.names = F)



#### 3. Clean data and variable names for detection data ####


str(data)


lessdata = data %>%
  select(-project, - organization, -buffer_radius_m, -daily_weather_station_nm,
         -daily_weather_station_elevation, -daily_weather_station_distance, -daily_min_temp,
         -daily_max_temp, -daily_mean_temp, -daily_total_rain_mm, -daily_total_snow_cm, -daily_precipitation_mm,
         -daily_snow_on_ground_cm, -hourly_weather_station_nm, -hourly_weather_station_elevation,
         -hourly_weather_station_distance, -hourly_temp, -hourly_dew_point, -hourly_rel_humidity,
         -hourly_precipitation_mm, -hourly_wind_direction, -hourly_wind_speed, -hourly_visibility_km,
         -hourly_station_pressure, -hourly_humidex, -hourly_wind_chill, -hourly_weather_attributes,
         -camera_make, -camera_model, -serial_number)

str(lessdata)

write.csv(lessdata, paste0(project, "_CAM_Detection_Data_", version,".csv" ), row.names=F)


#### 4. Independent detections and group count ####

# Remove observations without animals detected
dat <- lessdata[lessdata$common_name!="NONE",]

# Order the dataframe by location, date
dat <- dat[order(dat$location, dat$date_detected),]
head(dat)

dat <- dat %>%
  arrange(location, date_detected) %>%
  group_by(location, common_name) %>%
  mutate(duration = int_length(lag(date_detected) %--% date_detected) )

str(dat)

# loop that assigns group ID
dat$Event.ID <- 9999
mins <- independent
seq <- as.numeric(paste0(nrow(dat),0))
seq <- round(seq,-(nchar(seq)))
for (i in 2:nrow(dat)) {
  dat$Event.ID[i-1]  <- paste0("E",format(seq, scientific = F))
  if(is.na(dat$duration[i]) | abs(dat$duration[i]) > (mins * 60)){
    seq <- seq + 1
  }
}

# Update the information for the last row
# group ID  for the last row
if(dat$duration[nrow(dat)] < (mins * 60)|
   is.na(dat$duration[nrow(dat)])){
  dat$Event.ID[nrow(dat)] <- dat$Event.ID[nrow(dat)-1]
} else{
  dat$Event.ID[nrow(dat)] <- paste0("E",format(seq+1, scientific = F))
}


# Minimum group size at the tag level (includes age and sex)

tpos<-str_locate(dat$tag_comments, "gc")[,2]
tmptag=str_sub(dat$tag_comments, start = tpos+1, end = tpos+3)
tendpos=str_locate(tmptag,"\\D")[,2]
tendpos[is.na(tendpos)]<-2
dat$groupcount_tag<- str_sub(tmptag,start = 1, end = tendpos-1)


# Minimum group size by species, regardless of age and sex

pos<-str_locate(dat$image_comments, "group count = ")[,2]
tmp=str_sub(dat$image_comments, start = pos+1, end = pos+3)

endpos=str_locate(tmp,"\\D")[,2]
endpos[is.na(endpos)]<-2
dat$groupcount_species <-str_sub(tmp,start = 1, end = endpos-1)


# Now collapse the thing into independent events
class(dat$count)

as.data.frame(dat[dat$Event.ID == "E1000039",])

events<-dat %>%
  dplyr::group_by(Event.ID, common_name, scientific_name, species_rank, 
                  species_class,age_class, sex) %>%
  dplyr::summarise (gc_species = max(groupcount_species, na.rm = T),
                    gc_tag = max(groupcount_tag, na.rm = T),
                    gc_regular = max(count, na.rm = T))
events$gc_regular[events$gc_regular == -Inf]<-NA

for (i in 1: nrow(events)){
  if (is.na(events$gc_tag [i])){
    events$gc_tag[i] <- events$gc_regular[i]
  }
}
events$gc_tag<-as.numeric(events$gc_tag)

species_only<- events %>%
  group_by(Event.ID, common_name) %>%
  summarise(gc_estimated = sum(gc_tag, na.rm = T))

events2<-merge(events, species_only, by = c("Event.ID","common_name"),all.y=T)

events2 = events2 %>% mutate(mistake = events2$gc_estimated > events2$gc_species)


for (i in 1: nrow(events2)){
  if (is.na(events2$gc_species [i])){
    events2$gc_species[i] <- events2$gc_estimated[i]
  }
}


# look at potential mistakes
# note that just because gc_estimated is bigger than gc_species
# that doesn't mean that there is necessarily a mistake
# because gc_species comes from the comments based on 5 minute threshold
# and gc_estimated comes from the 30 minute threshold

mistakes.groupcount <- events2[events2$mistake == T & !is.na(events2$mistake),]

write.csv(mistakes.groupcount,paste0(project, "_mistakes_groupcount_",version,".csv"))

events2$gc_sp_final<-rep(NA, nrow(events2))

for ( i in 1: nrow(events2)){
  events2$gc_sp_final[i] <- max(events2$gc_estimated[i], events2$gc_species[i])
}

events2<-events2 %>%
  select (-gc_regular, -gc_species, -gc_estimated, -mistake)

names(events2)[8]
names(events2)[8]<-"group_count"
names(events2)[9]
names(events2)[9]<-"species_count"


# Calculate the event length and size 

# find out the last and the first of the time in the group
top <- dat %>% group_by(Event.ID) %>% top_n(1,date_detected) %>% select(Event.ID, date_detected)
bot <- dat %>% group_by(Event.ID) %>% top_n(-1,date_detected) %>% select(Event.ID, date_detected)
names(bot)[2] <- c("date_detected_end")
dec_no <- dat %>% group_by(location, Event.ID) %>% summarise(n())
names(dec_no)[3]<-"number_images"

# calculate the duration
diff <-  top %>% left_join(bot, by="Event.ID") %>%
  mutate(duration=abs(int_length(date_detected %--% date_detected_end))) %>%
  left_join(dec_no, by="Event.ID")

# Remove duplicates
diff <- diff[duplicated(diff)==FALSE,]

# Merge the data
independent_data <-  merge(diff, events2, by = "Event.ID", all.y = T)
str(independent_data)


# Save it for a rainy day
write.csv(independent_data, paste0(project,"_CAM_Independent_Detections_",version, ".csv"), row.names = F)
