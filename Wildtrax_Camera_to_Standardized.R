#### Intro ####

# This script is to make Wildtrax camera data output cleaner
# and create a deployment spreadsheet, pull group count from the comments, 
# and create an independent detections spreadsheet

# This script takes raw detection data from Wildtrax data download
# and it saves four data frames

# -1- Project_CAM_Species_List_v#.csv : A basic species list
# -2- Project_CAM_Deployment_Data_v#.csv :  Deployment information with start date, end date, and duration (effort)
# -3- Project_CAM_Detection_Data_v#.csv : Detection data. Just the raw detection data, but a bit cleaned
# -4- Project_CAM_Independent_Detections_v#.csv : Independent detections, summarized with group count, based on a time threshold you specify


# Created by Laura Nicole Stewart
# And Chris Beirne
# laura.nicole.stewart@gmail.com 


#### 0. Set up and load data ####

library(lubridate)
library(dplyr)
library(tidyr)
library(mefa4)
library(stringr)

version<-"v10"
project<-"Tuyeta" # version and project for naming saved csvs

independent <- 30 # Set the "independence" interval in minutes

setwd("C:/Users/laura/Documents/Wildco/3. Data and scripts/1. Master data")

data<-read.csv("../4. Raw data downloads/NWTBM_Tuyeta_Biodiversity_Project_2020_report.csv")

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

data$common_name <- str_to_title(data$common_name)
unique(data$common_name)
data[data$common_name == "Beaver","common_name"] <- "American Beaver"

data=arrange(data,location,date_detected)

spp <- data %>% 
  filter(species_rank %in% c("Species", "Subspecies")) %>%
  select(common_name, scientific_name, species_class) %>%
  unique() %>%
  arrange(desc(species_class), common_name) %>%
  as.data.frame()

write.csv(spp, paste0(project, "_CAM_Species_List_", version, ".csv" ), row.names = F)


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

# change variable names to standardized
names(deploy_wide)<- c("project",	"location",
                       "deployment_begin_date",	"deployment_end_date",
                       "deployment_id")

deploy_wide = deploy_wide %>% mutate(duration = deployment_end_date - deployment_begin_date)
class(deploy_wide$duration)

write.csv(deploy_wide, paste0(project,"_CAM_Deployment_Data_", version, ".csv"),row.names = F)



#### 3. Clean data and variable names for detection data ####


str(data)

# there's a lot of stuff coming out of Wildtrax that I don't want
# Choose below which variables to remove:

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
dat <- lessdata[lessdata$common_name!="None",]

# Order the dataframe by location, date
dat <- dat[order(dat$location, dat$date_detected),]
head(dat)

dat <- dat %>%
  arrange(location, date_detected) %>%
  group_by(location, common_name) %>%
  mutate(duration = int_length(lag(date_detected) %--% date_detected) )

str(dat)

# loop that assigns group ID
dat$event_id <- 9999
mins <- independent
seq <- as.numeric(paste0(nrow(dat),0))
seq <- round(seq,-(nchar(seq)))
for (i in 2:nrow(dat)) {
  dat$event_id[i-1]  <- paste0("E",format(seq, scientific = F))
  if(is.na(dat$duration[i]) | abs(dat$duration[i]) > (mins * 60)){
    seq <- seq + 1
  }
}

# Update the information for the last row
# group ID  for the last row
if(dat$duration[nrow(dat)] < (mins * 60)|
   is.na(dat$duration[nrow(dat)])){
  dat$event_id[nrow(dat)] <- dat$event_id[nrow(dat)-1]
} else{
  dat$event_id[nrow(dat)] <- paste0("E",format(seq+1, scientific = F))
}


# Minimum group size at the tag level (includes age and sex)

tpos<-str_locate(dat$tag_comments, "gc")[,2]
tmptag=str_sub(dat$tag_comments, start = tpos+1, end = tpos+3)
tendpos=str_locate(tmptag,"\\D")[,2]
tendpos[is.na(tendpos)]<-2
dat$groupcount_tag<- str_sub(tmptag,start = 1, end = tendpos-1)


# Now collapse the thing into independent events
class(dat$count) # should be numeric


events<-dat %>%
  # first group by event and the other variables we want to keep
  dplyr::group_by(event_id, common_name, scientific_name, species_rank, 
                  species_class,age_class, sex) %>%
  # then take group count as the max of the comments gc# count (gc_tag)
  # and seperately, as the max of the individual image count (gc_regular)
  dplyr::summarise (gc_tag = max(groupcount_tag, na.rm = T),
                    gc_regular = max(count, na.rm = T))
# replace impossible values with NAs
events$gc_regular[events$gc_regular == -Inf]<-NA

for (i in 1: nrow(events)){
  if (is.na(events$gc_tag [i])){ # where there was no tag comment
    events$gc_tag[i] <- events$gc_regular[i] # fill in group count from gc_regular
  }
}

events$gc_tag<-as.numeric(events$gc_tag)


names(events)[8]
names(events)[8]<-"group_count"
events <- select(events, -gc_regular) # remove the now redundant group count column


# Calculate the event length and size 

# find out the last and the first of the time in the group
top <- dat %>% group_by(event_id) %>% top_n(1,date_detected) %>% select(event_id, date_detected)
bot <- dat %>% group_by(event_id) %>% top_n(-1,date_detected) %>% select(event_id, date_detected)
names(bot)[2] <- c("date_detected_end")
dec_no <- dat %>% group_by(location, event_id) %>% summarise(n())
names(dec_no)[3]<-"number_images"

# calculate the duration
diff <-  top %>% left_join(bot, by="event_id") %>%
  mutate(duration=abs(int_length(date_detected %--% date_detected_end))) %>%
  left_join(dec_no, by="event_id")

# Remove duplicates
diff <- diff[duplicated(diff)==FALSE,]

# Merge the data
independent_data <-  merge(diff, events, by = "event_id", all.y = T)
str(independent_data)


# Save it for a rainy day
write.csv(independent_data, paste0(project,"_CAM_Independent_Detections_",version, ".csv"), row.names = F)
