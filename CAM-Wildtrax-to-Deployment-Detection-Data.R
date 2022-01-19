# This script is to make Wildtrax camera data output cleaner
# and create a deployment spreadsheet 

# Created by Laura Nicole Stewart
# And Chris Beirne
# laura.nicole.stewart@gmail.com 


# TO DO WITH THIS SCRIPT
# 1. VNA TO NA
# 2. FINISH GROUP COUNT & INDEPENDENT
# 3. ACTIVE CAMERA TIME IN DEPLOYMENT SHEET

#### 0. Set up and load data ####

library(lubridate)
library(dplyr)
library(tidyr)
library(mefa4)
library(stringr)

version<-"v4"

independent <- 30 # Set the "independence" interval in minutes

setwd("C:/Users/laura/Documents/Wildco/3. Data and scripts/1. Master data")

data<-read.csv("../4. Raw data downloads/NWTBM_Tuyeta_Biodiversity_Project_report.csv")

head(data) 
tail(data) # the way this is ordered makes no sense to me but it doesn't really matter
str(data) # pretty much everything is a character "chr"



#### 1. Clean data ####

data$location<-as.factor(data$location) # tell R location is a factor not a character
data$field_of_view<-as.factor(data$field_of_view)
data$common_name<-as.factor(data$common_name)
data$age_class<-as.factor(data$age_class)
data$sex<-as.factor(data$sex)
data$number_individuals<-as.numeric(data$number_individuals) # as NUMERIC this time
data$field_of_view<-as.factor(data$field_of_view)
summary(data$field_of_view)

as.POSIXct(head(data$date_detected))
as.POSIXct(head(data$date_detected), tz = "MST") # these were deployed in MST
data$date_detected = as.POSIXct(data$date_detected, tz = "MST")

str(data)
summary(data)


data=arrange(data,location,date_detected)


#### 2. Create Deployment dataframe using START and END tags ####


deployment_data = data[,c("project", "location","date_detected","field_of_view")]
deployment_data = deployment_data %>%
  subset(field_of_view == "START - First Good Image in FOV" | field_of_view == "END - Last Good Image in FOV")
deployment_data=arrange(deployment_data,location,date_detected)  
head(deployment_data)
deployment_data=unique(deployment_data) #START and END will be duplicated if there
#                                         is more than one tag on those images

# now bring in the first and last images into this data frame
# in case they haven't been tagged as START or END


firstnlast=data[1,c("project", "location","date_detected","field_of_view")]
f.l.data<-data[data$field_of_view != "Out of Range",]
i=1
for (i in 2: nrow(f.l.data)){
  if (f.l.data$location[i] != f.l.data$location[i-1]){
    firstnlast=rbind(firstnlast,f.l.data[(i-1):i,c("project", "location","date_detected","field_of_view")])
  }
}
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

mistakes.new=deploy_full[1,]
mistakes.new[1,]<-rep(NA,4)

for (i in 2: nrow(deploy_full)){
  if (deploy_full$field_of_view[i] == deploy_full$field_of_view[i-1]){
    mistakes.new=rbind(mistakes.new,deploy_full[(i-1):i,])
  }
}


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

write.csv(deploy_wide, paste0("Tuyeta_Deployment_Data_", version, ".csv"),row.names = F)



#### 3. Clean data and variable names for detection data ####


str(data)

names(data)[13]
names(data)[13]<-"age"
names(data)[15]
names(data)[15]<- "count"

lessdata = data %>%
  select(-project, - organization, -buffer_radius_m, -scientific_name, -daily_weather_station_nm,
         -daily_weather_station_elevation, -daily_weather_station_distance, -daily_min_temp,
         -daily_max_temp, -daily_mean_temp, -daily_total_rain_mm, -daily_total_snow_cm, -daily_precipitation_mm,
         -daily_snow_on_ground_cm, -hourly_weather_station_nm, -hourly_weather_station_elevation,
         -hourly_weather_station_distance, -hourly_temp, -hourly_dew_point, -hourly_rel_humidity,
         -hourly_precipitation_mm, -hourly_wind_direction, -hourly_wind_speed, -hourly_visibility_km,
         -hourly_station_pressure, -hourly_humidex, -hourly_wind_chill, -hourly_weather_attributes,
         -camera_make, -camera_model, -serial_number)

str(lessdata)


#### 4. Independent detections and Group Count ####

# Remove observations without animals detected
dat <- lessdata[lessdata$common_name!="NONE",]

# Order the dataframe by location, date
dat <- dat[order(dat$location, dat$date_detected),]
head(dat)

dat <- dat %>%
  arrange(location, date_detected) %>%
  group_by(location, common_name) %>%
  mutate(duration = int_length(lag(date_detected) %--% date_detected) )

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


# This is where we need the Minimum.Group.Size

dat$tag_comments
group<-str_match(dat$tag_comments, "gc")
group<-as.factor(group)
summary(group)
head(group)
dat$tag_comments[! is.na(group)]


# And Minimum.Species.Group.Size


demcomments<-dat$image_comments[str_detect(dat$image_comments, "group count = ")]
str_locate(demcomments, "group count = ")
pos<-str_locate(demcomments, "group count = ")[,2]
str_sub(demcomments, start = pos+1, end = pos+3)
tmp=str_sub(demcomments, start = pos+1, end = pos+3)
tmp[390]<-paste0(tmp[390],", 2")
tmp
str_replace_all()



# Calculate the event length and size 

# find out the last and the first of the time in the group
top <- dat %>% group_by(Event.ID) %>% top_n(1,date_detected) %>% select(Event.ID, date_detected)
bot <- dat %>% group_by(Event.ID) %>% top_n(-1,date_detected) %>% select(Event.ID, date_detected)
names(bot)[2] <- c("Date_Time.Captured_end")
dec_no <- dat %>% group_by(Event.ID) %>% summarise(n())
event_grp <- dat %>% group_by(Event.ID) %>% summarise(max(Minimum.Group.Size))

# caculate the duration
diff <-  top %>% left_join(bot, by="Event.ID") %>%
  mutate(duration=abs(int_length(Date_Time.Captured %--% Date_Time.Captured_end))) %>%
  left_join(event_grp, by="Event.ID")%>%
  left_join(dec_no, by="Event.ID")

# Remove duplicates
diff <- diff[duplicated(diff)==FALSE,]

names(diff) <- c("Event.ID","Date_Time.end","Date_Time.start","Event.Duration","Event.Groupsize","Event.Observations")
diff$Date_Time.end<-NULL;diff$Date_Time.start<-NULL
dat$duration <-NULL
# Merge the data
dat <-  dat %>%
  left_join(diff,by="Event.ID")

# Subset to the first observation in each event

# Subset to 30 minute indepedenents
ind.dat <- dat[!duplicated(dat$Event.ID),]
ind.dat <- as.data.frame(ind.dat)
ind.dat$Species <-as.factor(ind.dat$Species)

# Save it for a rainy day
write.csv(ind.dat, paste0(dat$Project.ID[1], "_",independent ,"min_Independent.csv"), row.names = F)
