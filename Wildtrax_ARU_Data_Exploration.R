### ARU data exploration

# This script needs a lot of love
# It currently probably doesn't work


#### 0. Load packages ####
library(tidyr)
library(dplyr)

#### 1. Load and clean data ####

getwd()
birds<-read.csv("ECCC_Boreal_Monitoring_Strategy_CWS_Northern_Region_2020_-_Tuyeta_report.csv")
str(birds)
# location is messed up. Try the location split next time maybe
# filter by species_class maybe
# filter by confidence
# there is one row per individual species

birds$tagged_in_wildtrax<- as.factor(birds$tagged_in_wildtrax)
summary(birds$tagged_in_wildtrax) # all true so we can ignore
birds$confidence<-as.factor(birds$confidence)
summary(birds$confidence) # keep only Confident and Confirmed
birds$species_class<-as.factor(birds$species_class)
summary(birds$species_class) # filter away UNKNOWN and ABIOTIC for now
table(birds[birds$species_class=="MAMMALIA","species_english_name"]) # one beaver and 7 squirrels only
table(birds[birds$species_class=="INSECTA","species_english_name"]) # one woodborer beetle
table(birds[birds$species_class=="AMPHIBIA","species_english_name"])
?filter
birds<-filter(birds, confidence %in% c("Confident", "Confirmed"), 
              ! species_class %in% c("MAMMALIA", "INSECTA", "AMPHIBIA"))

birds$rain<-as.factor(birds$rain)
birds$wind<-as.factor(birds$wind)
birds$noise<-as.factor(birds$noise)
summary(birds$rain)
summary(birds$wind)
summary(birds$noise) # to filter heavy noise or not to filter heavy noise
# let's say not for now

#### 2. Activity plot ####

eff<-birds[,c("location","recording_date","recording_time")]
eff<-unique(eff)
table(eff$location)
no_of_recordings<-as.data.frame(table(eff$location))
head(eff)
eff$recording_begin<-paste(eff$recording_date, eff$recording_time)

strptime(eff$recording_begin[1], "%Y-%m-%d %H:%M:%S", tz="UTC")
eff$recording_begin<-as.POSIXct(strptime(eff$recording_begin, "%Y-%m-%d %H:%M:%S", tz="UTC"))



n.stat <- length(unique(eff$location))

par(mar=c(2,6,1,1))
plot(c(min(eff$recording_begin, na.rm=T), max(eff$recording_begin, na.rm=T)),
     c(1,n.stat), las=1, ylab="", xlab="", type="n", yaxt="n")

# Have the first station plot at the top 
plot.order <- rev(unique(eff$location))

axis(2, at= 1:n.stat, labels= plot.order, las=1, cex.axis=0.4)
#mtext("Camera Deployment ID", 2, 4)
# Make lines for each of the cameras
for(i in 1:length(plot.order))
{
  abline(h=i, col=rgb(0,0,0,0.1))
  tmp <- eff[eff$location==plot.order[i],]
  for(j in 1:nrow(tmp))
  {
    points(tmp$recording_begin[j],
          i, lwd=2, pch = 19, cex = 0.8)
  }
  
}


# changes to make!
# colour by day/night

head(strptime(eff$recording_time, format = "%H:%M:%S", tz="UTC"))
?points
