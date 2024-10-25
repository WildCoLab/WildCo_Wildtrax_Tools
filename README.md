# Wildco Wildtrax Tools
These scripts clean Wildtrax output, then summarise the data into deployment histories and independent detections.

### Wildtrax_ARU_to_Standardized.R
This script summarizes Wildtrax ARU output. It saves four data frames: 
1. Species List : common names, scientific names, and species class of all the species detected
2. Deployment Data: time, date, and duration of all recordings transcribed at each station
3. Detection Data: Just the raw data downloads, but with irrelevant columns removed (you can choose which ones you consider irrelevant)
4. Independent Detections: Raw detection data, summarized into max group counts by year at each station. This includes abundance estimation for "Too Many to Count" situations, which is pulled from the species comments.

### Wildtrax_ARU_Data_Exploration.Rmd

Some basic maps and summaries, similar to the Camera_Data_Exploration.

### Wildtrax_Camera_to_Standardized.R
This script summarizes Wildtrax camera output. It saves four data frames:
1. Species List : common names, scientific names, species rank, and species class of all the species detected
2. Deployment Data: start dates, end dates, and duration of each active camera deployment
3. Detection Data: Just the raw data downloads, but with irrelevant columns removed (you can choose which ones you consider irrelevant)
4. Independent Detections: Raw detection data, summarized into independent detections based on your desired time threshold. This includes group size which, in this case, is gleaned from the comments section

### Wildtrax_Camera_Data_Exploration.Rmd
This R Markdown imports the detection data and the independent detections, as well as some spatial data, to create several figures and maps to visualize the data. It is based on the WildCo_Single_Site_Exploration but slightly modified to work with Wildtrax data.
