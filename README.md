# Wildco Wildtrax Tools
These scripts clean Wildtrax output, then summarise the data into deployment histories and independent detections.

### Tuyeta_example_data.csv
This is an example camera trap dataset, with 6 stations of data, from T'sudé Niliné Tuyeta Indigenous Protected Area.

### Wildtrax_ARU_to_Standardized.R
This script summarizes Wildtrax ARU output into maximum species counts per stations... it is still under development!

### Wildtrax_Camera_to_Standardized.R
This script summarizes Wildtrax camera output. It saves four data frames:
1. Species List : common names, scientific names, species rank, and species class of all the species detected
2. Deployment Data: start dates, end dates, and duration of each active camera deployment
3. Detection Data: Just the raw data downloads, but with irrelevant columns removed (you can choose which ones you consider irrelevant)
4. Independent Detections: Raw detection data, summarized into independent detections based on your desired time threshold. This includes group size which, in this case, is gleaned from the comments section

### Wildtrax_Camera_Data_Exploration.Rmd
This R Markdown imports the detection data and the independent detections, as well as some spatial data, to create several figures and maps to visualize the data. It is based on the WildCo_Single_Site_Exploration but slightly modified to work with Wildtrax data.
