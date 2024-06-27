

####### Import the necessary libraries ########

library(arrow)
library(tidyverse)
library(dplyr)
library(openxlsx)

df_housing <- read_parquet('https://intro-datascience.s3.us-east-2.amazonaws.com/SC-data/static_house_info.parquet')


########################## DATA PREPARATION ########################## 


################################# WEATHER (COUNTY) ################################# 

unique_counties <- unique(df_housing$in.county)
counties_data <- lapply(unique_counties, function(county) {
  df <- read.csv(paste0('https://intro-datascience.s3.us-east-2.amazonaws.com/SC-data/weather/2023-weather-data/', county, '.csv'))
  row.names(df) <- NULL
  
  # Convert date_time column from character to POSIXct (datetime)
  df$date_time <- as.POSIXct(df$date_time, format = "%Y-%m-%d %H:%M:%S")

  # Filter data for the month of July
  df <- df[format(df$date_time, "%m") == "07", ]
  df$county <- county
  return(df)
})

Weather_DF <- bind_rows(counties_data)
Weather_DF <- na.omit(Weather_DF)
colnames(Weather_DF) <- c("date_time", "dry_bulb_temp", "relative_humidity", 
                          "wind_speed", "wind_direction", 
                          "global_horz_radiation", "direct_norm_radiation", 
                          "diffuse_horz_radiation","county")

file_path_1 <- "C:\\Users\\Rishikesh\\Downloads\\Semester 1\\INTRO_DS_RK\\PROJECT\\DATA\\Final_Data\\Final_Weather_DF.xlsx"
write.xlsx(Weather_DF, file_path_1, rowNames = FALSE)

##########################################################################################

################################# ENERGY DATA ################################# 


###unique_buildings <- unique(df_housing$bldg_id)

Energy_DF <- data.frame()
### Loop over each house
for (i in 1:nrow(df_housing))
{
  building_id <- df_housing[i, "bldg_id"]
  df_building <- read_parquet(paste0('https://intro-datascience.s3.us-east-2.amazonaws.com/SC-data/2023-houseData/', building_id, '.parquet'))
  df_building$bldg_id <- building_id
  ### Convert date_time column and filter July's data
  df_building_july <- df_building[format(as.POSIXct(df_building$time), "%Y-%m") == "2018-07", ]
  Energy_DF <- rbind(Energy_DF,df_building_july)
}


file_path_2 <- "C:\\Users\\Rishikesh\\Downloads\\Semester 1\\INTRO_DS_RK\\PROJECT\\DATA\\Final_Data\\Final_Energy_DF.parquet"
write_parquet(Energy_DF, file_path_2)

##########################################################################################

# Sample_Energy_DF <- Energy_DF[1:100000, ]

########################## MERGE ENERGY AND COUNTY #######################################

Pre_Final_DF <- merge(Energy_DF, Weather_DF, by.x="time", by.y="date_time", all.x=TRUE)

######################### MERGE FINAL DF #################################################

Final_DF <- merge(df_housing, Pre_Final_DF, by.x="bldg_id", by.y="bldg_id", all.x=TRUE)

file_path_3 <- "C:\\Users\\Rishikesh\\Downloads\\Semester 1\\INTRO_DS_RK\\PROJECT\\DATA\\Final_Data\\Final_IDS_DF.xlsx"
write_parquet(Final_DF, file_path_3, rowNames = FALSE)

##########################################################################################


