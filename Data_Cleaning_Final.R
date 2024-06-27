

####### Import the necessary libraries ########

library(dplyr)
library(arrow)
library(readr)
library(tidyverse)

####################### DEPENDENT VARIABLES #######################

#### Clean existing irrelevant objects in the environment ####
rm(list = ls()) 

#### Read the Final Data File ####
Final_DF<- read.csv("C:\\Users\\Rishikesh\\Downloads\\Semester 1\\INTRO_DS_RK\\PROJECT\\DATA\\Final_Data\\Final_IDS_DF.csv")
str(Final_DF)


##### Creating a Copy of the Final Data ####
Final_Copy_DF<- Final_DF
# # summary(Final_Copy_DF)
# Final_Copy_DF <- na.omit(Final_Copy_DF)

#####  Cleaning the DF/ Removing 0's & NA's #####
is_all_zero_or_na <- function(column) {
  all(column == 0 | is.na(column))
}

columns_with_only_zero_or_na <- sapply(Final_Copy_DF, is_all_zero_or_na)
column_names <- names(Final_Copy_DF)[columns_with_only_zero_or_na]
column_names

length(column_names)

Final_Copy_DF <- Final_Copy_DF[, !columns_with_only_zero_or_na]

# count_in_columns <- sum(grepl("^in\\.", names(Final_Copy_DF)))
# count_in_columns


#####  Checking for Negative Values ##### 
has_negative_values <- function(column) {
  any(column < 0)
}

columns_with_negatives <- sapply(Final_DF[sapply(Final_Copy_DF, is.numeric)], has_negative_values)


# Print the names of columns that contain negative values
negative_value_columns <- names(Final_Copy_DF)[columns_with_negatives]
negative_value_columns

Final_Copy_DF$out.electricity.pv.energy_consumption[Final_Copy_DF$out.electricity.pv.energy_consumption < 0 & !is.na(Final_Copy_DF$out.electricity.pv.energy_consumption)] <- 0

# unique_values <- unique(Final_Copy_DF$out.electricity.pv.energy_consumption)
# unique_values


# find_negative_columns <- function(dataframe) {
#   # Apply 'has_negative_values' to each column
#   negative_columns <- sapply(dataframe, has_negative_values)
#   # Return names of columns that have negative values
#   return(names(dataframe)[negative_columns])
# }
# 
# negative_value_columns <- find_negative_columns(Final_Copy_DF)
# negative_value_columns

#out_columns <- grep("^out\\.", names(Final_Copy_DF), value = TRUE)




# Extract these columns and their values into a new dataframe
Out_DF <- Final_Copy_DF
# colnames(Out_DF)

##### Aggregate the dependent variables #####

##### KITCHEN
cols_to_check <- c("out.electricity.range_oven.energy_consumption",
                   "out.electricity.dishwasher.energy_consumption",
                   "out.electricity.refrigerator.energy_consumption",
                   "out.electricity.freezer.energy_consumption",
                   "out.natural_gas.range_oven.energy_consumption",
                   "out.natural_gas.grill.energy_consumption",
                   "out.propane.range_oven.energy_consumption")

missing_cols <- cols_to_check[!cols_to_check %in% names(Out_DF)]
# print("Missing columns:")
# print(missing_cols)



Out_DF$out.kitchen.energy_consumption <- Out_DF$out.electricity.range_oven.energy_consumption +
                                         Out_DF$out.electricity.dishwasher.energy_consumption +
                                         Out_DF$out.electricity.refrigerator.energy_consumption +
                                         Out_DF$out.electricity.freezer.energy_consumption  +
                                         Out_DF$out.natural_gas.grill.energy_consumption 


##### LAUNDRY
cols_to_check2 <- c("out.electricity.clothes_dryer.energy_consumption",
                    "out.natural_gas.clothes_dryer.energy_consumption",
                    "out.electricity.clothes_washer.energy_consumption",
                    "out.propane.clothes_dryer.energy_consumption")

missing_cols2 <- cols_to_check2[!cols_to_check2 %in% names(Out_DF)]
# print("Missing columns:")
# print(missing_cols2)
Out_DF$out.laundry.energy_consumption <- Out_DF$out.electricity.clothes_dryer.energy_consumption +
                                         Out_DF$out.electricity.clothes_washer.energy_consumption


##### HEATING_COOLING
columns_to_sum <- c("out.electricity.heating_fans_pumps.energy_consumption",
                    "out.electricity.heating_hp_bkup.energy_consumption",
                    "out.electricity.heating.energy_consumption",
                    "out.electricity.cooling.energy_consumption",
                    "out.natural_gas.heating_hp_bkup.energy_consumption",
                    "out.natural_gas.heating.energy_consumption",
                    "out.propane.heating_hp_bkup.energy_consumption",
                    "out.propane.heating.energy_consumption",
                    "out.fuel_oil.heating_hp_bkup.energy_consumption",
                    "out.fuel_oil.heating.energy_consumption",
                    "out.natural_gas.fireplace.energy_consumption",
                    "out.electricity.cooling_fans_pumps.energy_consumption")
miss_col<-columns_to_sum[!columns_to_sum %in% names(Out_DF)]
# print("Missing columns:")
# print(miss_col)

Out_DF$out.heating_cooling.energy_consumption <- Out_DF$out.electricity.heating_fans_pumps.energy_consumption +
                                                 Out_DF$out.electricity.heating.energy_consumption +
                                                 Out_DF$out.electricity.cooling.energy_consumption +
                                                 Out_DF$out.natural_gas.fireplace.energy_consumption +
                                                 Out_DF$out.electricity.cooling_fans_pumps.energy_consumption

##### WATER_HEATING
heating_col_missing <- c('out.electricity.hot_water.energy_consumption',
                         'out.fuel_oil.hot_water.energy_consumption',
                         'out.natural_gas.hot_water.energy_consumption',
                         'out.propane.hot_water.energy_consumption')
miss_col <- heating_col_missing[!heating_col_missing %in% names(Out_DF)]
# print("Missing columns:")
# print(miss_col)

Out_DF$out.water_heating.energy_consumption <- Out_DF$out.electricity.hot_water.energy_consumption



##### ELECTRICAL APPLIANCES
electrical_missing<-c('out.electricity.lighting_exterior.energy_consumption',
                      'out.electricity.lighting_garage.energy_consumption',
                      'out.electricity.lighting_interior.energy_consumption',
                      'out.electricity.plug_loads.energy_consumption',
                      'out.electricity.mech_vent.energy_consumption',
                      'out.natural_gas.lighting.energy_consumption', 
                      'out.electricity.ceiling_fan.energy_consumption')

miss_col<-electrical_missing[!electrical_missing %in% names(Out_DF)]
# print("Missing columns:")
# print(miss_col)

Out_DF$out.electrical_appliances.energy_consumption <- Out_DF$out.electricity.lighting_exterior.energy_consumption +
                                                       Out_DF$out.electricity.lighting_garage.energy_consumption +
                                                       Out_DF$out.electricity.lighting_interior.energy_consumption +
                                                       Out_DF$out.electricity.plug_loads.energy_consumption +
                                                       Out_DF$out.electricity.mech_vent.energy_consumption +
                                                       Out_DF$out.natural_gas.lighting.energy_consumption +
                                                       Out_DF$out.electricity.ceiling_fan.energy_consumption


##### OUTDOOR APPLIANCEs
outdoor_missing<-c('out.electricity.hot_tub_heater.energy_consumption',
                   'out.electricity.hot_tub_pump.energy_consumption',
                   'out.electricity.pool_heater.energy_consumption',
                   'out.electricity.pool_pump.energy_consumption',
                   'out.natural_gas.hot_tub_heater.energy_consumption',
                   'out.natural_gas.pool_heater.energy_consumption',
                   'out.electricity.well_pump.energy_consumption')
miss_col<-outdoor_missing[!outdoor_missing %in% names(Out_DF)]
# print("Missing columns:")
# print(miss_col)

Out_DF$out.outdoor_appliances.energy_consumption <- Out_DF$out.electricity.hot_tub_heater.energy_consumption +
                                                    Out_DF$out.electricity.hot_tub_pump.energy_consumption +
                                                    Out_DF$out.electricity.pool_heater.energy_consumption +
                                                    Out_DF$out.electricity.pool_pump.energy_consumption +
                                                    Out_DF$out.natural_gas.hot_tub_heater.energy_consumption +
                                                    Out_DF$out.natural_gas.pool_heater.energy_consumption +
                                                    Out_DF$out.electricity.well_pump.energy_consumption

##### RENEWABLE ENERGY
ren_missing<-c('out.electricity.pv.energy_consumption')
miss_col<-ren_missing[!ren_missing %in% names(Out_DF)]
# print("Missing columns:")
# print(miss_col)
Out_DF$out.renewable_energy.energy_consumption <- Out_DF$out.electricity.pv.energy_consumption


##### TOTAL ENERGY CONSUMPTION
total_missing <-  c('out.electricity.range_oven.energy_consumption',
                    'out.electricity.dishwasher.energy_consumption',
                    'out.electricity.refrigerator.energy_consumption',
                    'out.electricity.freezer.energy_consumption',
                    'out.natural_gas.range_oven.energy_consumption',
                    'out.natural_gas.grill.energy_consumption',
                    'out.propane.range_oven.energy_consumption',
                    'out.electricity.clothes_dryer.energy_consumption',
                    'out.natural_gas.clothes_dryer.energy_consumption',
                    'out.electricity.clothes_washer.energy_consumption',
                    'out.propane.clothes_dryer.energy_consumption',
                    'out.electricity.heating_fans_pumps.energy_consumption',
                    'out.electricity.heating_hp_bkup.energy_consumption',
                    'out.electricity.heating.energy_consumption',
                    'out.electricity.cooling.energy_consumption',
                    'out.natural_gas.heating_hp_bkup.energy_consumption',
                    'out.natural_gas.heating.energy_consumption',
                    'out.propane.heating_hp_bkup.energy_consumption',
                    'out.propane.heating.energy_consumption',
                    'out.fuel_oil.heating_hp_bkup.energy_consumption',
                    'out.fuel_oil.heating.energy_consumption',
                    'out.natural_gas.fireplace.energy_consumption',
                    'out.electricity.cooling_fans_pumps.energy_consumption',
                    'out.electricity.hot_water.energy_consumption',
                    'out.fuel_oil.hot_water.energy_consumption',
                    'out.natural_gas.hot_water.energy_consumption',
                    'out.propane.hot_water.energy_consumption',
                    'out.electricity.lighting_exterior.energy_consumption',
                    'out.electricity.lighting_garage.energy_consumption',
                    'out.electricity.lighting_interior.energy_consumption',
                    'out.electricity.plug_loads.energy_consumption',
                    'out.electricity.mech_vent.energy_consumption',
                    'out.natural_gas.lighting.energy_consumption',
                    'out.electricity.ceiling_fan.energy_consumption',
                    'out.electricity.hot_tub_heater.energy_consumption',
                    'out.electricity.hot_tub_pump.energy_consumption',
                    'out.electricity.pool_heater.energy_consumption',
                    'out.electricity.pool_pump.energy_consumption',
                    'out.natural_gas.hot_tub_heater.energy_consumption',
                    'out.natural_gas.pool_heater.energy_consumption',
                    'out.electricity.well_pump.energy_consumption',
                    'out.electricity.pv.energy_consumption')

miss_col<-total_missing[!total_missing %in% names(Out_DF)]
# print(miss_col)

Out_DF$out.total.energy_consumption <-  Out_DF$out.electricity.range_oven.energy_consumption +
                                        Out_DF$out.electricity.dishwasher.energy_consumption +
                                        Out_DF$out.electricity.refrigerator.energy_consumption +
                                        Out_DF$out.electricity.freezer.energy_consumption +
                                        Out_DF$out.natural_gas.grill.energy_consumption +
                                        Out_DF$out.electricity.clothes_dryer.energy_consumption +
                                        Out_DF$out.electricity.clothes_washer.energy_consumption +
                                        Out_DF$out.electricity.heating_fans_pumps.energy_consumption +
                                        Out_DF$out.electricity.heating.energy_consumption +
                                        Out_DF$out.electricity.cooling.energy_consumption +
                                        Out_DF$out.natural_gas.fireplace.energy_consumption +
                                        Out_DF$out.electricity.cooling_fans_pumps.energy_consumption +
                                        Out_DF$out.electricity.hot_water.energy_consumption +
                                        Out_DF$out.electricity.lighting_exterior.energy_consumption +
                                        Out_DF$out.electricity.lighting_garage.energy_consumption +
                                        Out_DF$out.electricity.lighting_interior.energy_consumption +
                                        Out_DF$out.electricity.plug_loads.energy_consumption +
                                        Out_DF$out.electricity.mech_vent.energy_consumption +
                                        Out_DF$out.natural_gas.lighting.energy_consumption +
                                        Out_DF$out.electricity.ceiling_fan.energy_consumption +
                                        Out_DF$out.electricity.hot_tub_heater.energy_consumption +
                                        Out_DF$out.electricity.hot_tub_pump.energy_consumption +
                                        Out_DF$out.electricity.pool_heater.energy_consumption +
                                        Out_DF$out.electricity.pool_pump.energy_consumption +
                                        Out_DF$out.natural_gas.hot_tub_heater.energy_consumption +
                                        Out_DF$out.natural_gas.pool_heater.energy_consumption +
                                        Out_DF$out.electricity.well_pump.energy_consumption +
                                        Out_DF$out.electricity.pv.energy_consumption


Out_DF <- Out_DF[, !names(Out_DF) %in% total_missing]

str(Out_DF)



###################### INDEPENDENT VARIABLES #######################


#colnames(Out_DF)
Input_DF <- Out_DF
colnames(Input_DF)


# Get list of character columns
character_columns <- names(Input_DF)[sapply(Input_DF, is.character)]
character_columns


Input_DF$in.plug_load_diversity
Input_DF$in.plug_load_diversity <- as.numeric(gsub("%", "", Input_DF$in.plug_load_diversity))

unique(Input_DF$in.misc_pool_pump)
Input_DF$in.misc_pool_pump <- gsub("1.0 HP Pump", "1", Input_DF$in.misc_pool_pump)
Input_DF$in.misc_pool_pump <- gsub("None", "0", Input_DF$in.misc_pool_pump)
Input_DF$in.misc_pool_pump <- as.numeric(Input_DF$in.misc_pool_pump)

unique(Input_DF$in.hot_water_fixtures)
Input_DF$in.hot_water_fixtures <- as.numeric(gsub("% Usage", "", Input_DF$in.hot_water_fixtures))

# unique(Input_DF$in.heating_setpoint_offset_magnitude)
# Input_DF$in.heating_setpoint_offset_magnitude <- as.numeric(gsub("F", "", Input_DF$in.heating_setpoint_offset_magnitude))

# unique(Input_DF$in.geometry_floor_area_bin)
# #Input_DF$in.geometry_floor_area_bin <- gsub("4000+", "4000-5500", Input_DF$in.geometry_floor_area_bin)
# Input_DF$in.geometry_floor_area_bin <- gsub("+", "", Input_DF$in.geometry_floor_area_bin)
# 
# Input_DF[Input_DF$in.geometry_floor_area_bin == "4000-5000+",c('in.geometry_floor_area_bin')]
# 
# # Splitting the range column and converting to a matrix
# split_data <- do.call(rbind, strsplit(Input_DF$in.geometry_floor_area_bin, "-", fixed = TRUE))
# split_data
# # Creating two new columns for lower and upper limits
# Input_DF$in.geometry_floor_area_bin_lowerLimit <- as.numeric(split_data[, 1])
# Input_DF$in.geometry_floor_area_bin_upperLimit <- as.numeric(split_data[, 2])  


# unique(Input_DF$in.cooling_setpoint_offset_magnitude)
# Input_DF$in.cooling_setpoint_offset_magnitude <- as.numeric(gsub("F", "", Input_DF$in.cooling_setpoint_offset_magnitude))
# 
# 
# unique(Input_DF$in.cooling_setpoint)
# Input_DF$in.cooling_setpoint <- as.numeric(gsub("F", "", Input_DF$in.cooling_setpoint))
# 
# unique(Input_DF$in.heating_setpoint)
# Input_DF$in.heating_setpoint <- as.numeric(gsub("F", "", Input_DF$in.heating_setpoint))

#################################################

convert_to_numeric <- function(column) {
  as.numeric(gsub("F", "", column))
}

columns_to_transform <- c("in.heating_setpoint_offset_magnitude","in.cooling_setpoint_offset_magnitude", "in.cooling_setpoint", "in.heating_setpoint")

for (col in columns_to_transform) {
  Input_DF[[col]] <- convert_to_numeric(Input_DF[[col]])
}

################################################

unique(Input_DF$in.misc_extra_refrigerator)
Input_DF$in.misc_extra_refrigerator <- gsub("EF ", "", Input_DF$in.misc_extra_refrigerator)
Input_DF$in.misc_extra_refrigerator <- gsub("None", "0", Input_DF$in.misc_extra_refrigerator)
Input_DF$in.misc_extra_refrigerator <- as.numeric(Input_DF$in.misc_extra_refrigerator)


unique(Input_DF$in.refrigerator)
Input_DF$in.refrigerator <- gsub("EF ", "", Input_DF$in.refrigerator)
Input_DF$in.refrigerator <- gsub(", 100% Usage", "", Input_DF$in.refrigerator)
Input_DF$in.refrigerator <- gsub("None", "0", Input_DF$in.refrigerator)
Input_DF$in.refrigerator <- as.numeric(Input_DF$in.refrigerator)

unique(Input_DF$in.vintage)
Input_DF$in.vintage <- gsub("s", "", Input_DF$in.vintage)
Input_DF$in.vintage <- gsub("<", "", Input_DF$in.vintage)
Input_DF$in.vintage <- as.numeric(Input_DF$in.vintage)



str(Input_DF)
colnames(Input_DF)

# sum_total_energy <- sum(Input_DF$out.total.energy_consumption, na.rm = TRUE)
# sum_total_energy


# Input_DF <- Input_DF[Input_DF$in.city != "Not in a census Place", ]
Input_DF <- Input_DF[!Input_DF$in.city %in% c("In another census Place", "Not in a census Place"), ]

Sample_DF <- Input_DF
Sample_DF$Day <- as.POSIXlt(Sample_DF$time)$mday  # Extract day
unique(Sample_DF$time)
# unique(Input_DF$in.city)
Sample_DF$Day <- Sample_DF[!is.na(Sample_DF$Day), ]
# unique(Input_DF$time)

Sample_DF$time <- as.POSIXct(Sample_DF$time, format = "%Y-%m-%d %H:%M:%S")
Sample_DF$Hour <- as.POSIXlt(Sample_DF$time)$hour # Extract hour
Sample_DF$Hour[is.na(Sample_DF$Hour)] <- 0

Input_DF$Day <- as.POSIXlt(Input_DF$time)$mday  # Extract day
# Input_DF$Day <- Input_DF[!is.na(Input_DF$Day), ]
Input_DF$time <- as.POSIXct(Input_DF$time, format = "%Y-%m-%d %H:%M:%S")
Input_DF$Hour <- as.POSIXlt(Input_DF$time)$hour # Extract hour
Input_DF$Hour[is.na(Input_DF$Hour)] <- 0 
#Input_DF$time <- as.POSIXct(Input_DF$time, format = "%Y-%m-%d %H:%M:%S")

### Converting all character columns to numeric 
# Sample_DF$Hour
char_cols <- sapply(Input_DF, is.character)
Input_DF[char_cols] <- lapply(Input_DF[char_cols], function(x) as.numeric(as.factor(x)))

unique(Sample_DF$in.misc_pool_pump)


# Input_DF$Day
# unique(Input_DF$Day)
# sum(is.na(Input_DF$Day))
# sum(is.na(Input_DF$Hour))
# unique(Input_DF$Hour)
# Input_DF$Hour

# dim(Input_DF[Input_DF$Hour == 0,])
# 182720 *2
# 365440 - 359730
# 
# 
# str(Input_DF)
# Input_DF <- na.omit(Input_DF)
str()
Input_DF$time <- Sample_DF$time
Input_DF$Hour <- Sample_DF$Hour
Input_DF$Day <- Sample_DF$Day
Input_DF$in.county <- Sample_DF$in.county
Input_DF$in.city <- Sample_DF$in.city
# str(Input_DF)

Input_DF <- subset(Input_DF, select = -c(X, bldg_id))
write.csv(Input_DF, file = "C:\\Users\\Rishikesh\\Downloads\\Semester 1\\INTRO_DS_RK\\PROJECT\\DATA\\Final_Data\\Final_Modeling_DF.csv", row.names = FALSE)

x

# Identify and display columns that are of numeric or integer data type
numeric_columns <- sapply(Sample_DF, is.numeric)
integer_columns <- sapply(Sample_DF, is.integer)

# Combine both numeric and integer columns
numeric_or_integer_columns <- numeric_columns | integer_columns

# Get the names of numeric or integer columns
numeric_or_integer_column_names <- names(Sample_DF)[numeric_or_integer_columns]

# Print the names of numeric or integer type columns
print(numeric_or_integer_column_names)

