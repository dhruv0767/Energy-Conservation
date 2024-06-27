

####### Import the necessary libraries ########

#install.packages("corrplot")
#install.packages("randomForest")
#install.packages("glmnet")

library(glmnet)
library(arrow)
library(tidyverse)
library(dplyr)
library(corrplot)
library(stats)
library(randomForest)
library(e1071)
library(caret)

#Modeling_DF <- read.csv("C:\\Users\\Rishikesh\\Downloads\\Semester 1\\INTRO_DS_RK\\PROJECT\\DATA\\Final_Data\\Final_Modeling_DF.csv")
Modeling_DF <- read.csv("C:\\Users\\Rishikesh\\Downloads\\Semester 1\\INTRO_DS_RK\\PROJECT\\DATA\\Final_Data\\Sample_Modeling_DF.csv")
Final_Modeling_DF <- Modeling_DF
Final_Modeling_DF <- na.omit(Final_Modeling_DF)

#colnames(Final_Modeling_DF)



columns_to_drop <- c("out.kitchen.energy_consumption","out.laundry.energy_consumption", 
                     "out.heating_cooling.energy_consumption","out.water_heating.energy_consumption", 
                     "out.electrical_appliances.energy_consumption", "out.outdoor_appliances.energy_consumption",
                     "out.renewable_energy.energy_consumption")

# Get the indices of the columns you want to drop
indices_to_drop <- which(names(Final_Modeling_DF) %in% columns_to_drop)

# Drop the columns by negative indexing
Final_Modeling_DF <- Final_Modeling_DF[, -c(indices_to_drop)]

Final_Modeling_DF$.
colnames(Final_Modeling_DF)

columns_to_keep <- c("Dry.Bulb.Temperature...C.","Relative.Humidity...","Wind.Speed..m.s",
                     "Wind.Direction..Deg.","Global.Horizontal.Radiation..W.m2.","Diffuse.Horizontal.Radiation..W.m2.",
                     "in.sqft","in.bedrooms","in.cooling_setpoint","in.cooling_setpoint_offset_magnitude",
                     "in.county","in.heating_setpoint","in.heating_setpoint","in.heating_setpoint_offset_magnitude",
                     "in.hot_water_fixtures","in.misc_extra_refrigerator","in.occupants","in.reeds_balancing_area",
                     "in.refrigerator","in.usage_level","in.water_heater","out.total.energy_consumption")

indices_to_keep <- which(names(Final_Modeling_DF) %in% columns_to_keep)

Final_Modeling_DF <- Final_Modeling_DF[, c(indices_to_keep)]


################## Modeling #########################
set.seed(123)  # for reproducibility
# Split data into predictors (X) and target (y)
X <- Final_Modeling_DF[, -which(names(Final_Modeling_DF) == "out.total.energy_consumption")]
y <- Final_Modeling_DF$out.total.energy_consumption

# Split data into training (70%) and test (30%) sets
trainIndex <- createDataPartition(y, p = 0.7, list = FALSE)
X_train <- X[trainIndex, ]
# y_train <- y[trainIndex]
X_test <- X[-trainIndex, ]
y_test <- y[-trainIndex]

trainIndex <- createDataPartition(Final_Modeling_DF$out.total.energy_consumption , p = 0.8, list = FALSE)
train_data <- Final_Modeling_DF[trainIndex, ]
test_data <- Final_Modeling_DF[-trainIndex, ]

# Fit linear regression model
model <- lm(out.total.energy_consumption ~ ., data = train_data)

# Get summary of the model
summary(model)

# Predict on test data
predictions <- predict(model, newdata = data.frame(test_data))
as.numeric(predictions)
unique(predictions)


# Since linear regression is used for continuous outcomes, we do not have classes and a confusion matrix.
# Instead, we can calculate mean squared error or other continuous outcomes performance metrics.
# For example, mean squared error:
mse <- mean((predictions - test_data$out.total.energy_consumption) ^ 2)
print(paste("Mean Squared Error on Test Set:", mse))

################################################################


# install.packages("sf")
# install.packages("tigris")
library(tigris)
library(sf)
# Load county shapefile data (for the US)
counties <- tigris::counties(cb = TRUE, class = "sf")
head(Final_Modeling_DF$in.county)
# Assuming spc_tbl_ is your data frame and it has a column 'county_number'
# Merge your data with the county shapefile
# Replace 'GEOID' with the appropriate field if different
view(counties)
df<- Final_Modeling_DF
merged_data <- merge(counties, df, by.x = "GEOID", by.y = "in.county")


colnames(Final_Modeling_DF)

my_plot <- ggplot(Final_Modeling_DF, aes(x = Hour, y = EnergyConsumption, color = Appliance)) +
  geom_line() +
  labs(title = "Hourly Energy Consumption by Appliance",
       x = "Hour of the Day",
       y = "Energy Consumption (in kWh)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))