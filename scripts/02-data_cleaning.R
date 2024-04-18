#### Preamble ####
# Purpose: Cleans the raw plane data recorded
# Date: 10 April 2024 
# Contact: hechen.zhang@mail.utoronto.ca
# Workplace Setup
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(psych)
library(broom)
library(tidyverse)
library(patchwork)
library(arrow)
library(kableExtra)
library(opendatatoronto)
library(grid)
library(here)


# read data
Data <- read_csv("data/raw_data/raw_fire_incident_data.csv")

# Select only the desired variables
desired_variables <- c("Civilian_Casualties", "Estimated_Dollar_Loss", "Building_Status", "Business_Impact", "Fire_Alarm_System_Operation", "Method_Of_Fire_Control", "Smoke_Alarm_at_Fire_Origin", "Sprinkler_System_Presence", "TFS_Arrival_Time", "TFS_Alarm_Time", "Fire_Under_Control_Time", "Number_of_responding_apparatus", "Number_of_responding_personnel")

Fire_Incidents_Data <- Data %>%
  select(all_of(desired_variables))

# Check the data types of all variables
glimpse(Fire_Incidents_Data)

table_data <- data.frame(
  "Variable Name" = c(
    "Civilian Casualties",
    "Estimated Dollar Loss",
    "Building Status",
    "Business Impact",
    "Fire Alarm System Operation",
    "Method Of Fire Control",
    "Smoke Alarm at Fire Origin",
    "Sprinkler System Presence",
    "TFS Alarm Time",
    "TFS Arrival Time",
    "Fire Under Control Time",
    "Number of responding apparatus",
    "Number of responding personnel"
  ),
  "Description" = c(
    "Civilian casualties observed at the scene",
    "Estimated dollar loss",
    "OFM Building status code and description",
    "OFM Business Impact code and description",
    "OFM Fire Alarm System Operation code and description",
    "OFM Method Of Fire Control code and description",
    "OFM Smoke Alarm at Fire Origin code and description",
    "OFM Sprinkler System Presence code and description",
    "Timestamp of when TFS was notified of the incident",
    "Timestamp of first arriving unit to incident",
    "Timestamp of fire under control",
    "Number of TFS responding apparatus",
    "Number of TFS responding personnel"
  )
)

# Create the table data
table_data |> kable( digits = 2,
                     booktabs = TRUE, linesep = "", escape = FALSE,
                     col.names = c("Variable Name", "Description")) |> kable_styling()

# Check for missing values
colSums(is.na(Fire_Incidents_Data))

# Examine which variable has how many missing values
missing_values <- colSums(is.na(Fire_Incidents_Data))
data.frame(Variable = names(missing_values), Missing_Values = as.integer(missing_values))

# Remove all missing and NA values from the data
Fire_Incidents_Data_Clean <- Fire_Incidents_Data %>%
  drop_na()

# Check the number of rows in the cleaned data
nrow(Fire_Incidents_Data_Clean)


write_csv(Fire_Incidents_Data_Clean, "data/analysis_data/cleaned_fire_incident_data.csv")
write_parquet(Fire_Incidents_Data_Clean, "data/analysis_data/cleaned_fire_incident_data.parquet")
