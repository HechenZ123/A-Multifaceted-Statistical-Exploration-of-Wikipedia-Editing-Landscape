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


# Create a new variable Risk_Type based on the criteria
Fire_Incidents_Data_Clean <- Fire_Incidents_Data_Clean %>%
  mutate(
    Risk_type = case_when(
      Civilian_Casualties == 0  & Estimated_Dollar_Loss < 5000 ~ "Very Low",
      Civilian_Casualties == 0 & Estimated_Dollar_Loss >= 5000 & Estimated_Dollar_Loss < 10000 ~ "Low",
      (Civilian_Casualties == 0 & Estimated_Dollar_Loss >= 10000 & Estimated_Dollar_Loss < 50000) |
        (Civilian_Casualties == 0 & Estimated_Dollar_Loss < 50000) ~ "Moderate",
      (Civilian_Casualties == 0 & Estimated_Dollar_Loss >= 50000 & Estimated_Dollar_Loss < 100000) |
        (Civilian_Casualties > 0 & Estimated_Dollar_Loss < 100000) ~ "High",
      Civilian_Casualties > 0 | Estimated_Dollar_Loss >= 100000 ~ "Very High"
    )
  ) %>%
  drop_na()

# Check the number of rows in the cleaned data
nrow(Fire_Incidents_Data_Clean)

Fire_Incidents_Data_Clean$Building_Status_Cat <- factor(Fire_Incidents_Data_Clean$Building_Status, levels = c("01 - Normal (no change)", "02 - Under Renovation", "03 - Under Construction", "05 - Abandoned, vacant (long term)", "08 - Not Applicable", "09 - Undetermined"), labels = c("Normal", "Under Renovation", "Under Construction", "Abandoned", "Not Applicable", "Undetermined"))
levels(Fire_Incidents_Data_Clean$Building_Status_Cat) <- list(
  "Normal" = c("Normal"),
  "Under Renovation/Construction" = c( "Under Renovation", "Under Construction"),
  "Abandon" = c("Abandoned"),
  "Undetermined/Not Applicable" = c("Not Applicable", "Undetermined"))
# Create a new variable that combines the levels
Fire_Incidents_Data_Clean$Business_Impact_Cat <- factor(Fire_Incidents_Data_Clean$Business_Impact, levels = c("1 - No business interruption", "2 - May resume operations within a week", "3 - May resume operations within a month", "4 - May resume operations within a year", "5 - May not resume operations", "8 - Not applicable (not a business)", "9 - Undetermined"), labels = c("No Interruption", "Resume in a week", "Resume in a month", "Resume in a year", "May not resume", "Not applicable", "Undetermined"))

# Combine the levels as you described
levels(Fire_Incidents_Data_Clean$Business_Impact_Cat) <- list(
  "No Interruption" = c("No Interruption"),
  "May Resume" = c("Resume in a week", "Resume in a month", "Resume in a year"),
  "May not resume" = c("May not resume"),
  "Undetermined/Not Applicable" = c("Not applicable", "Undetermined"))
Fire_Incidents_Data_Clean$Fire_Alarm_System_Operation_Cat <- factor(Fire_Incidents_Data_Clean$Fire_Alarm_System_Operation, levels = c("1 - Fire alarm system operated", "2 - Fire alarm system did not operate", "8 - Not applicable (no system)", "9 - Fire alarm system operation undetermined"), labels = c("Operated", "Did not operate", "Not Applicable", "Undetermined"))
levels(Fire_Incidents_Data_Clean$Fire_Alarm_System_Operation_Cat) <- list(
  "Operated" = c("Operated"),
  "Did not operate" = c("Did not operate"),
  "Undetermined/Not Applicable" = c("Not Applicable", "Undetermined"))

Fire_Incidents_Data_Clean$Method_Of_Fire_Control_Cat <- factor(Fire_Incidents_Data_Clean$Method_Of_Fire_Control, levels = c("1 - Extinguished by fire department", "2 - Extinguished by automatic system", "3 - Extinguished by occupant", "4 - Fire self extinguished", "5 - Action taken unclassified"), labels = c("Fire department", "Automatic system", "Occupant", "Self extinguished", "Undetermined"))
Fire_Incidents_Data_Clean$Smoke_Alarm_at_Fire_Origin_Cat <- factor(Fire_Incidents_Data_Clean$Smoke_Alarm_at_Fire_Origin, levels = c("1 - Floor/suite of fire origin: No smoke alarm", "1 - No smoke alarm", "2 - Floor/suite of fire origin: Smoke alarm present and operated", "2 - Smoke alarm present and operated","3 - Floor/suite of fire origin: Smoke alarm present did not operate", "3 - Smoke alarm present did not operate", "4 - Floor/suite of fire origin: Smoke alarm present, operation undetermined","4 - Smoke alarm present, operation undetermined", "9 - Floor/suite of fire origin: Smoke alarm presence undetermined", "9 - Smoke alarm presence undetermined"), labels = c("No smoke alarm", "No smoke alarm1", "Presented and operated" , "Presented and operated1", "Did not operate", "Did not operate1", "Undetermined", "Undetermined1", "Undetermined2", "Undetermined3"))


levels(Fire_Incidents_Data_Clean$Smoke_Alarm_at_Fire_Origin_Cat) <- list(
  "No smoke alarm" = c("No smoke alarm", "No smoke alarm1"),
  "Present and operated" = c("Presented and operated" , "Presented and operated1"),
  "Did not operate" = c("Did not operate", "Did not operate1"),
  "Undetermined" = c("Undetermined", "Undetermined1", "Undetermined2", "Undetermined3"))

Fire_Incidents_Data_Clean$Sprinkler_System_Presence_Cat <- factor(Fire_Incidents_Data_Clean$Sprinkler_System_Presence, levels = c("1 - Full sprinkler system present", "2 - Partial sprinkler system present", "3 - No sprinkler system", "9 - Undetermined"), labels = c("Full sprinkler system", "Partial sprinkler system", "No sprinkler system", "Undetermined"))
# Check the new categorical variable
table(Fire_Incidents_Data_Clean$Building_Status_Cat)
table(Fire_Incidents_Data_Clean$Business_Impact_Cat)
table(Fire_Incidents_Data_Clean$Fire_Alarm_System_Operation_Cat)
table(Fire_Incidents_Data_Clean$Method_Of_Fire_Control_Cat)
table(Fire_Incidents_Data_Clean$Smoke_Alarm_at_Fire_Origin_Cat)
table(Fire_Incidents_Data_Clean$Sprinkler_System_Presence_Cat)


# Calculate the difference between TFS_Arrival_Time1 and TFS_Alarm_Time1
Fire_Incidents_Data_Clean$TFS_Alarm_Time1 <- as.POSIXct(Fire_Incidents_Data_Clean$TFS_Alarm_Time, format = "%Y-%m-%dT%H:%M:%S")
Fire_Incidents_Data_Clean$TFS_Arrival_Time1 <- as.POSIXct(Fire_Incidents_Data_Clean$TFS_Arrival_Time, format = "%Y-%m-%dT%H:%M:%S")

Fire_Incidents_Data_Clean$alarm_to_arrival_time <- difftime(Fire_Incidents_Data_Clean$TFS_Arrival_Time1, Fire_Incidents_Data_Clean$TFS_Alarm_Time1, units = "mins")

Fire_Incidents_Data_Clean$alarm_to_arrival_time <- as.numeric(Fire_Incidents_Data_Clean$alarm_to_arrival_time)

# Calculate the difference between TFS_Arrival_Time1 and Fire_Under_Control_Time1
Fire_Incidents_Data_Clean$Fire_Under_Control_Time1 <- as.POSIXct(Fire_Incidents_Data_Clean$Fire_Under_Control_Time, format = "%Y-%m-%dT%H:%M:%S")

Fire_Incidents_Data_Clean$arrival_to_fire_control_time_hours <- difftime(Fire_Incidents_Data_Clean$Fire_Under_Control_Time1, Fire_Incidents_Data_Clean$TFS_Arrival_Time1, units = "hours")

Fire_Incidents_Data_Clean$arrival_to_fire_control_time <-  as.numeric(Fire_Incidents_Data_Clean$arrival_to_fire_control_time_hours)

Fire_Incidents_Data_Clean <- Fire_Incidents_Data_Clean %>%
  filter(alarm_to_arrival_time <= 200)

Fire_Incidents_Data_Clean <- na.omit(Fire_Incidents_Data_Clean)
# Check the number of rows in the cleaned data
nrow(Fire_Incidents_Data_Clean)

# Recode Risk_type into Risk_type_cat to use in logistic regression
Fire_Incidents_Data_Clean$Risk_type_cat <- recode(Fire_Incidents_Data_Clean$Risk_type,
                                                  "Low" = "Low to Moderate Risk",
                                                  "Moderate" = "Low to Moderate Risk",
                                                  "Very Low" = "Low to Moderate Risk",
                                                  "High" = "High Risk",
                                                  "Very High" = "High Risk")

# Recode Risk_type_cat as a binary variable
Fire_Incidents_Data_Clean$Risk_type_binary <- ifelse(Fire_Incidents_Data_Clean$Risk_type_cat == "High Risk", 1, 0)
Fire_Incidents_Data_Clean$logEDL <- log(Fire_Incidents_Data_Clean$Estimated_Dollar_Loss)
write_csv(Fire_Incidents_Data_Clean, "data/analysis_data/Cleaned_Fire_Incidents_Data.csv")
write_parquet(Fire_Incidents_Data_Clean, "data/analysis_data/Cleaned_Fire_Incidents_Data.parquet")
