#### Preamble ####
# Purpose: Test the simulated dataset.
# Date: 16 April 2024
# Contact: hechen.zhang@mail.utoronto.ca

# Workplace setup
# install.packages("testthat")
library(testthat)

# Load the simulated dataset
simulated_data <- read.csv("data/raw_data/simulated_data.csv")

# Test 1: Check correct number of rows
if (nrow(simulated_data) != 10000) {
  stop("Test 1 Failed: Incorrect number of rows.")
}

# Test 2: Check correct number of columns
if (ncol(simulated_data) != 13) {
  stop("Test 2 Failed: Incorrect number of columns.")
}

# Test 3: Check for all required columns
expected_cols <- c("Civilian_Casualties", "Estimated_Dollar_Loss", "Building_Status", 
                   "Business_Impact", "Fire_Alarm_System_Operation", "Method_Of_Fire_Control",
                   "Smoke_Alarm_at_Fire_Origin", "Sprinkler_System_Presence", "TFS_Alarm_Time",
                   "TFS_Arrival_Time", "Fire_Under_Control_Time", "Number_of_responding_apparatus",
                   "Number_of_responding_personnel")
if (!all(expected_cols %in% names(simulated_data))) {
  stop("Test 3 Failed: One or more required columns are missing.")
}

# Test 4: Ensure no negative values for Civilian Casualties
if (any(simulated_data$Civilian_Casualties < 0)) {
  stop("Test 4 Failed: Negative values found in Civilian Casualties.")
}

# Test 5: Check if Estimated Dollar Loss is within the expected range
if (any(simulated_data$Estimated_Dollar_Loss < 500 | simulated_data$Estimated_Dollar_Loss > 100000)) {
  stop("Test 5 Failed: Estimated Dollar Loss out of expected range.")
}

# Test 6: Check for non-negative values in Number of Responding Apparatus and Personnel
if (any(simulated_data$Number_of_responding_apparatus < 0) || any(simulated_data$Number_of_responding_personnel < 0)) {
  stop("Test 6 Failed: Negative values found in Number of Responding Apparatus or Personnel.")
}

# Test 7: Ensure data types are correct (Numeric for Civilian Casualties and Dollar Loss)
if (!is.numeric(simulated_data$Civilian_Casualties) || !is.numeric(simulated_data$Estimated_Dollar_Loss)) {
  stop("Test 7 Failed: Incorrect data type for Civilian Casualties or Estimated Dollar Loss.")
}

# Test 8: Check for valid categories in Building Status
valid_building_statuses <- c("Occupied", "Unoccupied", "Abandoned", NA)
if (any(!simulated_data$Building_Status %in% valid_building_statuses)) {
  stop("Test 8 Failed: Invalid categories found in Building Status.")
}

