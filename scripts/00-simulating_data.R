#### Preamble ####
# Purpose: Simulate the fire incidents data
# Date: 16 April 2024
# Contact: hechen.zhang@mail.utoronto.ca

# Load necessary library
library(dplyr)

# Set the number of rows to simulate
n <- 10000

# Simulate data
set.seed(302)  # For reproducibility
simulated_data <- data_frame(
  Civilian_Casualties = sample(0:10, n, replace = TRUE, prob = rep(1/11, 11)),  # Uniformly distributed for simplicity
  Estimated_Dollar_Loss = round(runif(n, min = 500, max = 100000), -2),
  Building_Status = sample(c("Occupied", "Unoccupied", "Abandoned", NA), n, replace = TRUE, prob = c(0.7, 0.1, 0.1, 0.1)),
  Business_Impact = sample(c("None", "Minor", "Moderate", "Severe", NA), n, replace = TRUE, prob = c(0.5, 0.2, 0.2, 0.05, 0.05)),
  Fire_Alarm_System_Operation = sample(c("Operational", "Non-operational", "Not present", NA), n, replace = TRUE, prob = c(0.7, 0.1, 0.1, 0.1)),
  Method_Of_Fire_Control = sample(c("Water", "Foam", "Dry chemical", "CO2", "No intervention", NA), n, replace = TRUE),
  Smoke_Alarm_at_Fire_Origin = sample(c("Present", "Absent", "Unknown", NA), n, replace = TRUE, prob = c(0.4, 0.4, 0.1, 0.1)),
  Sprinkler_System_Presence = sample(c("Present", "Absent", "Not applicable", NA), n, replace = TRUE, prob = c(0.3, 0.3, 0.3, 0.1)),
  TFS_Alarm_Time = as.POSIXct("2021-01-01 00:00:00") + runif(n, 0, 60*60*24*365),
  TFS_Arrival_Time = as.POSIXct("2021-01-01 00:00:00") + runif(n, 0, 60*60*24*365),
  Fire_Under_Control_Time = as.POSIXct("2021-01-01 00:00:00") + runif(n, 0, 60*60*24*365),
  Number_of_responding_apparatus = sample(1:10, n, replace = TRUE),
  Number_of_responding_personnel = sample(1:50, n, replace = TRUE)
)

# Record this simulation
write_csv(simulated_data, "data/raw_data/simulated_data.csv") 
