# Workplace Setup
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(psych)
library(broom)
library(tidyverse)
library(kableExtra)
library(grid)
library(aod)

Fire_Incidents_Data_Clean <- read_csv("data/analysis_data/Cleaned_Fire_Incidents_Data.csv")
# model 1
# Run the logistic regression using stepwise eliminaton
Stepwise_logistic <- step(glm(Risk_type_binary ~ Building_Status_Cat + Business_Impact_Cat + Fire_Alarm_System_Operation_Cat + Method_Of_Fire_Control_Cat + Smoke_Alarm_at_Fire_Origin_Cat + Sprinkler_System_Presence_Cat+ arrival_to_fire_control_time + alarm_to_arrival_time + Number_of_responding_apparatus+ Number_of_responding_personnel, data = Fire_Incidents_Data_Clean, family = binomial), direction = "both", trace = FALSE)

summary(Stepwise_logistic)


confint(Stepwise_logistic)

wald.test(b= coef(Stepwise_logistic), Sigma = vcov(Stepwise_logistic), Terms= 4:6)

Fire_Incidents_Data_Clean$pred <- predict(Stepwise_logistic, type='response')


# Create a dataframe for plotting
plot_df <- data.frame(Building_Status_Cat = unique(Fire_Incidents_Data_Clean$Building_Status_Cat),
                      Predicted_Prob = predict(Stepwise_logistic, newdata = data.frame(Building_Status_Cat = unique(Fire_Incidents_Data_Clean$Building_Status_Cat)), type = "response"))



# Create a list of plots
p1 <- ggplot(Fire_Incidents_Data_Clean, aes(x = Building_Status_Cat, y = pred)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.7) + scale_x_discrete(guide= guide_axis(n.dodge=2)) +
  labs(x = "Building Status", y = "Predicted Probability of High Risk", title = "Predicted Probability of High Risk by Building Status Category") +
  theme(legend.title = element_text(size=5), legend.text= element_text(size=5), axis.text= element_text(size=5), axis.title = element_text(size=5), plot.title = element_text(size=7))

p2 <- ggplot(Fire_Incidents_Data_Clean, aes(x = Business_Impact_Cat, y = pred)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.7) + scale_x_discrete(guide= guide_axis(n.dodge=2)) +
  labs(x = "Business Impact", y = "Predicted Probability of High Risk", title = "Predicted Probability of High Risk by Business Impact Category") +
  theme(legend.title = element_text(size=5), legend.text= element_text(size=5), axis.text= element_text(size=5), axis.title = element_text(size=5), plot.title = element_text(size=7))

p3 <- ggplot(Fire_Incidents_Data_Clean, aes(x = Fire_Alarm_System_Operation_Cat, y = pred)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.7) + scale_x_discrete(guide= guide_axis(n.dodge=2)) +
  labs(x = "Fire Alarm System Operation", y = "Predicted Probability of High Risk", title = "Predicted Probability of High Risk by Fire Alarm System Operation Category") +
  theme(legend.title = element_text(size=5), legend.text= element_text(size=5), axis.text= element_text(size=5), axis.title = element_text(size=5), plot.title = element_text(size=7))

p4 <- ggplot(Fire_Incidents_Data_Clean, aes(x = Method_Of_Fire_Control_Cat, y = pred)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.7) + scale_x_discrete(guide= guide_axis(n.dodge=2)) +
  labs(x = "Method of Fire Control", y = "Predicted Probability of High Risk", title = "Predicted Probability of High Risk by Method of Fire Control Category") +
  theme(legend.title = element_text(size=5), legend.text= element_text(size=5), axis.text= element_text(size=5), axis.title = element_text(size=5), plot.title = element_text(size=7))

p5 <- ggplot(Fire_Incidents_Data_Clean, aes(x = Smoke_Alarm_at_Fire_Origin_Cat, y = pred)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.7) + scale_x_discrete(guide= guide_axis(n.dodge=2)) +
  labs(x = "Smoke Alarm at Fire Origin", y = "Predicted Probability of High Risk", title = "Predicted Probability of High Risk by Business Impact Category") +
  theme(legend.title = element_text(size=5), legend.text= element_text(size=5), axis.text= element_text(size=5), axis.title = element_text(size=5), plot.title = element_text(size=7))

p6 <- ggplot(Fire_Incidents_Data_Clean, aes(x = Sprinkler_System_Presence_Cat, y = pred)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.7) + scale_x_discrete(guide= guide_axis(n.dodge=2)) +
  labs(x = "Sprinkler System Presence", y = "Predicted Probability of High Risk", title = "Predicted Probability of High Risk by Fire Alarm System Operation Category") +
  theme(legend.title = element_text(size=5), legend.text= element_text(size=5), axis.text= element_text(size=5), axis.title = element_text(size=5), plot.title = element_text(size=7))

p7 <- ggplot(Fire_Incidents_Data_Clean, aes(x = arrival_to_fire_control_time, y = pred)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.7) + scale_x_discrete(guide= guide_axis(n.dodge=2)) +
  labs(x = "Arrival to fire control time", y = "Predicted Probability of High Risk", title = "Predicted Probability of High Risk by Method of Fire Control Category") +
  theme(legend.title = element_text(size=5), legend.text= element_text(size=5), axis.text= element_text(size=5), axis.title = element_text(size=5), plot.title = element_text(size=7))

p8 <- ggplot(Fire_Incidents_Data_Clean, aes(x = alarm_to_arrival_time, y = pred)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.7) + scale_x_discrete(guide= guide_axis(n.dodge=2)) +
  labs(x = "Alarm to arrival time", y = "Predicted Probability of High Risk", title = "Predicted Probability of High Risk by Business Impact Category") +
  theme(legend.title = element_text(size=5), legend.text= element_text(size=5), axis.text= element_text(size=5), axis.title = element_text(size=5), plot.title = element_text(size=7))



# Grid the plots together
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, ncol = 2)

saveRDS(Stepwise_logistic, file= "models/model1.rds")


# model 2
Fire_Incidents_Data_Clean <- read_csv("data/analysis_data/Cleaned_Fire_Incidents_Data.csv")
#before performing regression I replaced any infinity value with mean
meanlogEDL <- mean(Fire_Incidents_Data_Clean$logEDL[which(Fire_Incidents_Data_Clean$logEDL>0)])
Fire_Incidents_Data_Clean$logEDL <- ifelse(is.infinite(Fire_Incidents_Data_Clean$logEDL), meanlogEDL, Fire_Incidents_Data_Clean$logEDL)
# Run the logistic regression using stepwise eliminaton
Stepwise_linear1 <- step(lm(logEDL ~ Building_Status_Cat + Business_Impact_Cat + Fire_Alarm_System_Operation_Cat + Method_Of_Fire_Control_Cat + Smoke_Alarm_at_Fire_Origin_Cat + Sprinkler_System_Presence_Cat + arrival_to_fire_control_time + alarm_to_arrival_time + Number_of_responding_apparatus+ Number_of_responding_personnel,
                            data = Fire_Incidents_Data_Clean), direction = "both", trace = FALSE)

summary(Stepwise_linear1)

# plot residuals
plot(Stepwise_linear1$residuals)
abline(h = 0, col = "red")

# Check normal distribution
qqnorm(Stepwise_linear1$residuals)
qqline(Stepwise_linear1$residuals, col = "red")

# Calculate residuals
residuals_linear1 <- residuals(Stepwise_linear1)

# Create histogram with normal curve overlaid
hist(residuals_linear1, breaks = 30, freq = FALSE, main = "Histogram of Residuals", xlab = "Residuals", ylab = "Density")
curve(dnorm(x, mean = mean_residuals_linear1, sd = sd_residuals_linear1), add = TRUE, col = "red")

# Calculate mean and standard deviation of residuals
mean_residuals_linear1 <- mean(residuals_linear1)
sd_residuals_linear1 <- sd(residuals_linear1)
sd_residuals_linear1
mean_residuals_linear1
saveRDS(Stepwise_linear1, file= "models/model2.rds")


# model 3
Fire_Incidents_Data_Clean <- read_csv("data/analysis_data/Cleaned_Fire_Incidents_Data.csv")
# Run the logistic regression using stepwise eliminaton
Stepwise_linear2 <- step(lm(Civilian_Casualties ~ Building_Status_Cat + Business_Impact_Cat + Fire_Alarm_System_Operation_Cat + Method_Of_Fire_Control_Cat + Smoke_Alarm_at_Fire_Origin_Cat + Sprinkler_System_Presence_Cat+ arrival_to_fire_control_time + alarm_to_arrival_time + Number_of_responding_apparatus+ Number_of_responding_personnel,
                            data = Fire_Incidents_Data_Clean), direction = "both", trace = FALSE)

summary(Stepwise_linear2)

# plot residuals
plot(Stepwise_linear2$residuals)
abline(h = 0, col = "red")

# Check normal distribution
qqnorm(Stepwise_linear2$residuals)
qqline(Stepwise_linear2$residuals, col = "red")

# Calculate residuals
residuals_linear2 <- residuals(Stepwise_linear2)

# Create histogram with normal curve overlaid
hist(residuals_linear2, breaks = 30, freq = FALSE, main = "Histogram of Residuals", xlab = "Residuals", ylab = "Density")
mean_residuals_linear2 <- mean(residuals_linear2)
sd_residuals_linear2 <- sd(residuals_linear2)
curve(dnorm(x, mean = mean_residuals_linear2, sd = sd_residuals_linear2), add = TRUE, col = "red")

# Calculate mean and standard deviation of residuals
sd_residuals_linear2
mean_residuals_linear2
saveRDS(Stepwise_linear2, file= "models/model3.rds")
