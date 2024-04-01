# Import necessary libraries 
library(data.table)
library(ggplot2)
library(tidyverse)

# Read data set 
data <- fread('./project/volume/data/interim/HLaw.csv')

dataV <- data[data$InterventionYear >= 2001, ] 

# Train Years: 1999, 2000
# Test Years: 2001,2002,2003

#####################################
# Z-score Evaluation (z=(X-Mean)/SD)# 
#####################################
# Prediction knowing two data points up to intervention (X)
dataV[, c("Prediction2001", "Prediction2002", "Prediction2003") := .(NA_real_, NA_real_, NA_real_)]

predict_future_values <- function(row) {
  # Extract the years and the corresponding values for the linear model
  years <- c(1999, 2000)
  values <- as.numeric(row[, .(V1 = `1999`, V2 = `2000`)])  # Convert to numeric vector
  
  # Fit the linear model
  model <- lm(values ~ years)
  
  # Create a data frame for the years we want to predict
  new_years <- data.frame(years = c(2001, 2002, 2003))
  
  # Use the model to predict the values for the new years
  predictions <- predict(model, newdata = new_years)
  
  # Return the predictions as a list
  return(as.list(predictions))
}

# Apply the function row by row
dataV[, c("Prediction2001", "Prediction2002", "Prediction2003") :=
        predict_future_values(.SD), .SDcols = c('1999', '2000'), by = .I]

calculate_pre_intervention_mean <- function(row) {
  years_before_intervention <- seq(from = 1999, to = row['InterventionYear'])
  values_before_intervention <- row[as.character(years_before_intervention)]
  mean_before_intervention <- mean(as.numeric(values_before_intervention), na.rm = TRUE)
  return(mean_before_intervention)
}

dataV$MeanPre <- apply(dataV, 1, calculate_pre_intervention_mean)

calculate_pre_intervention_sd <- function(row) {
  years_before_intervention <- seq(from = 1999, to = row['InterventionYear'])
  values_before_intervention <- row[as.character(years_before_intervention)]
  sd_before_intervention <- sd(as.numeric(values_before_intervention), na.rm = TRUE)
  return(sd_before_intervention)
}

dataV$SDPre <- apply(dataV, 1, calculate_pre_intervention_sd)

##############
# Prediction # 
##############

# Calculate Z-score 1 point 
dataV[, `Zscore` := (`Prediction2001` - `MeanPre`)/ `SDPre`]
zscore <- dataV$Zscore
summary(zscore)

# Calculate Z-score 2 point 
dataV[, `Zscore2` := (`Prediction2002` - `MeanPre`)/ `SDPre`]
zscore2 <- dataV$Zscore2
summary(zscore2)

# Calculate Z-score 3 point 
dataV[, `Zscore3` := (`Prediction2003` - `MeanPre`)/ `SDPre`]
zscore3 <- dataV$Zscore3
summary(zscore3)

##########################
# Creating Z-score Table # 
##########################

ztable <- dataV[, c("State", "Law", "Zscore", "Zscore2", "Zscore3")]

#################################################
# Scores With Confidence Interval (-0.25, 0.25) # 
#################################################
ggplot(ztable, aes(x = State, y = `Zscore`)) + 
  geom_point(position = position_jitter(width = 0.2, height = 0), size = 1) +
  geom_hline(yintercept = c(-0.25, 0.25), color = "red", linetype = "dashed") +
  labs(title = "Z-score (Implemented)", x = "State", y = "Z-score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(ztable, aes(x = State, y = `Zscore2`)) + 
  geom_point(position = position_jitter(width = 0.2, height = 0), size = 1) +
  geom_hline(yintercept = c(-0.25, 0.25), color = "red", linetype = "dashed") +
  labs(title = "Z-score (Implemented)", x = "State", y = "Z-score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(ztable, aes(x = State, y = `Zscore3`)) + 
  geom_point(position = position_jitter(width = 0.2, height = 0), size = 1) +
  geom_hline(yintercept = c(-0.25, 0.25), color = "red", linetype = "dashed") +
  labs(title = "Z-score (Implemented)", x = "State", y = "Z-score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Accept and build CFs that fall within this region
dataC1 <- dataV[Zscore >= -0.25 & Zscore <= 0.25]
dataC1 <- dataC1[, c("State", "Law", "Zscore", "Zscore2", "Zscore3")]
dataC2 <- dataV[Zscore2 >= -0.25 & Zscore2 <= 0.25]
dataC2 <- dataC2[, c("State", "Law", "Zscore", "Zscore2", "Zscore3")]
dataC3 <- dataV[Zscore3 >= -0.25 & Zscore3 <= 0.25]
dataC3 <- dataC3[, c("State", "Law", "Zscore", "Zscore2", "Zscore3")]

# Write out confident predictions
fwrite(dataC,'./project/volume/data/interim/ConfidentAvg.csv')
