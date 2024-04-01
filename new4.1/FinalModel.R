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
dataV[, `Predictions` := (`1999` + `2000`)/2]
pred <- dataV$`Predictions` 

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

# Calculate Z-score
dataV[, `Zscore` := (`Predictions` - `MeanPre`)/ `SDPre`]
zscore <- dataV$Zscore
summary(zscore)

##########################
# Creating Z-score Table # 
##########################
ztable <- dataV[, c("State", "Law", "Zscore")]

#################################################
# Scores With Confidence Interval (-0.25, 0.25) # 
#################################################
ggplot(ztable, aes(x = State, y = `Zscore`)) + 
   geom_point(position = position_jitter(width = 0.2, height = 0), size = 1) +
   geom_hline(yintercept = c(-0.25, 0.25), color = "red", linetype = "dashed") +
   labs(title = "Z-score (Implemented)", x = "State", y = "Z-score") +
   theme_minimal() +
   theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Accept and build CFs that fall within this region
dataC <- dataV[Zscore >= -0.25 & Zscore <= 0.25]

# Write out confident predictions
fwrite(dataC,'./project/volume/data/interim/ConfidentAvg.csv')
