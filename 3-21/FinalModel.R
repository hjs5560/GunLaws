# Import necessary libraries 
library(data.table)
library(ggplot2)
library(tidyverse)

# Read data set 
data <- fread('./project/volume/data/interim/dataHom.csv')

# Valid data years of focus 
dataV <- data[, c("State", "Year", "LawName", "Switching", "Homicides_-1", 
                   "Homicides_0", "Homicides_1", "Homicides_2", "Homicides_3")]

# Train Years: 1999, 2000
# Test Years: 2001,2002,2003

#####################################
# Z-score Evaluation (z=(X-Mean)/SD)# 
#####################################
# Prediction knowing two data points up to intervention (X)
dataV[, `Predictions` := (`Homicides_-1` + `Homicides_0`)/2]
pred <- dataV$`Predictions` 

############################
# Predicting One Point Out # 
############################
# Ensure cases with +1 actual data 
dataV1 <- dataV[!is.na(dataV$Homicides_1), ] 

# Mean of all actual points (Mean)
dataV1[, `Mean1` := (`Homicides_-1` + `Homicides_0` + `Homicides_1`)/3] 

# Standard Deviation of all actual points (SD)
dataV1[, `Sd1` := sqrt(((`Homicides_-1` - `Mean1`)**2 + (`Homicides_0` - `Mean1`)**2 + 
                         (`Homicides_1` - `Mean1`)**2)/3)]

# Calculate Z-score
dataV1[, `Zscore` := (`Predictions` - `Mean1`)/ `Sd1`]
zscore <- dataV1$Zscore
summary(zscore)

# Graph for a specific case
sub_case <- dataV1[State == 'AK' & LawName == 'ccbackground']
sub_case_long <- pivot_longer(sub_case, cols = `Homicides_-1`:`Homicides_1`, 
                              names_to = "IYear", values_to = "ActualValue")
sd1 <- sub_case_long$Sd1
mean_all1 <- sub_case_long$Mean1
pred1 <- sub_case_long$Predictions
ggplot(sub_case_long, aes(x = IYear, y = ActualValue)) + 
  geom_point(size = 3, color = "blue") +  # Actual points
  geom_line(aes(group = 1), color = "blue") +  # Connects the actual points
  geom_hline(yintercept = mean_all1, color = "red", linetype = "solid", size = 1) +  
  geom_hline(yintercept = pred1, color = "black", linetype = "dashed", size = 1) +  
  geom_hline(yintercept = mean_all1 + sd1, color = "green", linetype = "dotted", size = 1) +  
  geom_hline(yintercept = mean_all1 - sd1, color = "green", linetype = "dotted", size = 1) +  
  labs(title = "Actual Values Over Time for AK, nosyg Law, with Mean and SD",
       x = "Year",
       y = "Actual Value") +
  theme_minimal()

#############################
# Predicting Two Points Out # 
#############################
# Ensure cases with +2 actual data 
dataV2 <- dataV[!is.na(dataV$Homicides_2), ]

# Mean of all actual points (Mean)
dataV2[, `Mean2` := (`Homicides_-1` + `Homicides_0` + `Homicides_1`+`Homicides_2`)/4] 

# Standard Deviation of all actual points (SD)
dataV2[, `Sd2` := sqrt(((`Homicides_-1` - `Mean2`)**2 + (`Homicides_0` - `Mean2`)**2 + 
                         (`Homicides_1` - `Mean2`)**2 + (`Homicides_2` - `Mean2`)**2)/4)]

# Calculate Z-score
dataV2[, `Zscore2` := (`Predictions` - `Mean2`)/ `Sd2`]
zscore2 <- dataV2$Zscore2
summary(zscore2)

# Graph for a specific case
sub_case <- dataV2[State == 'AK' & LawName == 'ccbackground']
sub_case_long <- pivot_longer(sub_case, cols = `Homicides_-1`:`Homicides_2`, 
                              names_to = "IYear", values_to = "ActualValue")
sd2 <- sub_case_long$Sd2
mean_all2 <- sub_case_long$Mean2
pred2 <- sub_case_long$Predictions
ggplot(sub_case_long, aes(x = IYear, y = ActualValue)) + 
  geom_point(size = 3, color = "blue") +  # Actual points
  geom_line(aes(group = 1), color = "blue") +  # Connects the actual points
  geom_hline(yintercept = mean_all2, color = "red", linetype = "solid", size = 1) +  
  geom_hline(yintercept = pred1, color = "black", linetype = "dashed", size = 1) +  
  geom_hline(yintercept = mean_all2 + sd2, color = "green", linetype = "dotted", size = 1) +  
  geom_hline(yintercept = mean_all2 - sd2, color = "green", linetype = "dotted", size = 1) +  
  labs(title = "Actual Values Over Time for AK, nosyg Law, with Mean and SD",
       x = "Year",
       y = "Actual Value") +
  theme_minimal()

###############################
# Predicting Three Points Out # 
###############################
# Ensure cases with +3 actual data 
dataV3 <- dataV[!is.na(dataV$Homicides_3), ]

# Mean of all actual points (Mean)
dataV3[, `Mean3` := (`Homicides_-1` + `Homicides_0` + `Homicides_1`+`Homicides_2`+`Homicides_3`)/5] 

# Standard Deviation of all actual points (SD)
dataV3[, `Sd3` := sqrt(((`Homicides_-1` - `Mean3`)**2 + (`Homicides_0` - `Mean3`)**2 + 
                         (`Homicides_1` - `Mean3`)**2 + (`Homicides_2` - `Mean3`)**2 +
                         (`Homicides_3` - `Mean3`)**2)/5)]

# Calculate Z-score
dataV3[, `Zscore3` := (`Predictions` - `Mean3`)/ `Sd3`]
zscore3 <- dataV3$Zscore3
summary(zscore3)

# Graph for a specific case
sub_case <- dataV3[State == 'AK' & LawName == 'ccbackground']
sub_case_long <- pivot_longer(sub_case, cols = `Homicides_-1`:`Homicides_3`, 
                              names_to = "IYear", values_to = "ActualValue")
sd3 <- sub_case_long$Sd3
mean_all3 <- sub_case_long$Mean3
pred13 <- sub_case_long$Predictions
ggplot(sub_case_long, aes(x = IYear, y = ActualValue)) + 
  geom_point(size = 3, color = "blue") +  # Actual points
  geom_line(aes(group = 1), color = "blue") +  # Connects the actual points
  geom_hline(yintercept = mean_all3, color = "red", linetype = "solid", size = 1) +  
  geom_hline(yintercept = pred1, color = "black", linetype = "dashed", size = 1) +  
  geom_hline(yintercept = mean_all3 + sd3, color = "green", linetype = "dotted", size = 1) +  
  geom_hline(yintercept = mean_all3 - sd3, color = "green", linetype = "dotted", size = 1) +  
  labs(title = "Actual Values Over Time for AK, nosyg Law, with Mean and SD",
       x = "Year",
       y = "Actual Value") +
  theme_minimal()

##########################
# Creating Z-score Table # 
##########################
merge <- dataV[, c("State", "LawName", "Switching")]

# Add 1 point out scores 
merge$`1PointOut` <- NA
match <- match(paste(dataV$State, dataV$LawName), paste(dataV1$State, dataV1$LawName))
merge$`1PointOut`[!is.na(match)] <- dataV1$Zscore[na.omit(match)]

# Add 2 point out scores 
merge$`2PointOut` <- NA
match2 <- match(paste(dataV$State, dataV$LawName), paste(dataV2$State, dataV2$LawName))
merge$`2PointOut`[!is.na(match2)] <- dataV2$Zscore2[na.omit(match2)]

# Add 3 point out scores 
merge$`3PointOut` <- NA
match3 <- match(paste(dataV$State, dataV$LawName), paste(dataV3$State, dataV3$LawName))
merge$`3PointOut`[!is.na(match3)] <- dataV3$Zscore3[na.omit(match3)]

# Separate Interventions
# mergeI <- merge[Switching == "implemented"]
# mergeR <- merge[Switching == "removal"]

# Formatting
# ztableI <- mergeI[, c("State", "LawName", "1PointOut", "2PointOut", "3PointOut")]
# ztableR <- mergeR[, c("State", "LawName", "1PointOut", "2PointOut", "3PointOut")]

ztable <- merge[, c("State", "LawName", "1PointOut", "2PointOut", "3PointOut")]

#################################################
# Scores With Confidence Interval (-0.25, 0.25) # 
#################################################
# Removals 
# ggplot(ztableR, aes(x = State, y = `1PointOut`)) + 
#   geom_point(position = position_jitter(width = 0.2, height = 0), size = 1) +
#   geom_hline(yintercept = c(-0.25, 0.25), color = "red", linetype = "dashed") +
#   labs(title = "Z-score (Removal)", x = "State", y = "Z-score") +
#   theme_minimal() +
#  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# ggplot(ztableR, aes(x = State, y = `2PointOut`)) + 
#   geom_point(position = position_jitter(width = 0.2, height = 0), size = 1) +
#   geom_hline(yintercept = c(-0.25, 0.25), color = "red", linetype = "dashed") +
#   labs(title = "Z-score (Removal)", x = "State", y = "Z-score") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# ggplot(ztableR, aes(x = State, y = `3PointOut`)) + 
#   geom_point(position = position_jitter(width = 0.2, height = 0), size = 1) +
#   geom_hline(yintercept = c(-0.25, 0.25), color = "red", linetype = "dashed") +
#   labs(title = "Z-score (Removal)", x = "State", y = "Z-score") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Implementations
# ggplot(ztableI, aes(x = State, y = `1PointOut`)) + 
#   geom_point(position = position_jitter(width = 0.2, height = 0), size = 1) +
#   geom_hline(yintercept = c(-0.25, 0.25), color = "red", linetype = "dashed") +
#   labs(title = "Z-score (Implemented)", x = "State", y = "Z-score") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# ggplot(ztableI, aes(x = State, y = `2PointOut`)) + 
#   geom_point(position = position_jitter(width = 0.2, height = 0), size = 1) +
#   geom_hline(yintercept = c(-0.25, 0.25), color = "red", linetype = "dashed") +
#   labs(title = "Z-score (Implemented)", x = "State", y = "Z-score") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# ggplot(ztableI, aes(x = State, y = `3PointOut`)) + 
#   geom_point(position = position_jitter(width = 0.2, height = 0), size = 1) +
#   geom_hline(yintercept = c(-0.25, 0.25), color = "red", linetype = "dashed") +
#   labs(title = "Z-score (Implemented)", x = "State", y = "Z-score") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))

 ggplot(ztable, aes(x = State, y = `1PointOut`)) + 
   geom_point(position = position_jitter(width = 0.2, height = 0), size = 1) +
   geom_hline(yintercept = c(-0.25, 0.25), color = "red", linetype = "dashed") +
   labs(title = "Z-score (Implemented)", x = "State", y = "Z-score") +
   theme_minimal() +
   theme(axis.text.x = element_text(angle = 90, hjust = 1))
 ggplot(ztable, aes(x = State, y = `2PointOut`)) + 
   geom_point(position = position_jitter(width = 0.2, height = 0), size = 1) +
   geom_hline(yintercept = c(-0.25, 0.25), color = "red", linetype = "dashed") +
   labs(title = "Z-score (Implemented)", x = "State", y = "Z-score") +
   theme_minimal() +
   theme(axis.text.x = element_text(angle = 90, hjust = 1))
 ggplot(ztable, aes(x = State, y = `3PointOut`)) + 
   geom_point(position = position_jitter(width = 0.2, height = 0), size = 1) +
   geom_hline(yintercept = c(-0.25, 0.25), color = "red", linetype = "dashed") +
   labs(title = "Z-score (Implemented)", x = "State", y = "Z-score") +
   theme_minimal() +
   theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Accept and build CFs that fall within this region
dataC1 <- dataV1[Zscore >= -0.25 & Zscore <= 0.25]
dataC1$Homicides_2 <- NULL
dataC1$Homicides_3 <- NULL
dataC2 <- dataV2[Zscore2 >= -0.25 & Zscore2 <= 0.25]
dataC2$Homicides_3 <- NULL
dataC3 <- dataV3[Zscore3 >= -0.25 & Zscore3 <= 0.25]

# Write out confident predictions
fwrite(dataC1,'./project/volume/data/interim/CAvg1pointout.csv')
fwrite(dataC2,'./project/volume/data/interim/CAvg2pointout.csv')
fwrite(dataC3,'./project/volume/data/interim/CAvg3pointout.csv')


sub_case <- dataC1[State == 'AZ' & LawName == 'ccbackground']

# Get the homicide indices and years relative to current
hom_cols <- names(sub_case)[grepl("Homicides", names(sub_case))]
homicides <- unlist(sub_case[, ..hom_cols], use.names = FALSE)
years_to_change <- as.numeric(sub("Homicides_", "", hom_cols))

# Clean to remove indices with NA values 
valid_cols <- !is.na(homicides)
valid_data <- homicides[valid_cols]
valid_years <- years_to_change[valid_cols]

# Identify pre-intervention and post-intervention data 
pre_change_data <- valid_data[valid_years <= 0]
post_change_data <- valid_data[valid_years >= 0]
pre_change_years <- valid_years[valid_years <= 0]
post_change_years <- valid_years[valid_years >= 0]








