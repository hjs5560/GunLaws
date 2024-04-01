# Import necessary libraries 
library(data.table)
library(ggplot2)
library(tidyverse)

# Read data set 
data <- fread('./project/volume/data/interim/dataHom.csv')

# Get specific case 
sub_case <- data[State == 'MO' & Year == 2007 & LawName == 'age21handgunsale']

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

# Calculate the rolling mean
rolling_mean <- function(data) {
  recursive_means <- numeric(length(data))
  recursive_means[1] <- data[1]
  for (i in 2:length(data)) {
    recursive_means[i] <- mean(c(recursive_means[i-1], data[i]))
  }
  return(recursive_means)
}

# Apply to pre- and post-intervention data
rolling_mean_pre <- rolling_mean(pre_change_data)

# Assume trend continues as the last smoothed value for post-intervention prediction
last_premean <- tail(rolling_mean_pre, 1)
forecast_no_intervention <- rep(last_premean, length(post_change_data))

# Visualization
plot(pre_change_years, pre_change_data, type = "l", col = "black",
     xlim = range(c(pre_change_years, post_change_years)), 
     ylim = range(c(pre_change_data, post_change_data, rolling_mean_pre)),
     xlab = "Years Relative to Law Intervention", ylab = "Homicides", main = "Homicides: Pre vs Post Intervention")
lines(post_change_years, post_change_data, col = "blue") # actual post data
lines(pre_change_years, rolling_mean_pre, col = "orange", lty = 2) # rolling pre
lines(post_change_years, forecast_no_intervention, col = "red", lty = 2) # forecast (no intervention)

# shading difference of (no and yes intervention)
x_combined <- c(post_change_years, rev(post_change_years))
y_combined <- c(post_change_data, rev(forecast_no_intervention))
polygon(x_combined, y_combined, col = rgb(1, 0, 0, 0.2), border = NA)

# labeling difference, avg change or "effect" of legislation intervention 
avg_difference <- mean(post_change_data - forecast_no_intervention)
mid_x <- mean(post_change_years)
mid_y <- min(forecast_no_intervention) + (max(post_change_data) - min(forecast_no_intervention)) / 2
text(mid_x, mid_y, labels = paste("IE:", round(avg_difference, 2)), cex = 0.8, col = "darkred", pos = 4)

# key for user friendliness 
legend("topleft", legend = c("Actual Pre-Intervention", "Actual Post-Intervention", 
                             "Pre-Intervention Rolling", "Post-Intervention Rolling", 
                             "Intervention = False", "Intervention = True", "Avg Intervention Effect"), 
       col = c("black", "blue", "orange", "green", "red", "pink", "red"), 
       lty = c(1, 1, 2, 2, 2, 2, NA), fill = c(NA, NA, NA, NA, NA, NA, rgb(1, 0, 0, 0.2)), cex = 0.6)

# MAE, MSE, RMSE
residuals <- pre_change_data - rolling_mean_pre
mae <- mean(abs(residuals), na.rm = TRUE)
mse <- mean(residuals^2, na.rm = TRUE)
rmse <- sqrt(mse)

# Z-Score Evaluation
residual_mean <- mean(residuals, na.rm = TRUE)
residual_sd <- sd(residuals, na.rm = TRUE)
z_scores <- (residuals - residual_mean) / residual_sd
summary_z_scores <- summary(z_scores)
print(summary_z_scores)

# Potential Outlier Points
outliers <- which(abs(z_scores) > 2)
if(length(outliers) > 0) {
  print(paste("Outliers detected at positions:", toString(outliers)))
} else {
  print("No outliers detected based on Z score > 2.")
}

# Z Score dist Chart
hist(z_scores, main = "Histogram of Z Scores", xlab = "Z Score", 
     breaks = 30, col = "lightblue", border = "blue")

