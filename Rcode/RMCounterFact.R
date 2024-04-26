# Import necessary libraries 
library(data.table)
library(ggplot2)
library(tidyverse)

# Read data set 
data <- fread('./project/volume/data/interim/law_minus_plus.csv')

# Filter data for specific state and law
data <- subset(data, State == "MO" & LawName == "age21handgunsale" & Year == 2007)

# Get the homicide indices and years relative to current
hom_cols <- names(data)[grepl("Homicides", names(data))]
homicides <- unlist(data[, ..hom_cols], use.names = FALSE)
years_to_change <- as.numeric(sub("Homicides_", "", hom_cols))

# Assume the current year column holds the year of the law change
implementation_year <- data$Year[1]

# Calculate actual years from relative years
actual_years <- implementation_year + years_to_change

# Clean to remove indices with NA values 
valid_cols <- !is.na(homicides)
valid_data <- homicides[valid_cols]
valid_years <- actual_years[valid_cols]

# Identify pre-intervention and post-intervention data 
pre_change_data <- valid_data[valid_years <= implementation_year]
post_change_data <- valid_data[valid_years >= implementation_year]
pre_change_years <- valid_years[valid_years <= implementation_year]
post_change_years <- valid_years[valid_years >= implementation_year]
# Calculate the rolling mean
years_to_calculate_mean <- c(implementation_year - 2, implementation_year - 1)
mean_last_two_pre <- mean(pre_change_data[pre_change_years %in% years_to_calculate_mean])

forecast_start_index <- which(post_change_years == (implementation_year + 1))
forecast_no_intervention <- rep(mean_last_two_pre, length(post_change_data[forecast_start_index:length(post_change_data)]))

# Visualization
plot(pre_change_years, pre_change_data, type = "l", col = "black",
     xlim = range(c(pre_change_years, post_change_years)), 
     ylim = range(c(pre_change_data, post_change_data)),
     xlab = "Years", ylab = "Homicides", main = " ")
lines(post_change_years, post_change_data, col = "blue", type = "l")  # actual post data
lines(post_change_years[forecast_start_index:length(post_change_data)], forecast_no_intervention, col = "red", lty = 2)  # forecast (no intervention)

title(main = "Counterfactual: MO with the Removal of `AGE21HANDGUNSALE`", cex.main = 0.8)

# shading difference of (no and yes intervention)
y_combined <- c(post_change_data[forecast_start_index:length(post_change_data)], rev(forecast_no_intervention))
x_combined <- c(post_change_years[forecast_start_index:length(post_change_data)], rev(post_change_years[forecast_start_index:length(post_change_data)]))
polygon(x_combined, y_combined, col = "lightblue", border = NA)

# labeling difference, avg change or "effect" of legislation intervention 
avg_difference <- mean(post_change_data[forecast_start_index:length(post_change_data)] - forecast_no_intervention)
mid_x <- mean(post_change_years[forecast_start_index:length(post_change_data)])
mid_y <- min(forecast_no_intervention) + (max(post_change_data[forecast_start_index:length(post_change_data)]) - min(forecast_no_intervention)) / 2
text(mid_x-1, mid_y, labels = paste("IE:", round(avg_difference, 2)), cex = 0.8, col = "black", pos = 4)

# key for user friendliness 
legend("topleft", legend = c("Actual Pre-Intervention", "Actual Post-Intervention", 
                             "Predicted Without-Intervention"), 
       col = c("black", "blue", "red"), 
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

