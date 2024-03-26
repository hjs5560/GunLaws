# Load necessary libraries
library(tidyverse)
library(tidyr)
library(data.table)
library(dplyr)
library(data.table)
data <- fread('C:\\Users\\hsugd\\Downloads\\ALaw.csv')

data_filtered <- filter(data, InterventionYear >= 2004, InterventionYear <= 2016)

calculate_slope <- function(y1, y2) {
  slope <- y2 - y1
  return(slope)
}

calculate_z_scores <- function(errors) {
  mean_error <- mean(errors)
  sd_error <- sd(errors)
  z_scores <- (errors - mean_error) / sd_error
  return(z_scores)
}

# empty dataframe
results <- tibble(
  State = character(),
  Law = character(),
  Slope = numeric(),
  MeanError = numeric(),
  VarianceError = numeric(),
  ZScores = list()
)

for (i in 1:nrow(data_filtered)) {
  row <- data_filtered[i, ]
  
  # Calculate the slope using 1999 and 2000
  slope <- calculate_slope(row$`1999`, row$`2000`)
  
  # project values for 2001, 2002, and 2003
  projections <- row$`2000` + slope * 1:3
  actuals <- c(row$`2001`, row$`2002`, row$`2003`)
  errors <- actuals - projections
  
  # mean and variance
  mean_error <- mean(errors)
  variance_error <- var(errors)
  
  # calculate z-scores
  z_scores <- calculate_z_scores(errors)
  
  # append to results
  results <- rbind(results, tibble(
    State = row$State,
    Law = row$Law,
    Slope = slope,
    MeanError = mean_error,
    VarianceError = variance_error,
    ZScores = list(z_scores)
  ))
}

print(results)


average_z_scores <- function(z_scores_list) {
  sapply(z_scores_list, function(z_scores) mean(z_scores, na.rm = TRUE))
}

# average z-scores
results <- results %>%
  mutate(AvgZScore = average_z_scores(ZScores),
         SD = map_dbl(ZScores, sd), 
         MeanZScore = map_dbl(ZScores, mean)) %>% 
  select(-ZScores)

#write_csv(results, 'C:\\Users\\hsugd\\Downloads\\results.csv')









library(ggplot2)
library(dplyr)

# first case
example_case <- data_filtered[32, ]

years <- 1999:2003
actual_values <- c(example_case$`1999`, example_case$`2000`, example_case$`2001`, example_case$`2002`, example_case$`2003`)
slope <- calculate_slope(example_case$`1999`, example_case$`2000`)
projected_values <- example_case$`2000` + slope * (0:4) # Projects from 2000 to 2003 using the slope
errors <- actual_values[3:5] - projected_values[3:5]
mean_error <- mean(errors)
variance_error <- var(errors)

# Combined actual and projected data for plotting
plot_data <- data.frame(
  Year = rep(years, 2),
  Value = c(actual_values, projected_values),
  Type = rep(c("Actual", "Projected"), each = 5)
)

# Calculate the mean of the actual values for 2001 to 2003
mean_actuals <- mean(actual_values[3:5])

# Plot
p <- ggplot(plot_data, aes(x = Year, y = Value, color = Type)) +
  geom_line() +  # Draw lines to connect points
  geom_point() +  # Draw the data points
  geom_hline(yintercept = mean_actuals, linetype = "dashed", color = "green", size = 1) +  # Add mean line
  labs(title = paste("Analysis of", example_case$Law, "in", example_case$State),
       subtitle = paste("Mean Error:", round(mean_error, 2), "- Variance of Error:", round(variance_error, 2), "- Mean Actuals:", round(mean_actuals, 2)),
       x = "Year", y = "Homicides") +
  theme_minimal() +
  scale_color_manual(values = c("Actual" = "blue", "Projected" = "red")) +
  annotate("text", x = 2002, y = mean_actuals, label = paste("Mean Actuals:", round(mean_actuals, 2)), vjust = -1) # Annotate the mean

print(p)
