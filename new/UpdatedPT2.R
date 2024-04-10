# Import list 
library(data.table)
library(ggplot2)
library(tidyverse)

# Read data set 
data <- fread('./project/volume/data/interim/HLaw.csv')


predict_up_to_intervention <- function(dt, k) {
  # Ensure dt is a data.table
  setDT(dt)
  
  # Apply initial filter based on intervention years
  years_present <- range(dt$InterventionYear)
  min_valid_year <- years_present[1] + 4
  dt <- dt[InterventionYear >= min_valid_year, ]
  
  # Identify and order the columns representing years, ensuring they are numeric
  year_cols <- grep("^[0-9]{4}$", names(dt), value = TRUE)
  year_cols <- year_cols[order(as.numeric(year_cols))]
  
  # Calculate the maximum number of predictions needed, adjusted for indexing
  max_predictions <- max(dt$InterventionYear) - (min(as.numeric(year_cols)) + 2)
  
  # Initialize prediction columns
  for (i in seq_len(max_predictions)) {
    dt[, paste0("mean_", i) := NA_real_]
    dt[, paste0("slope_", i) := NA_real_]
  }
  
  # Loop through each row for prediction calculations
  for (idx in 1:nrow(dt)) {
    intervention_year <- dt$InterventionYear[idx]
    
    # Extract actual incident numbers for the first k years up to the intervention
    k_years_cols <- year_cols[1:k]
    k_values <- dt[idx, .SD, .SDcols = k_years_cols][, lapply(.SD, as.numeric)]
    
    # Calculate mean of incidents based on the first k data points
    mean_k <- mean(unlist(k_values), na.rm = TRUE)
    
    # Loop over the predictions
    for (i in seq_len(max_predictions)) {
      # Adjust the year column index to start calculations from the second point for labeling
      year_col_index <- i + k
      year_col_name <- year_cols[year_col_index]
      if (as.numeric(year_col_name) < intervention_year) {
        # Apply the constant mean
        dt[idx, (paste0("mean_", i)) := mean_k]
        
        # Predict the slope and incidents for the next year based on the first k years
        if (length(k_values) >= k) {
          # Use a sequence of 1:length(values) as a numeric predictor for the lm function
          years_for_lm <- seq_len(k)
          lm_fit <- lm(unlist(k_values) ~ years_for_lm)
          slope_k <- coef(lm_fit)["years_for_lm"]
          last_point <- tail(unlist(k_values), n = 1)
          predicted_incidents <- last_point + slope_k * (i)  # Adjust for the actual year difference
          dt[idx, (paste0("slope_", i)) := predicted_incidents]
        }
      }
    }
  }
  
  # Reorder columns: non-predictive, mean, then slope predictions
  mean_cols <- names(dt)[grep("mean_", names(dt))]
  slope_cols <- names(dt)[grep("slope_", names(dt))]
  non_pred_cols <- setdiff(names(dt), c(mean_cols, slope_cols))
  dt <- dt[, c(non_pred_cols, mean_cols, slope_cols), with = FALSE]
  
  return(dt)
}


# Apply the function to the data
result <- predict_up_to_intervention(data, 2)

calculate_z_scores <- function(data, k) {
  # Ensure data is a data.table
  setDT(data)
  
  # Find historical and prediction column names
  mean_cols <- grep("^mean_", names(data), value = TRUE)
  slope_cols <- grep("^slope_", names(data), value = TRUE)
  year_cols <- grep("^[0-9]{4}$", names(data), value = TRUE)
  
  # Initialize lists to store Z-scores
  z_scores_mean <- vector("list", length(mean_cols))
  z_scores_slope <- vector("list", length(slope_cols))
  
  # Calculate Z-scores for each prediction step
  for (i in seq_along(mean_cols)) {
    # Correctly select historical columns including all years up to and including the year of prediction
    # Adjust the index to ensure it starts from 1 and correctly accumulates historical years
    historical_cols_index <- 1:(k + i)
    if (historical_cols_index[length(historical_cols_index)] <= length(year_cols)) {
      historical_cols <- year_cols[historical_cols_index]
      
      # Calculate historical mean and SD using the correctly selected historical columns
      historical_mean <- rowMeans(data[, ..historical_cols], na.rm = TRUE)
      historical_sd <- apply(data[, ..historical_cols], 1, sd, na.rm = TRUE)
      
      # Calculate Z-score for the current prediction step
      mean_predictions <- data[[mean_cols[i]]]
      slope_predictions <- data[[slope_cols[i]]]
      
      z_scores_mean[[i]] <- (mean_predictions - historical_mean) / historical_sd
      z_scores_slope[[i]] <- (slope_predictions - historical_mean) / historical_sd
    } else {
      z_scores_mean[[i]] <- rep(NA, nrow(data))
      z_scores_slope[[i]] <- rep(NA, nrow(data))
    }
  }
  
  # Combine Z-scores into a list for output
  z_scores_list <- list(mean = z_scores_mean, slope = z_scores_slope)
  return(z_scores_list)
}

z_scores_all_steps <- calculate_z_scores(result, 2)

# Convert z-scores list to data frame
z_scores_df <- as.data.frame(z_scores_all_steps)
names(z_scores_df)[1] <- "1stepMeanZscore"
names(z_scores_df)[2] <- "2stepMeanZscore"
names(z_scores_df)[3] <- "3stepMeanZscore"
names(z_scores_df)[4] <- "4stepMeanZscore"
names(z_scores_df)[5] <- "5stepMeanZscore"
names(z_scores_df)[6] <- "6stepMeanZscore"
names(z_scores_df)[7] <- "7stepMeanZscore"
names(z_scores_df)[8] <- "8stepMeanZscore"
names(z_scores_df)[9] <- "9stepMeanZscore"
names(z_scores_df)[10] <- "10stepMeanZscore"
names(z_scores_df)[11] <- "11stepMeanZscore"
names(z_scores_df)[12] <- "12stepMeanZscore"
names(z_scores_df)[13] <- "13stepMeanZscore"
names(z_scores_df)[14] <- "14stepMeanZscore"
names(z_scores_df)[15] <- "15stepMeanZscore"
names(z_scores_df)[16] <- "1stepSlopeZscore"
names(z_scores_df)[17] <- "2stepSlopeZscore"
names(z_scores_df)[18] <- "3stepSlopeZscore"
names(z_scores_df)[19] <- "4stepSlopeZscore"
names(z_scores_df)[20] <- "5stepSlopeZscore"
names(z_scores_df)[21] <- "6stepSlopeZscore"
names(z_scores_df)[22] <- "7stepSlopeZscore"
names(z_scores_df)[23] <- "8stepSlopeZscore"
names(z_scores_df)[24] <- "9stepSlopeZscore"
names(z_scores_df)[25] <- "10stepSlopeZscore"
names(z_scores_df)[26] <- "11stepSlopeZscore"
names(z_scores_df)[27] <- "12stepSlopeZscore"
names(z_scores_df)[28] <- "13stepSlopeZscore"
names(z_scores_df)[29] <- "14stepSlopeZscore"
names(z_scores_df)[30] <- "15stepSlopeZscore"

# Ranges
summary(z_scores_df)

# Merge for graphing
final <- cbind(result, z_scores_df)

mean_z_cols <- grep("stepMeanZscore$", names(final), value = TRUE)
slope_z_cols <- grep("stepSlopeZscore$", names(final), value = TRUE)

final$meanZAvg <- rowMeans(final[, ..mean_z_cols], na.rm = TRUE)
final$meanZSlope <- rowMeans(final[, ..slope_z_cols], na.rm = TRUE)

# Best Score Calculation
final$Best <- apply(final[, c("meanZAvg", "meanZSlope")], 1, function(x) {
  which.min(abs(x))
})

# The Mean Model Out Preforms The Slope Model Everytime 
summary(final$Best)
summary(final$meanZAvg)


fwrite(final,'./project/volume/data/interim/master.csv')






