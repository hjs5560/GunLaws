# Import list 
library(data.table)
library(ggplot2)
library(tidyverse)

# Read data set 
data <- fread('./project/volume/data/interim/HLaw.csv')
validation <- fread('./project/volume/data/interim/master.csv')

predict_on_test_set <- function(dt) {
  # Convert to data.table if not already
  setDT(dt)
  
  # Filter based on intervention years: only include entries at least 4 years after the dataset's start
  years_present <- range(dt$InterventionYear)
  min_valid_year <- years_present[1] + 4
  dt <- dt[InterventionYear >= min_valid_year]
  
  # Identify columns representing years, ensuring they are ordered numerically
  year_cols <- grep("^[0-9]{4}$", names(dt), value = TRUE)
  year_cols <- sort(year_cols)
  
  # Initialize columns for predictions for years following the intervention up to 2016
  pred_years <- 2002:2016
  for (year in pred_years) {
    dt[, paste0("pred_", year) := NA_real_]
  }
  
  # Calculate the mean of incidents for the two years before the intervention for each row
  dt[, mean_pre_intervention := NA_real_]
  for (i in 1:nrow(dt)) {
    intervention_year <- dt$InterventionYear[i]
    pre_years_cols <- year_cols[year_cols < as.character(intervention_year)]
    if (length(pre_years_cols) >= 2) {
      last_two_years <- tail(pre_years_cols, 2)
      pre_values <- unlist(dt[i, .SD, .SDcols = last_two_years], use.names = FALSE)
      dt[i, mean_pre_intervention := mean(pre_values, na.rm = TRUE)]
    }
  }
  
  # Predict values for the years after the intervention
  dt[, (paste0("pred_", pred_years)) := lapply(pred_years, function(year) {
    if (year > InterventionYear) return(mean_pre_intervention) else return(NA_real_)
  }), by = .(InterventionYear)]
  
  return(dt)
}

# Apply the function
test_result <- predict_on_test_set(data)

test_result$mean_pre_intervention <- NULL

fwrite(test_result,'./project/volume/data/interim/predictions.csv')


