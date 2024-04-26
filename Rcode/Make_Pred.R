# Import list 
library(data.table)
library(ggplot2)
library(tidyverse)

# Read data set 
dataH <- fread('./project/volume/data/interim/HLaw.csv')
dataA <- fread('./project/volume/data/interim/ALaw.csv')
dataS <- fread('./project/volume/data/interim/SLaw.csv')
evalH <- fread('./project/volume/data/interim/EvalH.csv')
evalA <- fread('./project/volume/data/interim/EvalA.csv')
evalS <- fread('./project/volume/data/interim/EvalS.csv')

predict_on_test_set <- function(dt) {
  library(data.table)
  setDT(dt)
  
  # Filter based on intervention years: only include entries at least 4 years after the dataset's start
  years_present <- range(dt$InterventionYear)
  min_valid_year <- years_present[1] + 4
  dt <- dt[InterventionYear >= min_valid_year & InterventionYear < 2016]
  
  # Identify columns representing years, ensuring they are ordered numerically
  year_cols <- grep("^[0-9]{4}$", names(dt), value = TRUE)
  year_cols <- sort(year_cols)
  
  # Initialize columns for predictions for years following the intervention up to 2016
  pred_years <- 2002:2016
  for (year in pred_years) {
    dt[, paste0("pred_", year) := NA_real_]
  }
  
  # Calculate the mean of incidents for the two years before the intervention for each row
  dt[, `:=`(mean_pre_intervention = NA_real_, sd_pre_intervention = NA_real_)]
  for (i in 1:nrow(dt)) {
    intervention_year <- dt$InterventionYear[i]
    pre_years_cols <- year_cols[year_cols < as.character(intervention_year)]
    if (length(pre_years_cols) >= 2) {
      last_two_years <- tail(pre_years_cols, 2)
      pre_values <- unlist(dt[i, .SD, .SDcols = last_two_years], use.names = FALSE)
      dt[i, mean_pre_intervention := mean(pre_values, na.rm = TRUE)]
      dt[i, sd_pre_intervention := sd(pre_values, na.rm = TRUE)]
    }
  }
  
  # Predict values for the years after the intervention
  dt[, (paste0("pred_", pred_years)) := lapply(pred_years, function(year) {
    if (year > InterventionYear) return(mean_pre_intervention) else return(NA_real_)
  }), by = .(InterventionYear)]
  
  # Calculate intervention effect as the difference between actual post-intervention values and predicted values
  effect_cols <- vector("character", length(pred_years))
  for (year in pred_years) {
    actual_col <- as.character(year)
    pred_col <- paste0("pred_", year)
    effect_col_name <- paste0("effect_", year)
    if (actual_col %in% names(dt)) {
      dt[, (effect_col_name) := get(actual_col) - get(pred_col)]
      effect_cols[year - min(pred_years) + 1] <- effect_col_name
    }
  }
  
  # Calculate the mean of the intervention effects
  # Ensure we use only the non-NULL entries in effect_cols for .SDcols
  valid_effect_cols <- effect_cols[effect_cols %in% names(dt)]
  if (length(valid_effect_cols) > 0) {
    dt[, mean_effect := rowMeans(.SD, na.rm = TRUE), .SDcols = valid_effect_cols]
  }
  
  return(dt)
}

# Apply the function
test_resultH <- predict_on_test_set(dataH)
test_resultA <- predict_on_test_set(dataA)
test_resultS <- predict_on_test_set(dataS)

fwrite(test_resultH,'./project/volume/data/interim/predH.csv')
fwrite(test_resultA,'./project/volume/data/interim/predA.csv')
fwrite(test_resultS,'./project/volume/data/interim/predS.csv')




