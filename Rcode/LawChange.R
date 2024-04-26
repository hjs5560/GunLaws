# Input Statements
library(data.table)
library(ggplot2)
library(dplyr)
library(tidyr)
library(esquisse)

# Read Data
laws <- fread('./project/volume/data/interim/law_violenceSL.csv')

# Function to Detect Changes In Law
detect <- function(df) {
  # df: Laws data set
  # Output: Data set that identifies each case of removal and implementation
  
  law_cols <- setdiff(names(df), c("Year", "State", "Assaults", "Homicides", "Suicides"))
  
  # Change Laws to long format for detection
  df_long <- df %>%
    pivot_longer(cols = all_of(law_cols), names_to = "LawName", values_to = "LawStatus") %>%
    arrange(State, Year, LawName)
  
  # Detect Change from 1 - 0 or 0 - 1
  df_changes <- df_long %>%
    group_by(State, LawName) %>%
    mutate(PreviousLawStatus = lag(LawStatus, default = first(LawStatus)),
           LawChange = LawStatus != PreviousLawStatus,
           Switching = case_when(LawStatus == 1 & LawChange ~ "implemented",
                                 LawStatus == 0 & LawChange ~ "removal",
                                 TRUE ~ NA_character_)) %>%
    filter(LawChange) %>%
    ungroup() %>%
    select(State, Year, LawName, Switching)
  
  # Select Previous and Next Years 
  df$PreviousYear = df$Year + 1
  df$NextYear = df$Year - 1
  
  df_joined <- df_changes %>%
    left_join(df, by = c("State", "Year")) %>%
    left_join(df %>% rename_with(~paste0(., ".prev"), matches("Assaults|Homicides|Suicides")), 
              by = c("State", "Year" = "PreviousYear")) %>%
    left_join(df %>% rename_with(~paste0(., ".next"), matches("Assaults|Homicides|Suicides")), 
              by = c("State", "Year" = "NextYear"))
  
  # Define New DF with Respective Columns
  new_df <- df_joined %>%
    transmute(State, Year, LawName, Switching,
              PreviousYearAssaults = Assaults.prev, CurrentYearAssaults = Assaults, NextYearAssaults = Assaults.next,
              PreviousYearHomicides = Homicides.prev, CurrentYearHomicides = Homicides, NextYearHomicides = Homicides.next,
              PreviousYearSuicides = Suicides.prev, CurrentYearSuicides = Suicides, NextYearSuicides = Suicides.next)
  
  return(new_df)
}

# Apply to Data
LawChange <- detect(laws)

str(LawChange)

# Write new csv to interim 
fwrite(LawChange,'./project/volume/data/interim/law_change.csv')



