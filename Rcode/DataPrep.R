# Input Statements
library(data.table)
library(readxl)
library(dplyr)
library(tidyr)

# Read Data
assault <- fread('./project/volume/data/raw/GAgg.csv')
homicide <- fread('./project/volume/data/raw/GHom.csv')
suicide <- fread('./project/volume/data/raw/GSui.csv')
laws <- read_excel('./project/volume/data/raw/Laws.xlsx')

str(assault)
str(laws)

laws$lawtotal<- NULL

# Cleaning for consistency 
filter_laws <- laws %>% filter(year <= 2016)
filter_laws <- filter_laws %>% rename(Year = year, State = state)

# Take care of state names to abbreviations for consistency 
filter_laws$State <- state.abb[match(filter_laws$State, state.name)]

# Somehow it made Louisiana LA and we need it to be LO to work with violence data
filter_laws <- filter_laws %>%
  mutate(State = ifelse(State == "LA", "LO", State))

mergedAH <- merge(assault[,.(Year, USA)], homicide[,.(Year, USA)], by = "Year", all.x = TRUE)
names(mergedAH)[names(mergedAH) == "USA.x"] <- "Assaults"
names(mergedAH)[names(mergedAH) == "USA.y"] <- "Homicides"
mergedAHS <- merge(mergedAH, suicide[,.(Year, USA)], by = "Year", all.x = TRUE)
names(mergedAHS)[names(mergedAHS) == "USA"] <- "Suicides"

# Write csv for national level
fwrite(mergedAHS,'./project/volume/data/interim/nat_violence.csv')

# Removing DC because they aren't in laws data
assault <- assault[, c("USA", "DC") := NULL]
homicide <- homicide[, c("USA", "DC") := NULL]
suicide <- suicide[, c("USA", "DC") := NULL]

# Data to long
a_long <- pivot_longer(assault, cols = -Year, names_to = "State", values_to = "Incidents")
h_long <- pivot_longer(homicide, cols = -Year, names_to = "State", values_to = "Incidents")
s_long <- pivot_longer(suicide, cols = -Year, names_to = "State", values_to = "Incidents")

# Merge data sets
merged1 <- merge(filter_laws, a_long, by = c("Year", "State"), all.x = TRUE)
names(merged1)[names(merged1) == "Incidents"] <- "Assaults"
merged2 <- merge(merged1, h_long, by = c("Year", "State"), all.x = TRUE)
names(merged2)[names(merged2) == "Incidents"] <- "Homicides"
law_violence <- merge(merged2, s_long, by = c("Year", "State"), all.x = TRUE)
names(law_violence)[names(law_violence) == "Incidents"] <- "Suicides"

# Write CSVs 
fwrite(law_violence,'./project/volume/data/interim/law_violenceSL.csv')

