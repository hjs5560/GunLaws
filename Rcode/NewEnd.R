# Input Statements
library(data.table)
library(readxl)
library(dplyr)
library(tidyr)

# Read Data
assault <- fread('./project/volume/data/raw/GAgg.csv')
homicide <- fread('./project/volume/data/raw/GHom.csv')
suicide <- fread('./project/volume/data/raw/GSui.csv')
laws <- fread('./project/volume/data/interim/law_change.csv')

# Remove unwanted columns
assault$USA <- NULL
homicide$USA <- NULL
suicide$USA <- NULL
assault$DC <- NULL
homicide$DC <- NULL
suicide$DC <- NULL
assault$MA <- NULL
homicide$MA <- NULL
suicide$MA <- NULL

# Reformat datasets to long
assault_long <- melt(assault, id.vars = "Year", variable.name = "State", value.name = "Cases")
assault_wide <- spread(assault_long, key = Year, value = Cases)
hom_long <- melt(homicide, id.vars = "Year", variable.name = "State", value.name = "Cases")
hom_wide <- spread(hom_long, key = Year, value = Cases)
sui_long <- melt(suicide, id.vars = "Year", variable.name = "State", value.name = "Cases")
sui_wide <- spread(sui_long, key = Year, value = Cases)

# Law Information
laws1 <- laws[, c("State", "LawName", "Switching", "Year")]
colnames(laws1) <- c("State", "Law", "Intervention", "InterventionYear")

# Merge with violence data 
homicide_law <- merge(laws1, hom_wide, by = "State", all = TRUE)
assault_law <- merge(laws1, assault_wide, by = "State", all = TRUE)
suicide_law <- merge(laws1, sui_wide, by = "State", all = TRUE)

str(homicide_law)
str(assault_law)
str(suicide_law)

# Write out completed datasets
fwrite(homicide_law,'./project/volume/data/interim/HLaw.csv')
fwrite(assault_law,'./project/volume/data/interim/ALaw.csv')
fwrite(suicide_law,'./project/volume/data/interim/SLaw.csv')












