# Import list 
library(data.table)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(edd)
library(esquisse)
library(gridExtra)  

# Read data set 
predH <- fread('./project/volume/data/interim/predH.csv')
predA <- fread('./project/volume/data/interim/predA.csv')
predS <- fread('./project/volume/data/interim/predS.csv')

evalH <- fread('./project/volume/data/interim/EvalH.csv')
evalA <- fread('./project/volume/data/interim/EvalA.csv')
evalS <- fread('./project/volume/data/interim/EvalS.csv')

MasterEval <- rbind(evalH, evalA, evalS) 

colnames(MasterEval)

MasterEvalLong <- tidyr::gather(MasterEval, key = "Models", value = "value", meanZAvg, meanZSlope)

ggplot(MasterEvalLong, aes(x = Models, y = value, fill = Models)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#F5F5DC", "#009CDE"), labels = c("Mean", "Slope")) +  
  scale_x_discrete(labels = c("meanZAvg" = "Mean", "meanZSlope" = "Slope")) +
  theme_minimal() +
  labs(x = "Models", y = "Z-Scores", title = "Distribution of Z-scores: Mean vs. Slope Model")

analyze_intervention <- function(data, state, law) {
  # Ensure data is a data.table
  setDT(data)
  
  # Filter data for the specific state and law
  subset <- data[State == state & Law == law]
  
  # Exit if no data is found
  if (nrow(subset) == 0) {
    print("No data available for the specified state and law.")
    return()
  }
  
  # Get intervention year
  intervention_year <- subset$InterventionYear[1]
  
  # Prepare data for plotting
  years <- as.character(1999:2016)  # Adjust based on your dataset years
  actual_data <- melt(subset, measure.vars = years, variable.name = "Year", value.name = "Value")
  actual_data[, Type := "Actual"]
  
  # Filter valid predicted columns before melting
  predicted_years <- paste0("pred_", years)
  valid_predicted_columns <- predicted_years[predicted_years %in% names(subset)]
  
  # Ensure all predicted data columns are numeric
  subset[, (valid_predicted_columns) := lapply(.SD, as.numeric), .SDcols = valid_predicted_columns]
  
  predicted_data <- melt(subset, measure.vars = valid_predicted_columns, variable.name = "Year", value.name = "Value")
  predicted_data[, Type := "Predicted"]
  
  # Merging data with handling missing values
  plot_data <- rbindlist(list(actual_data, predicted_data), fill = TRUE)
  plot_data[, Year := as.numeric(gsub("pred_", "", Year))]
  
  # Plotting with ggplot
  p <- ggplot(plot_data, aes(x = Year, y = Value, color = Type)) +
    geom_line() +
    geom_point() +
    geom_vline(xintercept = intervention_year, linetype = "dashed", color = "blue") +
    geom_rect(aes(xmin = intervention_year - 1, xmax = intervention_year + 1, ymin = -Inf, ymax = Inf), fill = "yellow", alpha = 0.01, color = NA) +  # Removed outline
    scale_x_continuous(breaks = 1999:2016) +
    labs(title = paste("Intervention Analysis for", state, "-", law),
         x = "Year", y = "Value") +
    theme_minimal() +
    theme(legend.position = "none") + 
    geom_point(na.rm = TRUE)
  
  print(p)
}

# Example usage
analyze_intervention(predH, "AL", "ccbackground")


plot_data <- predH %>%
  select(State, Law, starts_with("effect_")) %>%
  gather(key = "Year", value = "EffectSize", -State, -Law)

plot_data$Year <- as.integer(sub("effect_", "", plot_data$Year))
plot_data <- na.omit(plot_data)

p <- ggplot(plot_data, aes(x = Law, y = EffectSize)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Distribution of Effect Sizes by Law", x = "Type of Law", y = "Effect Size")

print(p)

implemented_laws <- predH %>%
  filter(Intervention == "implemented")

summary(implemented_laws$mean_effect)

removed_laws <- predH %>%
  filter(Intervention == "removal")

summary(removed_laws$mean_effect)

plot_implemented <- ggplot(implemented_laws, aes(x = Law, y = mean_effect)) +
  geom_boxplot(fill = "#b08980") +
  labs(title = "Effect Sizes for Implemented Laws", x = "Type of Law", y = "Effect Size") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(plot_implemented)

plot_removed <- ggplot(removed_laws, aes(x = Law, y = mean_effect)) +
  geom_boxplot(fill = "#b08980") +
  labs(title = "Effect Sizes for Removed Laws", x = "Type of Law", y = "Effect Size") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(plot_removed)

custom_palette <- generate_chalky_clay_palette(104)

scatter_plot <- ggplot(plot_data, aes(x = Year, y = EffectSize)) +
  geom_point(aes(color = Law), alpha = 0.6) +  # Use 'color' aesthetic for point colors
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  labs(title = "Trend of Effect Sizes Over Time", x = "Year", y = "Effect Size") +
  theme_minimal() +
  scale_color_manual(values = custom_palette) +  # Apply custom color palette to point colors
  theme(legend.position = "none")

print(scatter_plot)

density_plot <- ggplot(plot_data, aes(x = EffectSize, fill = Law)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density of Effect Sizes by Law", x = "Effect Size", y = "Density") +
  theme_minimal() +
  theme(legend.position = "none")

print(density_plot)

generate_chalky_clay_palette <- function(num_colors) {
  # Define a range of HSL (hue, saturation, lightness) values for chalky clay-like colors
  hues <- seq(20, 50, length.out = num_colors)  # Adjust the range of hues as needed
  
  # Create a color palette with HSL values (low saturation, moderate lightness)
  chalky_clay_palette <- hcl(h = hues, c = 30, l = 70)  # Low saturation and moderate lightness
  
  return(chalky_clay_palette)
}

custom_palette <- generate_chalky_clay_palette(83)

bar_chart <- ggplot(implemented_laws, aes(x = Law, y = mean_effect, fill = Law)) +
  geom_bar(stat = "identity") +
  labs(title = "Mean Effect Sizes by Law Implementation", x = "Type of Law", y = "Mean Effect Size") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = custom_palette) +  
  theme(legend.position = "none")

print(bar_chart)

custom_palette <- generate_chalky_clay_palette(83)

bar_chartR <- ggplot(removed_laws, aes(x = Law, y = mean_effect, fill = Law)) +
  geom_bar(stat = "identity") +
  labs(title = "Mean Effect Sizes by Law Removal", x = "Type of Law", y = "Mean Effect Size") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = custom_palette) +  
  theme(legend.position = "none")

print(bar_chartR)

summarize_implementations <- function(data, violence_type) {
  summary <- data %>%
    filter(Intervention == "implemented") %>%
    summarise(
      Mean_Effect = mean(mean_effect, na.rm = TRUE),
      SD = sd(mean_effect, na.rm = TRUE),
      Violence_Type = violence_type
    )
  return(summary)
}

summary_a <- summarize_implementations(predA, "Assaults")
summary_h <- summarize_implementations(predH, "Homicides")
summary_s <- summarize_implementations(predS, "Suicides")

all_summaries <- rbind(summary_a, summary_h, summary_s)

box_data <- rbind(
  predA %>% filter(Intervention == "implemented") %>% mutate(Violence_Type = "Assaults"),
  predH %>% filter(Intervention == "implemented") %>% mutate(Violence_Type = "Homicides"),
  predS %>% filter(Intervention == "implemented") %>% mutate(Violence_Type = "Suicides")
)

box_plot_facet <- ggplot(box_data, aes(x = Violence_Type, y = mean_effect, fill = Violence_Type)) +
  geom_boxplot() +
  facet_wrap(~ Violence_Type, scales = "free_y") +  # Facets with free y scales
  labs(title = "Distribution of Implementation Effects by Violence Type",
       x = "",  # Remove the x-axis label
       y = "Implementation Effects") +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),  # Remove the x-axis text
    axis.ticks.x = element_blank(),  # Remove the x-axis ticks if desired
    strip.background = element_rect(fill = "white"),  # Make facet labels background white
    strip.text = element_text(size = 12)  # Adjust facet label text size if needed
  ) +
  scale_fill_manual(values = c("Assaults" = "#BFCFFF", "Homicides" = "blue", "Suicides" = "lightblue")) 

box_plot_facet

box_dataR <- rbind(
  predA %>% filter(Intervention == "removal") %>% mutate(Violence_Type = "Assaults"),
  predH %>% filter(Intervention == "removal") %>% mutate(Violence_Type = "Homicides"),
  predS %>% filter(Intervention == "removal") %>% mutate(Violence_Type = "Suicides")
)

box_plot_facetR <- ggplot(box_dataR, aes(x = Violence_Type, y = mean_effect, fill = Violence_Type)) +
  geom_boxplot() +
  facet_wrap(~ Violence_Type, scales = "free_y") +  # Facets with free y scales
  labs(title = "Distribution of Removal Effects by Violence Type",
       x = "",  # Remove the x-axis label
       y = "Removal Effects") +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),  # Remove the x-axis text
    axis.ticks.x = element_blank(),  # Remove the x-axis ticks if desired
    strip.background = element_rect(fill = "white"),  # Make facet labels background white
    strip.text = element_text(size = 12)  # Adjust facet label text size if needed
  ) +
  scale_fill_manual(values = c("Assaults" = "#BFCFFF", "Homicides" = "blue", "Suicides" = "lightblue")) 

box_plot_facetR
