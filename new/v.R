# Import list 
library(data.table)
library(ggplot2)
library(tidyverse)

# Read data set 
pred <- fread('./project/volume/data/interim/predictions.csv')
validation <- fread('./project/volume/data/interim/master.csv')

predS <- pred[, -(1:22)]

master2 <- cbind(validation, predS)

zcolumns <- master2[, 53:67]

row_sd <- function(row) {
  sd(row, na.rm = TRUE)
}

len_sd <- function(row) {
  sum(!is.na(row))
}

mean <- master2$meanZAvg
mean <- data.frame(mean)

N <- apply(zcolumns, 1, len_sd)
N <- data.frame(N)

SD_Zscore  <- apply(zcolumns, 1, row_sd)
SD_Zscore <- data.frame(SD_Zscore)

confidence_level <- 0.95

critical_value <- qnorm((1 + confidence_level) / 2)

standard_error <- SD_Zscore * sqrt(1 + 1/N)

lower_bound <- mean - critical_value * standard_error
upper_bound <- mean + critical_value * standard_error

results <- data.frame(
  mean = mean,
  SD_Zscore = SD_Zscore,
  N = N,
  lower_bound = lower_bound,
  upper_bound = upper_bound
)




