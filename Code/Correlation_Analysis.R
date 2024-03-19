options(scipen = 100)

# Packages
library(ggplot2)
library(dplyr)
library(assert)
library(openxlsx)
library(tidyverse)
library(ggcorrplot)

# Datasets
plz2017 <- read.csv("./Datasets/PLZ_2017.csv", sep = ";", encoding = "latin1")
plz2021 <- read.csv("./Datasets/PLZ_2021.csv", sep = ";", encoding = "latin1")

# Columns to analyse 
correlation_columns <- intersect(names(plz2017), names(plz2021))
n_correlation_columns <- length(correlation_columns)


# TODO: (Mohamed)

correlation_columns_mohamed <- correlation_columns[seq(from = 1, by = 1, to = n_correlation_columns / 2)]

# TODO: (Ilija)

correlation_columns_ilija <- correlation_columns[seq(from = (n_correlation_columns/2) + 1, by = 1, to = n_correlation_columns)]

