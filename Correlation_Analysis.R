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

