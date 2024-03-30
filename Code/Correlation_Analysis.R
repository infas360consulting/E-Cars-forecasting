options(scipen = 100)

# Packages
library(ggplot2)
library(dplyr)
library(assert)
library(openxlsx)
library(tidyverse)
library(ggcorrplot)

# Datasets
#2017
plz2017 <- read.csv("./Datasets/PLZ_2017.csv", sep = ";", encoding = "latin1")
plz2017 <- plz2017 %>% arrange(desc(plz))
#2021
plz2021 <- read.csv("./Datasets/PLZ_2021.csv", sep = ";", encoding = "latin1")
plz2021 <- plz2021[plz2021$plz %in% plz2017$plz, ]
plz2021 <- plz2021 %>% arrange(desc(plz))

#function 

right <- function(x, n) {
  substr(x, nchar(x) - n + 1, nchar(x))
}


# Columns to analyse 
correlation_columns <- intersect(names(plz2017), names(plz2021))
correlation_columns <- correlation_columns[right(correlation_columns, 2) != "kl"]
correlation_columns <- correlation_columns[!correlation_columns %in% 
                                             c("plz", "plz5_kba_kraft3","plz5_kba","ort", "Jahr")]
n_correlation_columns <- length(correlation_columns)

# Change data type

plz2017[, correlation_columns] <- apply(X = plz2017[, correlation_columns],
                                        MARGIN = 2,
                                        FUN = function(x) as.numeric(x))

plz2021[, correlation_columns] <- apply(X = plz2021[, correlation_columns],
                                        MARGIN = 2,
                                        FUN = function(x) as.numeric(x))

# Analysis between years
correlation_vector <- sapply(X = correlation_columns, FUN = function(x) cor(x = plz2017[, x], y = plz2021[, x]))
correlation_vector <- as.numeric(correlation_vector)
correlation_vector

mean(correlation_vector, na.rm = TRUE)
sd(correlation_vector,na.rm = TRUE)

# Analysis within years
# TODO: (Mohamed)

# TODO: (Ilija)

