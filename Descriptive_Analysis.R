options(scipen = 100)
# Packages

library(ggplot2)
library(dplyr)
library(assert)

# functions

# Calculates most frequent values in a column
modus <- function(x) {
  return(names(sort(table(x), decreasing = T, na.last = T)[1]))
}

# Calculates descriptive statistics of each column

data_quality <- function(df) {
  column_names <- names(df)
  result <- apply(X = df, MARGIN = 2, function(x) list(minimum = min(x, na.rm = TRUE), maximum = max(x, na.rm = TRUE), median = median(x, na.rm = TRUE),
                                                       average = mean(x, na.rm =TRUE), mode = modus(x), missing_values = sum(is.na(x) | x == -99)))
  do.call(rbind.data.frame, result)
  }


# Loading Data

plz2017 <- read.csv("./Datasets/PLZ_2017.csv", sep = ";", encoding = "latin1")

# Dimensions of data

dim(plz2017)

# Data quality check

DataQuality2017 <- data_quality(df = plz2017[, -c(1,2)])

