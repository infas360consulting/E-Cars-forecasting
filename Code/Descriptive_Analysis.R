options(scipen = 100)
# Packages

library(ggplot2)
library(dplyr)
library(assert)
library(openxlsx)
library(tidyverse)
library(ggcorrplot)

# functions
data_view <- function(columns, df) {
  View(df[, tolower(c("plz", columns))])
}

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

# Data quality check matrix

DataQuality2017 <- data_quality(df = plz2017[, -c(1,2)])

# Correlation analysis between different years of target variable

ecars <- lapply(paste("./Datasets/PLZ_", 2017:2021,".csv", sep = ""), function(x) as.data.frame(read.csv(x, sep = ";", encoding = "latin1")[, c("plz", "plz5_kba_kraft3")]) %>% arrange(desc(plz)))
ecars <- ecars %>% reduce(left_join, by = "plz")
names(ecars)[-1] <- paste("plz5_kba_kraft3_", 2017:2021, sep = "")

correlation_matrix <- cor(ecars[, names(ecars) != "plz"])

p_corr <- ggcorrplot(correlation_matrix)
p_corr
ggsave("./Ecars/Correlation_Plot_Target.jpeg")
# Time series analysis of the target variable

ecars <- lapply(2017:2021, FUN=function(x) data.frame(read.csv(paste("./Datasets/PLZ_", x, ".csv", sep = ""), sep = ";", encoding = "latin1")[, c("plz","plz5_kba_kraft3")], jahr=x) %>% arrange(desc(plz)))
ecars <- do.call(rbind.data.frame, ecars)
ecars$jahr <- factor(ecars$jahr, levels = as.character(2017:2021))
ecars$plz <- factor(ecars$plz)

# Boxplot ecars selling numbers trough time

p1 <- ggplot(data = na.omit(ecars[ecars$plz5_kba_kraft3 <= 20000, ]), aes(x=jahr, y=plz5_kba_kraft3)) + geom_boxplot()
p1

# Number of sold ecars accross different zipcodes

p2 <- ggplot(data = na.omit(ecars[substr(ecars$plz,1,2) =="81", ]), aes(y=plz5_kba_kraft3, x=jahr, col=plz, group = plz)) + geom_line() +
  xlab("Jahr") + ylab("Bestand nach Kraftstoffart - Elektro und (Benzin- und Diesel-)Hybrid")
p2
# Data View
data_view("plz5_flaeche", df=plz2017)

