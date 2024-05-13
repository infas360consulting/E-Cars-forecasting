options(scipen = 100)

# Packages
library(ggplot2)
library(dplyr)
library(assert)
library(openxlsx)
library(tidyverse)
library(ggcorrplot)

# Datasets
source("./Code/Dataloader.R")

#function 

right <- function(x, n) {
  substr(x, nchar(x) - n + 1, nchar(x))
}


# Columns to analyse
correlation_columns <- intersect(names(plz2017), names(plz2021))
correlation_columns17 <- correlation_columns[sapply(plz2017[, correlation_columns], function(x) is.numeric(x) & length(unique(x)) > 1)]
correlation_columns21 <- correlation_columns[sapply(plz2021[, correlation_columns], function(x) is.numeric(x) & length(unique(x)) > 1)]
correlation_columns <- intersect(correlation_columns21, correlation_columns17)
correlation_columns <- correlation_columns[!correlation_columns %in% c("plz5_kba", "plz5_kba_kraft3")]

# Analysis between years
correlation_vector <- sapply(X = correlation_columns, FUN = function(x) cor(x = plz2017[, x], y = plz2021[, x]))
correlation_vector <- as.numeric(correlation_vector)
correlation_vector 

mean(correlation_vector, na.rm = TRUE)
sd(correlation_vector,na.rm = TRUE)

temp <- data.frame(correlation = correlation_vector)

box_cor <- ggplot(data = temp, aes(y = correlation )) + geom_boxplot(width = 0.3) + theme(axis.text.x  = element_blank(),
                                                                               axis.ticks = element_blank(),
                                                                               axis.title.x = element_blank(),
                                                                               axis.text.y = element_text(size = 12)) + 
  xlim(c(-1,1)) + ylab("Pearson Correlation") + scale_y_continuous(breaks = seq(-0.5,1,0.25))

ggsave("./Ecars/Boxplot_Pairwise_Correlation.jpeg", box_cor)
# Analysis within years
# TODO: (Mohamed)

# TODO: (Ilija)

