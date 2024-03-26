options(scipen = 100)
set.seed(2021)
# Packages
library(ggplot2)
library(dplyr)
library(openxlsx)
library(tidyverse)
library(sf)

# Datasets

plz2017 <- read.csv("./Datasets/PLZ_2017.csv", sep = ";", encoding = "latin1")
plz2021 <- read.csv("./Datasets/PLZ_2021.csv", sep = ";", encoding = "latin1")

# Read shape-file of german postcodes

postcodes <- st_read(dsn = "./Datasets/plz-5stellig/plz-5stellig.shp")

# functions

get_mapping_data <- function(data, postcodes.shape = postcodes
                             , fill = c("plz5_kba", "plz5_kba_kraft3")) {
  
  data[, "plz"] <- as.character(data[, "plz"])
  data <- data[, c("plz", "plz5_ew", "ort", fill)]
  left_join(x = data, y = postcodes.shape[, c("plz", "geometry")], by = "plz")
  
}

get_postcode_plot <- function(data, fill, city = NULL, year = NULL
                              , title = NULL, subtitle = NULL) {
  
  ggplot(data = data[data[, "ort"] %in% city, ]) +
    geom_sf(aes_string(fill = fill, geometry = "geometry")) +
    theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_blank(),
          legend.position = "bottom") +
    labs(fill = element_blank(), title = title, subtitle = subtitle)
  
  # TODO: customize legend
  # TODO: customize colour
}

# Map 2017 car registrations 
plz2017.map <- get_mapping_data(plz2017)

# Cologne
cologne.2017 <- get_postcode_plot(data = plz2017.map, fill = "log(plz5_kba_kraft3 / plz5_ew)",
                                  city = "Köln")
# Munich
munich.2017 <- get_postcode_plot(data = plz2017.map, fill = "log(plz5_kba_kraft3 / plz5_ew)"
                                 , city = "München")

# TODO: Unite polgons of 81248 and 81249

# Map 2021 ecar registrations Cologne
plz2021.map <- get_mapping_data(data = plz2021)

# Cologne
cologne.2021 <- get_postcode_plot(data = plz2021.map, fill = "log(plz5_kba_kraft3 / plz5_ew)",
                                  city = "Köln")
# Munich
munich.2021 <- get_postcode_plot(data = plz2021.map, fill = "log(plz5_kba_kraft3 / plz5_ew)",
                                  city = "München")


  
