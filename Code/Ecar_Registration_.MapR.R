options(scipen = 100)
set.seed(2021)
# Packages
library(ggplot2)
library(dplyr)
library(openxlsx)
library(tidyverse)
library(sf)


# Datasets

source("./Code/Dataloader.R")

# Read shape-file of german postcodes

postcodes <- st_read(dsn = "./Datasets/plz-5stellig/plz-5stellig.shp")

# functions

get_mapping_data <- function(data, postcodes.shape = postcodes
                             , fill = c("plz5_kba", "plz5_kba_kraft3")) {
  
  if (any(data$jahr %in% 2017:2020)) {
    unified.postcodes <- st_union(postcodes.shape[postcodes.shape$plz %in% c("81248", "81249"), ])
    postcodes.shape[postcodes.shape$plz == "81249", "geometry"] <- st_multipolygon(unified.postcodes)
  }
  
  data[, "plz"] <- as.character(data[, "plz"])
  data <- data[, c("plz", "plz5_ew", "ort", "jahr", "plz5_hh",fill)]
  result <- left_join(x = data, y = postcodes.shape[, c("plz", "geometry")], by = "plz")
  result
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
# Map 2018 ecar registrations
plz2018.map <- get_mapping_data(data = plz2018)

# Cologne
cologne.2018 <- get_postcode_plot(data = plz2018.map, fill = "log(plz5_kba_kraft3 / plz5_ew)",
                                  city = "Köln")

# Munich
munich.2018 <- get_postcode_plot(data = plz2018.map, fill = "log(plz5_kba_kraft3 / plz5_ew)",
                                 city = "München")
# Map 2019 ecar registrations
plz2019.map <- get_mapping_data(data = plz2019)

# Cologne
cologne.2019 <- get_postcode_plot(data = plz2019.map, fill = "log(plz5_kba_kraft3 / plz5_ew)",
                                  city = "Köln")
# Munich
munich.2019 <- get_postcode_plot(data = plz2019.map, fill = "log(plz5_kba_kraft3 / plz5_ew)",
                                 city = "München")

# Map 2020 ecar registrations
plz2020.map <- get_mapping_data(data = plz2020)

# Cologne
cologne.2020 <- get_postcode_plot(data = plz2020.map, fill = "log(plz5_kba_kraft3 / plz5_ew)",
                                  city = "Köln")
# Munich
munich.2020 <- get_postcode_plot(data = plz2020.map, fill = "log(plz5_kba_kraft3 / plz5_ew)",
                                 city = "München")

# Map 2021 ecar registrations Cologne
plz2021.map <- get_mapping_data(data = plz2021)

# Cologne
cologne.2021 <- get_postcode_plot(data = plz2021.map, fill = "log(plz5_kba_kraft3 / plz5_ew)",
                                  city = "Köln")
# Munich
munich.2021 <- get_postcode_plot(data = plz2021.map, fill = "log(plz5_kba_kraft3 / plz5_ew)",
                                  city = "München")

# Create Export of maps 

# Specify which maps are exported
map.procedure <- data.frame(fill = c(rep("plz5_kba_kraft3", 10),
                       rep("plz5_kba_kraft3 / plz5_ew", 10),
                       rep("log(plz5_kba_kraft3 / plz5_ew)", 10),
                       rep("plz5_kba_kraft3 / plz5_hh", 10),
                       rep("log(plz5_kba_kraft3 / plz5_hh)", 10),
                       rep("log(plz5_kba_kraft3)", 10)),
              
              city = ifelse(1:60 %% 2 == 0, "München", "Köln"),
              year = rep(2017:2021, 6)
              ) %>% arrange(year)

# Set title for maps
map.procedure$title <- case_when(map.procedure$fill == "plz5_kba_kraft3" ~ "Anzahl E-Auto Zulassungen",
                                 map.procedure$fill == "plz5_kba_kraft3 / plz5_ew" ~ "Anzahl E-Auto Zulassungen pro Einwohner",
                                 map.procedure$fill == "plz5_kba_kraft3 / plz5_hh" ~ "Anzahl E-Auto Zulassungen pro Privathaushalt",
                                 map.procedure$fill == "log(plz5_kba_kraft3 / plz5_ew)" ~ "Logarithmierte Anzahl E-Auto Zulassungen pro Einwohner",
                                 map.procedure$fill == "log(plz5_kba_kraft3 / plz5_hh)" ~ "Logarithmierte Anzahl E-Auto Zulassungen pro Privathaushalt",
                                 map.procedure$fill == "log(plz5_kba_kraft3)" ~ "Logarithmierte Anzahl E-Auto Zulassungen"
      )
# Set subtitle for maps
map.procedure$subtitle <- paste(map.procedure$city, map.procedure$year, sep = ", ")
mapping.data <- do.call(rbind, list(plz2017.map, plz2018.map, plz2019.map, plz2020.map, plz2021.map))

# Save maps
p <- apply(X = map.procedure,
           MARGIN = 1,
       FUN = function(x) 
        get_postcode_plot(data = mapping.data[mapping.data$jahr == x["year"], ], year = x["year"]
                          , city = x["city"], title = x["title"], subtitle = x["subtitle"], fill = x["fill"])
       )

# Create export procedure

map.procedure$stadt <- ifelse(map.procedure$city == "München", "Munich", "Cologne")
map.procedure$export.name <- paste(paste(gsub(pattern = " ", replacement = "_", x = map.procedure$title),
                                   map.procedure$stadt,
                                   map.procedure$year, sep = "_"), "jpeg", sep = ".")
map.procedure$path <- paste("./Ecars", map.procedure$title, map.procedure$export.name, sep = "/") 

# Export files

#mapply(plots = p, path = map.procedure$path, FUN = function(plots, path) ggsave(filename = path, plot = plots))
