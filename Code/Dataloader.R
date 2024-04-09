# packages
library(dplyr)
library(sf)

# functions
right <- function(x, n) {
  substr(x, nchar(x) - n + 1, nchar(x))
}


read_data <- function(year) {
  #Read data
  data <- read.csv(paste("./Datasets/PLZ_",year,".csv", sep = ""), sep = ";", encoding = "latin1")
  names(data) <- tolower(names(data))
  
  #Merge covariates from 2017 
  if(year %in% as.character(2018:2020)) {
    data2017 <- read.csv("./Datasets/PLZ_2017.csv", encoding = "latin1", sep = ";")
    
    # Aggregate 81248 and 81249
    data$plz <- as.character(data$plz)
    data2017$plz <- as.character(data2017$plz)
    
    if(any(data$plz %in% "81248")) {
      data <- data %>% replace(is.na(.), 0)
      colums_to_aggregate <- c("plz5_kba", "plz5_kba_kraft3")
      data[data$plz == "81249", colums_to_aggregate] <- colSums(data[data$plz %in% c("81248", "81249"), colums_to_aggregate])
      data <- data[data$plz != "81248", ]
    }
    
    # Merge data
    result <- left_join(x = data, y = data2017[, !names(data2017) %in% c("plz5_kba_kraft3", "plz5_kba", "ort")], by = "plz")
  }
  
  else if(year %in% as.character(c(2017,2021))) {
    result <- data
  }
  result
}

# read infas360 data
plz2017 <- read_data("2017")
plz2018 <- read_data("2018")
plz2019 <- read_data("2019")
plz2020 <- read_data("2020")
plz2021 <- read_data("2021")

# Read shape-file of german postcodes
postcodes <- st_read(dsn = "./Datasets/plz-5stellig/plz-5stellig.shp")
