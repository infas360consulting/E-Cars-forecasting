# packages
library(dplyr)
library(sf)
library(openxlsx)

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
    names(data2017) <- tolower(names(data2017))
    
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
    result <- left_join(x = data, y = data2017[, !names(data2017) %in% c("plz5_kba_kraft3", "plz5_kba", "ort", "jahr")], by = "plz")
  }
  
  else if(year == as.character(2021)) {
    columns_not_aggregate <- tolower(c("PLZ", "ORT", "Jahr", "PLZ5_REL1_KL_BRD", "PLZ5_REL2_KL_BRD",
                                     "PLZ5_REL3_KL_BRD", "PLZ5_REL1_KL", "PLZ5_REL2_KL", "PLZ5_REL3_KL",
                                     "PLZ5_KK_EW_KL", "PLZ5_KK_HH_KL", "PLZ5_OPNV_IDX_KL", "PLZ5_NACHFRAGE_KL"
                                     , "PLZ5_EW_DICHTE_KL", "PLZ5_BDICHTE_KL", "PLZ5_STRUKTUR_KL", "PLZ5_SEKTOR_KL"))
    
    data[data$plz == "81249", !names(data) %in% columns_not_aggregate] <- colSums(data[data$plz %in% c("81249", "81248"), !names(data) %in% columns_not_aggregate])
    data <- data[data$plz != "81248", ]
    result <- data
  }
  
  else {
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

# create spatio-temporal dataset
df_quality_check_2017 <- read.xlsx("./Datasets/DataQualityCheck.xlsx", sheet = "2017")
columns.wappelhorst <- df_quality_check_2017$column[!is.na(df_quality_check_2017$category)]
columns.to.select <- c("plz5_kba_kraft3","plz","plz5_kba", "ort", "jahr", columns.wappelhorst)
columns.to.select <- intersect(columns.to.select, intersect(names(plz2017), names(plz2021)))
ecars <- do.call(what = rbind, args = list(plz2017[, columns.to.select],
                                           plz2018[, columns.to.select],
                                           plz2019[, columns.to.select],
                                           plz2020[, columns.to.select],
                                           plz2021[, columns.to.select]))
ecars <- ecars[order(ecars$jahr, ecars$plz), ]

# Read shape-file of german postcodes
postcodes <- st_read(dsn = "./Datasets/plz-5stellig/plz-5stellig.shp")
unified.postcodes <- st_union(postcodes[postcodes$plz %in% c("81248", "81249"), ])
postcodes[postcodes$plz == "81249", "geometry"] <- st_multipolygon(unified.postcodes)
postcodes <- postcodes[postcodes$plz %in% ecars$plz, ]
postcodes <- postcodes[order(postcodes$plz), ]

#remove 
remove(unified.postcodes)
remove(df_quality_check_2017)
