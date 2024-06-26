# packages
library(dplyr)
library(sf)
library(openxlsx)
library(spind)

# functions
right <- function(x, n) {
  substr(x, nchar(x) - n + 1, nchar(x))
}

read_data <- function(year) {
  #Read data
  data <- read.csv(paste("./Datasets/PLZ_",year,".csv", sep = ""), sep = ";", encoding = "latin1")
  names(data) <- tolower(names(data))
  # Charging infrastructure 
  # TODO: Ilija Anpassung senden Excel-Datei
  charging_infrastructure <- read.xlsx("./Datasets/Ladesaeulenregister.xlsx", startRow = 11, detectDates = TRUE)
  names(charging_infrastructure) <- tolower(names(charging_infrastructure))
  charging_infrastructure$jahr <- as.numeric(substr(charging_infrastructure$datum, 7, 10))
  chargingcolumn <- charging_infrastructure[charging_infrastructure$jahr <= as.numeric(year), ] %>% group_by(postleitzahl) %>% tally(name = "plz5_ladesaeulen")
  chargingpoint <- charging_infrastructure[charging_infrastructure$jahr <= as.numeric(year), ] %>% group_by(postleitzahl) %>% summarise(plz5_ladepunkte = sum(anzahl.ladepunkte))
  chargingpoints <- left_join(chargingcolumn, chargingpoint, c("postleitzahl"))                                                                                                                                      
                                                                                                                                                           
  data$plz <- as.character(data$plz)
  data <- left_join(data, chargingpoints, by = c("plz" = "postleitzahl"))
  data$plz5_ladepunkte <- ifelse(is.na(data$plz5_ladepunkte), 0, data$plz5_ladepunkte)
  data$plz5_ladesaeulen <- ifelse(is.na(data$plz5_ladesaeulen), 0, data$plz5_ladesaeulen)
  

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
  columns_not_aggregate <- tolower(c("PLZ", "ORT", "Jahr", "PLZ5_REL1_KL_BRD", "PLZ5_REL2_KL_BRD",
                                     "PLZ5_REL3_KL_BRD", "PLZ5_REL1_KL", "PLZ5_REL2_KL", "PLZ5_REL3_KL",
                                     "PLZ5_KK_EW_KL", "PLZ5_KK_HH_KL", "PLZ5_OPNV_IDX_KL", "PLZ5_NACHFRAGE_KL"
                                     , "PLZ5_EW_DICHTE_KL", "PLZ5_BDICHTE_KL", "PLZ5_STRUKTUR_KL", "PLZ5_SEKTOR_KL"))
  
  columns.numeric <- names(result)[!names(result) %in% columns_not_aggregate]
  result[, columns.numeric] <- data.frame(apply(X = result[, columns.numeric], MARGIN = 2, FUN = function(x) as.numeric(x)))
  result[, columns.numeric] <- data.frame(apply(X = result[, columns.numeric], MARGIN = 2, FUN = function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x)))
  result[, intersect(names(result), columns_not_aggregate)] <- data.frame(apply(X = result[, intersect(names(result), columns_not_aggregate)], MARGIN = 2, FUN = function(x) ifelse(is.na(x), modus(x), x)))
  if(year == as.character(2021)) {
    result[result$plz == "81249", columns.numeric] <- colSums(result[result$plz %in% c("81249", "81248"), columns.numeric])
    result[result$plz != "81248", ]
    names(result)[names(result) %in% tolower(c("PLZ5_GEBTYP_PRIV1", "PLZ5_GEBTYP_PRIV8"))] <- tolower(c("PLZ5_BASISTYP1", "PLZ5_BASISTYP6"))
  }
  result <- result %>% arrange(desc(plz))
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
columns.to.select <- c("plz5_kba_kraft3","plz","plz5_kba", "ort", "jahr","plz5_ladepunkte", "plz5_ladesaeulen", "plz5_flaeche", "plz5_basistyp1", "plz5_basistyp6", columns.wappelhorst)
columns.to.select <- intersect(columns.to.select, intersect(names(plz2017), names(plz2021)))
ecars <- do.call(what = rbind, args = list(plz2017[, columns.to.select],
                                           plz2018[, columns.to.select],
                                           plz2019[, columns.to.select],
                                           plz2020[, columns.to.select],
                                           plz2021[, columns.to.select]))
ecars <- ecars[order(ecars$jahr, ecars$plz), ]
ecars_outlier <- ecars %>% group_by(ort, jahr) %>% summarise(median = median(plz5_kba_kraft3 / plz5_ew),
                                            lower.quartil = quantile(plz5_kba_kraft3 / plz5_ew, 0.25),
                                            upper.quartil = quantile(plz5_kba_kraft3 / plz5_ew, 0.75),
                                            interquartil.distance = 1.50 * (upper.quartil - lower.quartil),
                                            outlier.treshold = upper.quartil + interquartil.distance)

ecars <- left_join(ecars, ecars_outlier, c("ort", "jahr"))
ecars <- ecars %>% mutate(is.outlier = ifelse(plz5_kba_kraft3 / plz5_ew > outlier.treshold, TRUE, FALSE),
                          plz5_ew_18u30_w_ant = plz5_ew_18u30_w * 100/ plz5_ew)
ecars <- ecars %>% relocate(c("plz","ort", "jahr", "plz5_kba_kraft3", "plz5_ew", "is.outlier"))

columns.to.select <- c(columns.to.select, "plz5_ew_18u30_w_ant")

# Ecars arima data
ecars$jahr <- as.numeric(ecars$jahr)
ecars_arima <- ecars[ecars$jahr > min(ecars$jahr), ]
ecars_arima$vorjahr <- as.character(ecars_arima$jahr - 1)
ecars$jahr <- as.character(ecars$jahr)
ecars_arima <- left_join(ecars_arima, ecars[, c("plz", "plz5_kba_kraft3", "jahr")], by = c("plz"="plz", "vorjahr"="jahr"), suffix = c("","_vorjahr"))
ecars_arima$plz5_kba_kraft3_diff <- ecars_arima$plz5_kba_kraft3 - ecars_arima$plz5_kba_kraft3_vorjahr
ecars_arima <- ecars_arima %>% relocate(c("plz5_kba_kraft3_vorjahr","plz5_kba_kraft3", "plz5_kba_kraft3_diff"), .after = "ort")


# Read shape-file of german postcodes
postcodes <- st_read(dsn = "./Datasets/plz-5stellig/plz-5stellig.shp")
unified.postcodes <- st_union(postcodes[postcodes$plz %in% c("81248", "81249"), ])
postcodes[postcodes$plz == "81249", "geometry"] <- st_multipolygon(unified.postcodes)
postcodes <- postcodes[postcodes$plz %in% ecars$plz, ]
postcodes <- postcodes[order(postcodes$plz), ]

#remove 
remove(unified.postcodes)
remove(df_quality_check_2017)
