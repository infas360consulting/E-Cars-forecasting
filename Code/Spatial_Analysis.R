options(scipen = 100)
set.seed(2021)
# Packages
library(ggplot2)
library(dplyr)
library(assert)
library(openxlsx)
library(tidyverse)
library(CARBayes)
library(bamlss)

# Datasets
source("./Code/Dataloader.R") # read infas data
source("./Code/Explorative_Analysis.R")

# Sort dataset order
plz2021 <- plz2021 %>% arrange(desc(plz))
plz2017 <- plz2017 %>% arrange(desc(plz))
postcodes <- postcodes %>% arrange(desc(plz))

# neighbour matrix for postcodes in 2017
neighbour.matrix17 <- st_touches(postcodes[postcodes$plz %in% plz2017$plz, ], sparse = FALSE)
neighbour.matrix17 <- apply(X = neighbour.matrix17, MARGIN = 1, FUN = function(x) as.numeric(x))
colnames(neighbour.matrix17) <- plz2017$plz
rownames(neighbour.matrix17) <- plz2017$plz

#neighbour matrix for postcodes in 2021
neighbour.matrix21 <- st_touches(postcodes[postcodes$plz %in% plz2021$plz, ], sparse = FALSE)
neighbour.matrix21 <- apply(X = neighbour.matrix21, MARGIN = 1, FUN = function(x) as.numeric(x))
colnames(neighbour.matrix21) <- plz2021$plz
rownames(neighbour.matrix21) <- plz2021$plz

# Fit Mcar model 21
MCar21 <- CARBayes::S.CARleroux(formula = get_formula(covariates = "plz5_solar", offset = "plz5_ew")
                        , family = "poisson", data = plz2021, W = neighbour.matrix21,
                      burnin = 10, n.sample = 1000, rho = 1)

MCar17 <- CARBayes::S.CARleroux(formula = get_formula(covariates = columns.wappelhorst[1:5], offset = "plz5_ew")
                                , family = "poisson", data = plz2017, W = neighbour.matrix17,
                                burnin = 10, n.sample = 1000, rho = 1)


