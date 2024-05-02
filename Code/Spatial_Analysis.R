options(scipen = 100)
#packages
library(sp)
library(CARBayesST)

#Datasets
source("./Code/Dataloader.R") # read infas data, german postcode shape file

#functions 
source("./Code/utils.R") # read auxiliary functions

# neighbour matrix W
W <- st_intersects(postcodes, sparse = FALSE, )
W <- apply(X = W, MARGIN = 1, FUN = function(x) as.numeric(x))
diag(W) <- 0
colnames(W) <- postcodes$plz
rownames(W) <- postcodes$plz

# Model
model.formula <- get_formula(covariates = c("plz5_solar", "plz5_miet", "plz5_garage", "plz5_eg_ant", "plz5_basistyp2"), offset = "plz5_hh", target = "plz5_kba_kraft3")
Model1 <- ST.CARadaptive(formula = model.formula,
                         family = "poisson", data = ecars, W = W, burnin = 2000, n.sample = 220000, thin = 100)

Model2 <- ST.CARlinear(formula = model.formula, family = "poisson", data = ecars, W = neighbour.matrix, n.sample = 2200000, burnin = 2000, thin = 100)


