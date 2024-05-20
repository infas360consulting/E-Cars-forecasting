options(scipen = 1000)
set.seed(2024)
#Packages
library(dplyr)
library(CARBayesST)
library(sf)
library(lme4)
library(matrixStats)
library(bayestestR)

#Datasets
source("./Code/Dataloader.R") # read infas data, german postcode shape file, columns to choose from

#functions 
source("./Code/utils.R") # read auxiliary functions

#Variable Selection 
ecars_variable_selection <- ecars[ecars$jahr %in% c(2017:2019), ]

# neighbour matrix W
W <- st_intersects(postcodes, sparse = FALSE, )
W <- apply(X = W, MARGIN = 1, FUN = function(x) as.numeric(x))
diag(W) <- 0
colnames(W) <- postcodes$plz
rownames(W) <- postcodes$plz

# Neighbor Matrix Munich
W_M <- W[which(substr(rownames(W), 1,1) == "8"), which(substr(rownames(W), 1,1) == "8")]
ecars_M <- ecars[ecars$ort == "München", ]

# Neighbor Matrix Cologone
W_K <- W[which(substr(rownames(W), 1,1) == "5"), which(substr(rownames(W), 1,1) == "5")]
ecars_K <- ecars[ecars$ort == "Köln", ]

### Variable Selection with Generalized Mixed Model

get_variable_selection2 <- function(data, covariates, target, offset.variable, seed, max.modelfits) {
  #Replicability
  set.seed(seed = seed)
  # Remove potential flawed covariates
  covariates <- covariates[!covariates %in% c(target, offset.variable, "jahr", "plz5_kba", "plz")]
  unique.length.covariates <- apply(X = data[, covariates], MARGIN = 2, FUN = function(x) length(unique(x)))
  covariates <- covariates[unique.length.covariates > 1]
  # Scale numeric predictors to ensure convergence
  columns.to.scale <- intersect(covariates, names(data)[sapply(data, is.numeric)])
  data[, columns.to.scale] <- data.frame(scale(data[, columns.to.scale], scale = TRUE, center = TRUE))
  # Setting empty model
  empty.model.formula <- get_formula(covariates = "1 + (1|jahr) + (1|plz)", offset = offset.variable)
  # Setting iterative selection
  current.model.formula <- empty.model.formula
  #selection_trajectory <- data.frame(model.formula = as.character(current.model.formula), model.fit = model.fit[which.min(model.fit)])
  columns.to.choose.from <- covariates
  current.model.fit <- Inf
  is.continiue <- TRUE
  #Create fitting function for apply loop
  model <- function(model.formula, data) {
    glmer(formula = model.formula, data = data, family = "poisson", nAGQ = 1)
  }
  while(is.continiue) {
    formulas.to.try <- sapply(X = columns.to.choose.from, FUN = function(x) get_updated_formula(model.formula = current.model.formula, cov.update = x))
    temp.model.fit <- sapply(X = formulas.to.try, FUN = function(x) AIC(model(model.formula = x, data = data)))
    
    if(min(temp.model.fit) < current.model.fit) {
      current.model.formula <- formulas.to.try[which.min(temp.model.fit)][[1]]
      added.variable <- columns.to.choose.from[which.min(temp.model.fit)]
      columns.to.choose.from <- drop_correlated_variable(covariates = columns.to.choose.from, y = added.variable, data = data)
      max.modelfits <- max.modelfits - length(unique(columns.to.choose.from))
      if(length(columns.to.choose.from) < 1 | max.modelfits < 0) {
        is.continiue <- FALSE
      }
      
    }
    
    else {
      is.continiue <- FALSE
    }
    
  }
  list(formula= update.formula(current.model.formula, ~. - (1|plz) - (1|jahr)), remaining.modelfits = max.modelfits, remaining.columns = columns.to.choose.from, model = model(model.formula = current.model.formula, data = data))
}

### Selection results
training <- as.character(2017:2019)

## Cologne
#Training
selected.model.cologne <- get_variable_selection2(data = ecars_K[ecars_K$jahr %in% training, ], covariates = columns.to.select, target = "plz5_kba_kraft3", offset.variable = "plz5_ew", seed = 2024, max.modelfits = 600)
ST.Cologne <- ST.CARar(formula = selected.model.cologne$formula, family = "poisson", data = ecars_K, W = W_K, burnin = 20000, n.sample = 300000, thin = 100, AR = 1, n.chains = 10)

#Forecasting
ecars_K_forecasting <- ecars_K[ecars_K$jahr %in% as.character(2019:2021), ]
to.forecast <- as.character(2021)
is.forecast <- ecars_K_forecasting$jahr %in% to.forecast
ecars_K_forecasting$forecast <- ST.CARar.Predict(is.forecast = is.forecast, formula = selected.model.cologne, data = ecars_K_forecasting, W = W_K, burnin = 20000, n.sample = 500000, thin = 100, n.chains = 100, seed = 2024)
ecars_K_forecasting <- ecars_K_forecasting %>% relocate("forecast", .after = "plz5_kba_kraft3") %>% relocate(c("plz5_ladesaeulen", "plz5_ew_18u30_w", "plz5_firm_ums_kl9", "plz5_wfl_kl1"), .after = "forecast")

#Performance
cor(ecars_K_forecasting[is.forecast, c("plz5_kba_kraft3", "forecast")])

## Munich
selected.model.munich <- get_variable_selection2(data = ecars_M[ecars_M$jahr %in% training, ], covariates = columns.to.select, target = "plz5_kba_kraft3", offset.variable = "plz5_ew", seed = 2024, max.modelfits = 700)
ST.Munich <- ST.CARar(formula = selected.model.munich$formula, family = "poisson", data = ecars_M, W = W_M, burnin = 20000, n.sample = 300000, thin = 100, AR = 1, n.chains = 30)
