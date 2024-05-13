options(scipen = 1000)
set.seed(2024)
#Packages
library(dplyr)
library(CARBayesST)
library(sf)
library(lme4)

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

#Test for Munich Model
W_M <- W[which(substr(rownames(W), 1,1) == "8"), which(substr(rownames(W), 1,1) == "8")]
ecars_M <- ecars[ecars$ort == "München", ]

### Variable Selection with spatio-temporal Bayes Model

get_variable_selection <- function(data, burnin, n.sample, thin, covariates, target, offset.variable, W, seed, max.modelfits) {
#Replicability
set.seed(seed = seed)
# Remove potential flawed covariates
covariates <- covariates[!covariates %in% c(target, offset.variable, "jahr", "ort", "plz5_kba", "plz")]
unique.length.covariates <- apply(X = data[, covariates], MARGIN = 2, FUN = function(x) length(unique(x)))
covariates <- covariates[unique.length.covariates > 1]

# Selection of spatio-temporal model for variable selection based empty model
empty.model.formula <- get_formula(covariates = "1", offset = offset.variable)

# ST.CARar()
Model1 <- ST.CARar(formula = empty.model.formula, family = "poisson", data = data, W = W
                   , burnin = burnin, n.sample = n.sample, thin = thin, AR = 1)

#ST.CARlinear()
Model2 <- ST.CARlinear(formula = empty.model.formula, family = "poisson", data = data, W = W,
                       burnin = burnin, n.sample = n.sample, thin = thin)

#ST.CARanova()
Model3 <- ST.CARanova(formula = empty.model.formula, family = "poisson", data = data, W = W,
                      burnin = burnin, n.sample = n.sample, thin = thin, interaction = TRUE)

# Select best empty model
models <- c("ST.CARar", "ST.CARlinear", "ST.CARanova")
model.fit <- c(Model1$modelfit["DIC"], Model2$modelfit["DIC"], Model3$modelfit["DIC"])
selected.model <- models[which.min(model.fit)]

# Setting iterative selection
current.model.formula <- empty.model.formula
#selection_trajectory <- data.frame(model.formula = as.character(current.model.formula), model.fit = model.fit[which.min(model.fit)])
columns.to.choose.from <- covariates
current.model.fit <- model.fit[which.min(model.fit)]
is.continiue <- TRUE

if(selected.model == "ST.CARar") {
  model <- function(model.formula, data = data, W = W, burnin = burnin, n.sample = n.sample, thin = thin) {
    ST.CARar(formula = model.formula, family = "poisson", data = data, W=W, burnin = burnin, n.sample = n.sample, thin = thin, AR = 1)
  }

  while(is.continiue) {
    formulas.to.try <- sapply(X = columns.to.choose.from, FUN = function(x) get_updated_formula(model.formula = current.model.formula, cov.update = x))
    temp.model.fit <- sapply(X = formulas.to.try, FUN = function(x) model(model.formula = x, data = data, W = W, burnin = burnin, n.sample = n.sample, thin = thin)$modelfit["DIC"])
    
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

}

else if(selected.model == "ST.CARlinear") {
  model <- function(model.formula, data = data, W = W, burnin = burnin, n.sample = n.sample, thin = thin) {
    ST.CARlinear(formula = model.formula, family = "poisson", data = data, W = W, burnin = burnin, n.sample = n.sample, thin = thin)
  }

  while(is.continiue) {
    formulas.to.try <- sapply(X = columns.to.choose.from, FUN = function(x) get_updated_formula(model.formula = current.model.formula, cov.update = x))
    temp.model.fit <- sapply(X = formulas.to.try, FUN = function(x) model(model.formula = x, family = "poisson", data = data, W = W, burnin = burnin, n.sample = n.sample, thin = thin)$modelfit["DIC"])
      
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

}

else if(selected.model == "ST.CARanova") {
  model <- function(model.formula, data = data, W = W, burnin = burnin, n.sample = n.sample, thin = thin) {
  ST.CARanova(formula = model.formula, family = "poisson", data = data, W = W, burnin = burnin, n.sample = n.sample, thin = thin, interaction = TRUE)
}

  while(is.continiue) {
    formulas.to.try <- sapply(X = columns.to.choose.from, FUN = function(x) get_updated_formula(model.formula = current.model.formula, cov.update = x))
    temp.model.fit <- sapply(X = formulas.to.try, FUN = function(x) model(model.formula= x, data = data, W = W, burnin = burnin, n.sample = n.sample, thin = thin)$modelfit["DIC"])
    
    if(min(temp.model.fit) < current.model.fit) {
      current.model.formula <- formulas.to.try[which.min(temp.model.fit)][[1]]
      added.variable <- columns.to.choose.from[which.min(temp.model.fit)]
      columns.to.choose.from <- drop_correlated_variable(covariates = columns.to.choose.from, y = added.variable, data = data)
      current.model.fit <- min(temp.model.fit)
      max.modelfits <- max.modelfits - length(unique(columns.to.choose.from))
      if(length(columns.to.choose.from) < 1 | max.modelfits < 0) {
          is.continiue <- FALSE
        }
    }
    
    else {
      is.continiue <- FALSE
    }
    
  }

}

else {
  print("No Model selected")
  stop()
}
current.model.formula
}

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
    glmer(formula = model.formula, data = data, family = "poisson", nAGQ = 0)
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


### EXAMPLE:

#columns.to.select <- columns.to.select[!columns.to.select %in% c("plz5_kba_kraft3", "plz","plz5_kba", "ort", "jahr")]
#example.covariates <- sample(columns.to.select, 20)
#
#get_variable_selection(data = ecars_variable_selection, burnin = 20000, n.sample = 100000, thin = 100, covariates = example.covariates,
#                      target = "plz5_kba_kraft3", offset.variable = "plz5_hh", W = W, seed = 2024, max.modelfits = 25)

### Final Variable Selection

#selection.result <- get_variable_selection(data = ecars_variable_selection, burnin = 20000, n.sample = 100000, thin = 100, covariates = columns.to.select,
#                       target = "plz5_kba_kraft3", offset.variable = "plz5_ew", W = W, seed = 2024, max.modelfits = 1000)


#selected.model <- as.formula(plz5_kba_kraft3 ~ 1 + offset(log(plz5_hh)) + plz5_selbst + plz5_kk_ew + 
# plz5_solar + plz5_ew_m + plz5_anz_firm_g + plz5_hh_eink_kl4 + 
#  plz5_str_typ1 + plz5_wfl_kl2 + plz5_grundfl_kl5 + plz5_eauto_score2)

#M <- ST.CARar(formula = selection.result, family = "poisson", data = ecars_variable_selection, W = W, burnin = 20000, n.sample = 100000, thin = 100, AR = 1)
#coef(M)
#M
set.seed(2024)
selected.model <- get_variable_selection2(data = ecars_variable_selection, target = "plz5_kba_kraft3", covariates = columns.to.select, offset.variable = "plz5_ew", seed = 2024, max.modelfits = 1000)
M2 <- ST.CARar(formula = selected.model$formula, family = "poisson", data = ecars_variable_selection, W = W, burnin = 20000, n.sample = 100000, thin = 100, AR = 1, n.chains = 50)
M2

#M3 <- ST.CARar(formula = selected.model$formula, family = "poisson", data = ecars_variable_selection, W = W, burnin = 20000, n.sample = 100000, thin = 100, AR = 1, n.chains = 50)
#M3
#coef(M2)

## Use Model to predict future ecar registrations
set.seed(2024)
new_data <- ecars
new_data$plz5_kba_kraft3 <- ifelse(new_data$jahr %in% c("2021"), NA, new_data$plz5_kba_kraft3)

M2_Pred <- ST.CARar(formula = selected.model$formula, family = "poisson", data = new_data, W = W, burnin = 20000, n.sample = 100000, thin = 100, n.chains = 5, AR = 1)
Result <- new_data[, c("plz", "jahr", "plz5_kba_kraft3")]
Result$fitted_values <- M2_Pred$fitted.values
Result$plz5_kba_kraft3[Result$jahr == "2021"] <- ecars[ecars$jahr == "2021", "plz5_kba_kraft3"]
Result$fitted_values[Result$jahr == "2021"] <- rowMeans(sapply(seq_len(5), FUN = function(x) colMeans(M2_Pred$samples$Y[[x]])))
#Result$diff <- abs(Result$fitted_values - Result$plz5_kba_kraft3)
#Result$prop <- (Result$fitted_values / (Result$plz5_kba_kraft3)) - 1
#Problem25 <- Result$plz[Result$plausible > 2 & Result$jahr == "2021"]
Result$plausible <- Result$fitted_values / Result$plz5_kba_kraft3
Problem24 <-  Result$plz[Result$plausible > 2 & Result$jahr == "2021"]


## München Modell
munich_vs <- ecars[ecars$ort == "München" & ecars$jahr %in% as.character(2017:2020), ]
munich.model <- get_variable_selection2(data = munich_vs, target = "plz5_kba_kraft3", covariates = columns.to.select, offset.variable = "plz5_ew", seed = 2024, max.modelfits = 700)

#Neighbour Matrix
W_M <- W[which(substr(rownames(W), 1,1) == "8"), which(substr(rownames(W), 1,1) == "8")]
ecars_M <- ecars[ecars$ort == "München", ]

#SpatioTemporal Model
MunichST <- ST.CARar(formula = munich.model$formula, family = "poisson", data = munich_vs, W = W_M, burnin = 20000, n.sample = 100000, thin = 100, n.chains = 5, AR = 1)
MunichST
#Forecasting

munich_forecast <- ecars[ecars$ort == "München", ]
munich_forecast$plz5_kba_kraft3 <- ifelse(munich_forecast$jahr == "2021", NA, munich_forecast$plz5_kba_kraft3)

# Model
MunichForecastM <- ST.CARar(formula = munich.model$formula, family = "poisson", data = munich_forecast, W = W_M, burnin = 20000, n.sample = 100000, thin = 100, AR = 1)

Result <- data.frame(plz = munich_forecast$plz[munich_forecast$jahr == 2021], plz5_kba_kraft3 = ecars$plz5_kba_kraft3[ecars$jahr == 2021 & ecars$ort == "München"], prediction = colMeans(MunichForecastM$samples$Y))

### Use Generalized Mixed Linear Model with fixed time effect
TestData <- ecars[ecars$jahr %in% as.character(2020:2021), ]
covariates.numeric <- names(TestData)[sapply(TestData, is.numeric)]
covariates.numeric <- covariates.numeric[!covariates.numeric %in% c("plz", "jahr", "plz5_kba", "plz5_kba_kraft3", "plz5_hh")]
TestData[, covariates.numeric] <- scale(TestData[, covariates.numeric], center = TRUE, scale = TRUE)

Result <- TestData[, c("plz", "jahr", "plz5_kba_kraft3")]
Result$prediction <- predict(selected.model$model, newdata = TestData, type = "response")


