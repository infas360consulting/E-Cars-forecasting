options(scipen = 100)

#Packages
library(dplyr)
library(CARBayesST)
library(sf)

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



### EXAMPLE:

#columns.to.select <- columns.to.select[!columns.to.select %in% c("plz5_kba_kraft3", "plz","plz5_kba", "ort", "jahr")]
#example.covariates <- sample(columns.to.select, 20)
#
#get_variable_selection(data = ecars_variable_selection, burnin = 20000, n.sample = 100000, thin = 100, covariates = example.covariates,
#                      target = "plz5_kba_kraft3", offset.variable = "plz5_hh", W = W, seed = 2024, max.modelfits = 25)

### Final Variable Selection

#selection.result <- get_variable_selection(data = ecars_variable_selection, burnin = 20000, n.sample = 100000, thin = 100, covariates = columns.to.select,
#                       target = "plz5_kba_kraft3", offset.variable = "plz5_hh", W = W, seed = 2024, max.modelfits = 1000)
#

selected.model <- as.formula(plz5_kba_kraft3 ~ 1 + offset(log(plz5_hh)) + plz5_selbst + plz5_kk_ew + 
 plz5_solar + plz5_ew_m + plz5_anz_firm_g + plz5_hh_eink_kl4 + 
  plz5_str_typ1 + plz5_wfl_kl2 + plz5_grundfl_kl5 + plz5_eauto_score2)

#M <- ST.CARar(formula = selection.result, family = "poisson", data = ecars_variable_selection, W = W, burnin = 20000, n.sample = 100000, thin = 100, AR = 1)
#coef(M)
#M

M2 <- ST.CARar(formula = selected.model, family = "poisson", data = ecars_variable_selection, W = W, burnin = 20000, n.sample = 100000, thin = 100, AR = 1)
M2

M3 <- ST.CARadaptive(formula = selected.model, family = "poisson", data = ecars, burnin = 20000, n.sample = 100000, thin = 100, W = W)
coef(M3)

M4 <- ST.CARar(formula = plz5_kba_kraft3 ~ plz5_solar, family = "poisson", data = ecars_M, burnin = 20000, n.sample = 100000, thin = 100, W = W_M, AR = 1)
coef(M4)
M4

M5 <- glm(formula = get_updated_formula(selected.model, "ort"), family = "poisson", data = ecars)
summary(M5)
coef(M5)

M6 <- ST.CARar(formula = get_updated_formula(selected.model, "ort"), family = "poisson", data = ecars_variable_selection, W = W, burnin = 20000, n.sample = 100000, thin = 100, AR = 1)
M6

coef(M5)
coef(M6)
