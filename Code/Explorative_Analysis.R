options(scipen = 100)
set.seed(2021)
# Packages
library(ggplot2)
library(dplyr)
library(assert)
library(openxlsx)
library(tidyverse)
library(ggcorrplot)
library(mgcv)
library(lme4)


# Datasets

plz2017 <- read.csv("./Datasets/PLZ_2017.csv", sep = ";", encoding = "latin1")
plz2021 <- read.csv("./Datasets/PLZ_2021.csv", sep = ";", encoding = "latin1")
df_quality_check_2017 <- read.xlsx("./Datasets/DataQualityCheck.xlsx", sheet = "2017")

# TODO: First model analysis of known predictors of ecars registrations

#' Categorized all covariates into four groups based on findings in (Wappelhorst, 2022)
columns.wappelhorst <- df_quality_check_2017$column[!is.na(df_quality_check_2017$category)]

# functions

# generates formula for glm-function
get_formula <- function(covariates, offset = NULL, target = "plz5_kba_kraft3") {
  
  if (is.null(offset)) {
    covariates <- covariates[!covariates %in% c("plz5_kba_kraft3", "plz5_kba", "Jahr")]
    result <- as.formula(paste(target, paste0(covariates, collapse = " + "), sep = " ~ "))
  }
  else if (!is.null(offset)) {
    covariates <- covariates[!covariates %in% c("plz5_kba_kraft3", "plz5_kba", "Jahr", offset)]
    result <-  as.formula(paste(target, paste(c(covariates, paste("offset(log(", offset, "))", collapse = "")), collapse = "+")
                                , sep = " ~ "))
  }
  result
}
# fit of a poisson model
poisson_fit <- function(formula, data) {
  model <- glm(formula = formula, data = data, family = "poisson")
  model
}

# Models for 2017

#' Approximation of the linear model in "Electric Vehicles for Everyone?
#' State, District and City Level Uptake Patterns in Germany"
Model_Wappelhorst <- glm(plz5_kba_kraft3 ~
                          plz5_kk_ew
                        + plz5_student
                        + plz5_basistyp1
                        + plz5_anz_firm
                        + plz5_flaeche
                        + offset(log(plz5_ew))
                          , data = plz2017, family = "poisson")
summary(Model_Wappelhorst)


# generates random sets of poisson models

model_sampler <- function(covariates = columns.wappelhorst, target = "plz5_kba_kraft3", sample.matrix, offset = "plz5_ew", data) {
  sampling.procedure <- mapply(FUN = function(x,y) rep(x,y), x = sample.matrix[, 1], y = sample.matrix[, 2])
  sampling.procedure <- as.vector(sampling.procedure)
  
  covariates_samples <- sapply(X = sampling.procedure, FUN = function(n) get_formula(covariates = sample(x = covariates, size = n, replace = FALSE)
                                                                                     , offset = offset))
  model.fits <- lapply(X = covariates_samples, FUN = function(formula) poisson_fit(formula = formula, data = data))
  model.fits
}

# First explorative analysis

# fit 30 different poisson models of different size

size.steps <- seq(from = 5, to = 30, by =  5)
sample.matrix <- cbind(size.steps, rep(5, length(size.steps)))
poisson.models <- model_sampler(sample.matrix = sample.matrix, data = plz2017)
