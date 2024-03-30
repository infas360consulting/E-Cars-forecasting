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
df_quality_check_2021 <- read.xlsx("./Datasets/DataQualityCheck.xlsx", sheet = "2021")

# TODO: First model analysis of known predictors of ecars registrations

#' Categorized all covariates into four groups based on findings in (Wappelhorst, 2022)
columns.wappelhorst <- df_quality_check_2017$column[!is.na(df_quality_check_2017$category)]

# functions

# generates formula for glm-function
get_formula <- function(covariates, offset = NULL, target = "plz5_kba_kraft3") {
  
  if (is.null(offset)) {
    covariates <- tolower(covariates[!covariates %in% c("plz5_kba_kraft3", "plz5_kba", "Jahr")])
    result <- as.formula(paste(target, paste0(covariates, collapse = " + "), sep = " ~ "))
  }
  else if (!is.null(offset)) {
    covariates <- tolower(covariates[!covariates %in% c("plz5_kba_kraft3", "plz5_kba", "Jahr", offset)])
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

# generates random sets of poisson models

model_sampler <- function(covariates = columns.wappelhorst, target = "plz5_kba_kraft3", sample.matrix, offset = "plz5_ew", data) {
  sampling.procedure <- mapply(FUN = function(x,y) rep(x,y), x = sample.matrix[, 1], y = sample.matrix[, 2])
  sampling.procedure <- as.vector(sampling.procedure)
  
  covariates_samples <- sapply(X = sampling.procedure, FUN = function(n) get_formula(covariates = sample(x = covariates, size = n, replace = FALSE)
                                                                                     , offset = offset))
  model.fits <- lapply(X = covariates_samples, FUN = function(formula) poisson_fit(formula = formula, data = data))
  model.fits
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


# First explorative analysis

# fit 30 different poisson models of different size
model.size.start <- 5
model.size.end <- 30
model.size.steps <- 5
size.steps <- seq(from = model.size.start, to = model.size.end, by =  model.size.steps)
sample.matrix <- cbind(size.steps, rep(5, length(size.steps)))
poisson.models <- model_sampler(sample.matrix = sample.matrix, data = plz2017)

# Models for 2021

covariates2021 <- intersect(columns.wappelhorst, names(plz2021))
setdiff(columns.wappelhorst, names(plz2021))

# Basistyp was changed to Gebäudewohntypologie

covariates2021 <- c(covariates2021, intersect(paste("plz5_gebpretyp", 1:100, sep = ""), names(plz2021)))
covariates2021 <- c(covariates2021, intersect(paste("plz5_gebtyp_priv", 1:100, sep = ""), names(plz2021)))

# fit 30 different poisson models of different size
model.size.start <- 5
model.size.end <- 30
model.size.steps <- 5
size.steps <- seq(from = model.size.start, to = model.size.end, by =  model.size.steps)
sample.matrix <- cbind(size.steps, rep(5, length(size.steps)))
poisson.models <- model_sampler(sample.matrix = sample.matrix, data = plz2021, covariates = covariates2021)

library(tidyverse)
library(glmnet)
library(reshape2)

# importing the data
data_21 = read.csv2("/Users/ilijaspasojevic/Desktop/infas360_PLZ_Daten_15022024/PLZ_2021.csv", encoding = "latin1")
data_17 = read.csv2("/Users/ilijaspasojevic/Desktop/infas360_PLZ_Daten_15022024/PLZ_2017.csv", encoding = "latin1")

data_21$plz5_miet = as.numeric(data_21$plz5_miet)
data_21$plz5_eg_ant = as.numeric(data_21$plz5_eg_ant)

# defining data frames with several numbers of covariables 
# small small one has most important one then medium hast most important and important one and the large one 
data_21_small = data_21 %>% 
  select(c("plz5_kba_kraft3", "ort", "plz5_n3","plz5_wfl", "plz5_garage", "plz5_wh_preis_idx", "plz5_dist_opnv", "plz5_ew", "plz5_hh", 
           "plz5_absol_hoch", "plz5_kk_ew", "plz5_anz_firm", "plz5_opnv_idx", "plz5_ew_dichte_kl", "plz5_kauf", "plz5_miet"))
# data_21_small$plz5_miet = as.numeric(data_21_small$plz5_miet)

data_21_medium = data_21 %>% 
  select(c(colnames(data_21_small), "plz5_adr", "plz5_hh_dinks", "plz5_absol_fach", "plz5_kk_hh", "plz5_eg_ant", 
           "plz5_kk_vol", "plz5_solar"))
# data_21_medium$plz5_miet = as.numeric(data_21_medium$plz5_miet)
# data_21_medium$plz5_eg_ant = as.numeric(data_21_medium$plz5_eg_ant)

# nehme lage schlecht raus da sonst linear abhaengig der Datensatz da lage gut +. mittel + schlecht = anzahl adressen 
data_21_large = data_21 %>%
  select(starts_with("plz5_wolage") | plz5_ew_m | starts_with("plz5_firm_ums")) %>%
  bind_cols(data_21_medium) %>%
  mutate(plz5_ew_m_ratio = plz5_ew_m / plz5_ew) %>%
  select(-plz5_ew_m) %>% 
  mutate(plz5_lage_gut = plz5_wolage1 + plz5_wolage2 + plz5_wolage3, 
         plz5_lage_mittel = plz5_wolage4 + plz5_wolage5 + plz5_wolage6) %>%
  select(- starts_with("plz5_wolage")) %>% 
  mutate(plz5_firma_klein = plz5_firm_ums_kl1 + plz5_firm_ums_kl2 + plz5_firm_ums_kl3, 
         plz5_firma_mittel = plz5_firm_ums_kl4 + plz5_firm_ums_kl5 + plz5_firm_ums_kl6, 
         plz5_firma_groß = plz5_firm_ums_kl7 + plz5_firm_ums_kl8 + plz5_firm_ums_kl9 + plz5_firm_ums_kl10) %>% 
  select(- starts_with("plz5_firm_ums"))

# ersetzte NA's mit dem mean 
data_21_large = data_21_large %>%
  mutate(plz5_firma_klein = ifelse(is.na(plz5_firma_klein), mean(plz5_firma_klein, na.rm = TRUE), plz5_firma_klein)) %>%
  mutate(plz5_firma_mittel = ifelse(is.na(plz5_firma_mittel), mean(plz5_firma_mittel, na.rm = TRUE), plz5_firma_mittel)) %>%
  mutate(plz5_firma_groß = ifelse(is.na(plz5_firma_groß), mean(plz5_firma_groß, na.rm = TRUE), plz5_firma_groß)) 

# fitting now possion models for the different data sets 
data_list = list(data_21_small, data_21_medium, data_21_large)
model_list = vector(mode = "list", length = 3)
performance_matrix_2021 = matrix(ncol = 2, nrow = 3)
colnames(performance_matrix_2021) = c("AIC", "BIC")
rownames(performance_matrix_2021) = c("small", "medium", "large")
for (i in 1:3) {
  fit = glm(plz5_kba_kraft3 ~. , data = data_list[[i]], family = poisson())
  model_list[[i]] = fit 
  performance_matrix_2021[i,1] = AIC(fit)
  performance_matrix_2021[i,2] = BIC(fit)
}

performance_matrix_2021
# Nach AIC und BIC ist small am besten dann medium und performance von large am schelchtesten 

# Dies wart lediglich der Anfang rein über Intuation jeztzt veruche es mit Lasso etc zuselecktieren 
model_lasso = glmnet(x = as.matrix(data_21_large[,-1]), y= data_21_large$plz5_kba_kraft3, alpha = 1) 
sum_lasso = summary(model_lasso)
lambda_lasso = cv.glmnet(x = as.matrix(data_21_large[, -1]), 
                         y = data_21_large$plz5_kba_kraft3)
coef(model_lasso, s = lambda_lasso)

str(data_21_large)




data_list = list(data_21_small, data_21_medium, data_21_large)
model_list = vector(mode = "list", length = 3)
performance_matrix_2021 = matrix(ncol = 2, nrow = 3)
colnames(performance_matrix_2021) = c("AIC", "BIC")
rownames(performance_matrix_2021) = c("small", "medium", "large")
for (i in 1:3) {
  start_values <- rep(0, ncol(data_list[[i]]) )
  
  fit = glm(plz5_kba_kraft3 ~., offset = plz5_ew ,data = data_list[[i]], family = poisson(), start = start_values)
  model_list[[i]] = fit 
  performance_matrix_2021[i,1] = AIC(fit)
  performance_matrix_2021[i,2] = BIC(fit)
}

performance_matrix_2021

summary(model_list[[1]])


correlation_matrix = cor(data_21_large[, -2], use = "pairwise.complete.obs")

# Melt correlation matrix
melted_correlation <- melt(correlation_matrix)

# Plot heatmap
ggplot(data = melted_correlation, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Correlation Heatmap") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
