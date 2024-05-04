# generates formula for glm-function
get_formula <- function(covariates, offset = NULL, target = "plz5_kba_kraft3") {
  
  if (is.null(offset)) {
    covariates <- tolower(covariates[!covariates %in% c("plz5_kba_kraft3", "plz5_kba", "jahr")])
    result <- as.formula(paste(target, paste0(covariates, collapse = " + "), sep = " ~ "))
  }
  else if (!is.null(offset)) {
    covariates <- tolower(covariates[!covariates %in% c("plz5_kba_kraft3", "plz5_kba", "jahr", offset)])
    result <-  as.formula(paste(target, paste(c(covariates, paste("offset(log(", offset, "))", collapse = "")), collapse = "+")
                                , sep = " ~ "))
  }
  result
}

# update existing formula 

get_updated_formula <- function(model.formula, cov.update) {
  temp <- Reduce(paste, deparse(model.formula))
  as.formula(paste(temp, cov.update[[1]], sep  = " + "))
}

#drop correlated columns
drop_correlated_variable <- function(covariates, y, data, cutoff = 0.7) {
  covariates.temp <- covariates[sapply(X = data[, covariates], FUN = function(x) is.numeric(x))]
  covariates.temp <- covariates.temp[abs(sapply(X = covariates.temp, FUN = function(x) cor(x = data[, x], y = data[, y], use = "pairwise.complete.obs"))) < cutoff &
                                       covariates.temp != y]
  c(covariates.temp, covariates[!sapply(X = data[, covariates], FUN = function(x) is.numeric(x))])
}

# Calculates most frequent values in a column
modus <- function(x) {
  return(names(sort(table(x, useNA = "no"), decreasing = T, na.last = T)[1]))
}

