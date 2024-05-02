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
drop_correlated_variable <- function(covariates, y, data, cutoff=0.7) {
  covariates[abs(sapply(X = covariates, FUN = function(x) cor(x = data[, x], y = data[, y], use = "pairwise.complete.obs"))) < cutoff]
}
