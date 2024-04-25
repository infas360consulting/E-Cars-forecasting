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