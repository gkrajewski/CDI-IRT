get_start_thetas <- function(fscores, age = NULL, sex = NULL, correction = 0){
  
  aggr_formula <- paste(c(age, sex), collapse = " + ")
  if(aggr_formula == "") {
    fscores_aggr <- mean(fscores$F1) - correction
    start_thetas <- fscores
    start_thetas$F1start <- fscores_aggr
    } else {
    fscores_aggr <- aggregate(as.formula(paste("F1 ~", aggr_formula)), data = fscores, mean)
    colnames(fscores_aggr)[colnames(fscores_aggr) == "F1"] <- "F1start"
    fscores_aggr$F1start <- fscores_aggr$F1start - correction
    join(fscores, fscores_aggr) -> start_thetas
  }
  
  start_thetas <- as.matrix(start_thetas[, ncol(start_thetas)])
  
  return(list(fscores_aggr, start_thetas))
  
}
