get_start_thetas <- function(responses_demo, fscores){
  
  fscores_demo <- cbind(responses_demo[, c("days", "months", "gender")], fscores)
  fscores_demo$weeks <- ceiling(fscores_demo$days / 7)
  fscores_aggr <- aggregate(F1 ~ months + gender, data = fscores_demo, mean)
  
  age_unit <- colnames(fscores_aggr)[1]
  start_thetas <- as.matrix(
    mapply(
      function(age, gender){
        return(fscores_aggr[fscores_aggr[[age_unit]] == age & fscores_aggr$gender == gender, "F1"])
      },
      fscores_demo[[age_unit]],
      fscores_demo$gender
    )
  )
  
  return(list(fscores_aggr, start_thetas))
  
}