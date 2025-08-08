get_sim_results <- function(sim_results, fscores, params){
  
  #Obtain mean test length
  tests_lengths <- laply(sim_results, function(x) length(x$items_answered))
  mean_length <- round(mean(tests_lengths), 1)
  
  #Obtain median test length
  median_length <- round(median(tests_lengths), 1)
  
  #Obtain thetas
  thetas <- laply(sim_results, function(x) x$thetas)
  
  #Get correlation of thetas with full scores
  cor <- round(cor(thetas, fscores$F1), 3)
  
  #Get mean SE
  meanSE <- round(mean(laply(sim_results, function(x) x$SE_thetas)), 3)
  
  #Get reliability
  rel <- round(1 - meanSE**2, 3)
  
  #Get number of unused items
  raw_responses <- laply(sim_results, function(x) x$raw_responses)
  items_used_nr <- length(which(apply(raw_responses, 2, function(x) any(!is.na(x)))))
  unused <- nrow(params) - items_used_nr
  
  return(paste("Mean length:", mean_length, " Median length:", median_length, " Correlation:", cor, " Mean SE:", meanSE, " Reliability:", rel, " Unused items:", unused))
  
}