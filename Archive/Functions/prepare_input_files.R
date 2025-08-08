prepare_input_files <- function(params, cdi, fscores_aggr, question, group, file1, file2){
  
  #items.csv
  params_cdi <- cbind(params, cdi)
  params_cdi <- params_cdi[, !names(params_cdi) %in% c("item", "category", "number.wg", "number.ws")]
  params_cdi$question <- question
  params_cdi$group <- group
  write.csv(params_cdi, file = file1, fileEncoding = "UTF-8", row.names = F)
  
  #starThetas.csv
  start_thetas_df <- fscores_aggr
  colnames(start_thetas_df) <- c("age", "gender", "theta")
  start_thetas_df$group <- group
  write.csv(start_thetas_df, file = file2, fileEncoding = "UTF-8", row.names = F)
  
}