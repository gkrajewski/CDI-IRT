prepare_input_files <- function(params, cdi, fscores_aggr, question, group, id, file1, file2){
  
  #items.csv
  params_cdi <- data.frame(params, item = cdi)
  params_cdi$question <- question
  params_cdi$group <- group
  params_cdi$id <- id
  write.csv(params_cdi, file = file1, fileEncoding = "UTF-8", row.names = FALSE)
  
  #starThetas.csv
  if(length(fscores_aggr) > 1) {
    start_thetas_df <- fscores_aggr
    colnames(start_thetas_df)[colnames(start_thetas_df) == "sex"] <- "gender"
    colnames(start_thetas_df)[colnames(start_thetas_df) == "F1start"] <- "theta"
  } else {
    start_thetas_df <- data.frame(theta = fscores_aggr)
  }
  start_thetas_df$group <- group
  write.csv(start_thetas_df, file = file2, fileEncoding = "UTF-8", row.names = F)
  
}
