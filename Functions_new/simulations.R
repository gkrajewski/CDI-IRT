sim_se <- function(min_SEM, file = NULL) {
  message(paste("Starting simulation with SE <", min_SEM, "@", Sys.time()))
  results <- mirtCAT(mo = mo, method = "MAP", criteria = "MI", start_item = "MI",
                         local_pattern = responses_r, cl = cl, design = list(min_SEM = min_SEM))
  if(! is.null(file)) save(results, file = file)
  message(paste("Finished @", Sys.time()))
  beep()
  return(results)
}

report_sim_results <- function(sim_results, fscores, cdi_length = nrow(cdi_r)){
  
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
  unused <- cdi_length - items_used_nr
  
  return(paste("Mean length:", mean_length, " Median length:", median_length, " Correlation:", cor, " Mean SE:", meanSE, " Reliability:", rel, " Unused items:", unused))
}

plot_length <- function(results, cdi_name, se) {
  #Prepare cuts
  tests_lengths <- laply(results, function(x) length(x$items_answered))
  cuts <- cut(tests_lengths, breaks = c(0, 10, 15, 25, 35, 50, 75, 100, nrow(cdi_r)-1, nrow(cdi_r)),
              labels = c("1 - 10", "11 - 15", "16 - 25", "26 - 35", "36 - 50", "51 - 75", "76 - 100",
                         paste("101 -", nrow(cdi_r)-1), paste(nrow(cdi_r), "(all)")))
  
  #Plot
  ggplot(data.frame(round(table(cuts) / nrow(responses_r) * 100, 1)), aes(x = cuts, y = Freq)) +
    xlab("Number of items administered") +
    ylab("Percent of respondents (%)") +
    geom_bar(stat = "identity") +
    geom_text(aes(label = Freq), vjust = -0.3, size=6) + 
    labs(title = paste0(cdi_name, ": SE = ", se, " stop criterion")) +
    theme_pubclean() +
    theme(text = element_text(size=16))
}