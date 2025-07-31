sim_se <- function(min_SEM, file = NULL) {
  message(paste("Starting simulation with SE <", min_SEM, "@", Sys.time()))
  results <- mirtCAT(mo = mo, method = "MAP", criteria = "MI", start_item = "MI",
                         local_pattern = responses, cl = cl, design = list(min_SEM = min_SEM))
  if(! is.null(file)) save(results, file = file)
  message(paste("Finished @", Sys.time()))
  beep()
  return(results)
}

report_sim_results <- function(sim_results, fscores, cdi_length = nrow(cdi)){

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

plot_length <- function(results, cdi_name, se, bin_width = 10, pfs = 6, xfs = 14, title = paste0(cdi_name, " with stop criterion SE < ", ceiling(se * 100) / 100)) {

  ###
  # Plots a histogram of the distribution of administration length (number of items)
  # for given simulation results.
  #
  # Parameters:
  # 1: results - simulation results (as returned by sim_se(), which is a wrapper for mirtCAT())
  # 2: cdi_name - for the plot title: name of the CDI
  # 3: se - for the plot title: SE threshold used as a stopping criterion in sim_se()
  # 4: bin_width - (optional) defaults to 10 and last bin always equals to all items used while second to last always adjusts
  # 5: pfs - (optional) font size for percentages above bars
  # 6: xfs - (optional) font size for bin labels on X axis
  # 7: title - (optional) defaults to "cdi_name with stop criterion SE < se"
  ###

  #Prepare cuts
  tests_lengths <- laply(results, function(x) length(x$items_answered))
  len <- nrow(cdi)
  if((len-1) %% bin_width < bin_width/2) {
    breaks <- c(seq(0, len-1-bin_width, by=bin_width), len-1, len)
  } else {
    breaks <- c(seq(0, len-1, by=bin_width), len-1, len)
  }
  labels <- c(paste(breaks+1, breaks[-1], sep = "-")[1:(length(breaks)-2)], paste(len, "(all)"))
  cuts <- cut(tests_lengths, breaks = breaks, labels = labels)

  #Plot
  ggplot(data.frame(round(table(cuts) / nrow(responses) * 100, 1)), aes(x = cuts, y = Freq)) +
    xlab("Number of items administered") +
    ylab("Percent of respondents (%)") +
    geom_bar(stat = "identity") +
    geom_text(aes(label = Freq), vjust = -0.3, size=pfs) +
    labs(title = title) +
    theme_pubclean() +
    ylim(0, 100) +
    theme(text = element_text(size=16), axis.text.x = element_text(size=xfs))
}
