sim_se <- function(min_SEM, file = NULL) {
  message(paste("Starting simulation with SE <", min_SEM, "as STOP criterion @", Sys.time()))
  results <- mirtCAT(mo = mo, method = "MAP", criteria = "MI", start_item = "MI",
                     local_pattern = responses, cl = cl, design = list(min_SEM = min_SEM),
                     progress = TRUE)
  if(! is.null(file)) save(results, file = file)
  message(paste("Finished @", Sys.time()))
  beep()
  return(results)
}

report_sim_results <- function(sim_results, fscores, cdi_length){

  #Obtain mean test length
  tests_lengths <- map_int(sim_results, function(x) length(x$items_answered))
  mean_length <- round(mean(tests_lengths), 1)

  #Obtain median test length
  median_length <- round(median(tests_lengths), 1)

  #Obtain thetas
  thetas <- map_dbl(sim_results, "thetas")

  #Get correlation of thetas with raw scores
  cor_score <- round(cor(thetas, fscores$score), 3)

  #Get correlation of thetas with full thetas
  cor_full <- round(cor(thetas, fscores$F1), 3)

  #Get mean SE
  meanSE <- round(mean(map_dbl(sim_results, "SE_thetas")), 3)

  #Get reliability
  reliability <- round(1 - meanSE**2, 3)

  #Get number of unused items
  raw_responses <- do.call(rbind, map(sim_results, "raw_responses"))
  items_used_count <- sum(apply(raw_responses, 2, function(x) any(!is.na(x))))
  items_unused_count <- cdi_length - items_used_count

  cat(paste0("Mean length: ", mean_length, "; Median length: ", median_length, "\nCorrelation with all-item thetas: ", cor_full,
              ", with raw scores: ", cor_score,"\nMean SE: ", meanSE, "; Reliability: ", reliability,
              "\nNever used items: ", items_unused_count, " out of all ", cdi_length))

  invisible(list(mean_length, median_length, cor_full, cor_score, meanSE, reliability, items_unused_count))
}

plot_length <- function(results, cdi_name, se, responses_dim,
                        bin_width = 10, pfs = 6, xfs = 14,
                        title = paste0(cdi_name, " with stop criterion SE < ", ceiling(se * 1000) / 1000)) {

  ###
  # Plots a histogram of the distribution of administration length (number of items)
  # for given simulation results.
  #
  # Parameters:
  # 1: results - simulation results (as returned by sim_se(), which is a wrapper for mirtCAT())
  # 2: cdi_name - for plot title: name of the CDI
  # 3: se - for plot title: SE threshold used as a stopping criterion in sim_se()
  # 4: responses_dim: number of administrations (for bar heights) and of items (for last and second to last bin)
  # 5: bin_width - (optional) defaults to 10 (last bin always equals to all items used while second to last always adjusts)
  # 6: pfs - (optional) font size for percentages above bars
  # 7: xfs - (optional) font size for bin labels on X axis
  # 8: title - (optional) defaults to "cdi_name with stop criterion SE < se"
  ###

  #Prepare cuts
  tests_lengths <- map_int(results, function(x) length(x$items_answered))
  n_adm <- responses_dim[1]
  len   <- responses_dim[2]
  if((len-1) %% bin_width < bin_width/2) {
    breaks <- c(seq(0, len-1-bin_width, by=bin_width), len-1, len)
  } else {
    breaks <- c(seq(0, len-1, by=bin_width), len-1, len)
  }
  labels <- c(paste(breaks+1, breaks[-1], sep = "-")[1:(length(breaks)-2)], paste(len, "(all)"))
  cuts <- cut(tests_lengths, breaks = breaks, labels = labels)

  #Plot
  ggplot(data.frame(round(table(cuts) / n_adm * 100, 1)), aes(x = cuts, y = Freq)) +
    xlab("Number of items administered") +
    ylab("Percent of respondents (%)") +
    geom_bar(stat = "identity") +
    geom_text(aes(label = Freq), vjust = -0.3, size=pfs) +
    labs(title = title) +
    theme_minimal() +
    ylim(0, 100) +
    theme(text = element_text(size=16), axis.text.x = element_text(size=xfs))
}

sim_length_distro_q <- function(results, q) {
  tests_lengths <- map_int(results, function(x) length(x$items_answered))
  threshold <- quantile(tests_lengths, 1 - q)
  print(threshold)
  invisible(list(distro = tests_lengths, threshold = round(threshold)))
}
