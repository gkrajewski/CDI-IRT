quadpts_comp <- function(responses, quadpts, NCYCLES, output_file = NULL) {
  cat(paste("Starting number of quadrature points (quadpts) comparison for the following values:",
            paste(quadpts, collapse = ", "), "\n"))
  sapply(quadpts, function(x) {
    cat(paste("\n -- quadpts:", x, "@", Sys.time(), "-- \n"))
    mod <- mirt(data = responses, model = 1, SE = TRUE, quadpts = x, technical = list(NCYCLES = NCYCLES))
    list(quadpts = x, conv = mod@OptimInfo$converged, logLik = mod@Fit$logLik,
         item1pars = coef(mod, simplify = T)$items[1, 1:2])
  }) -> quadpts

  if (!is.null(output_file)){
    save(quadpts, file = output_file)
    cat(paste0("\nOutput is saved as ", output_file, " in ", getwd()))
  }
  cat(paste("\nFinished @", Sys.time()))
  beep()
  return(quadpts)
}

misfits_removal <- function(responses, quadtps, NCYCLES, p, output_file = NULL, maxN = ncol(responses)){

  ###
  # Creates subsequent models and removes items from the pool until there are no misfits.
  #
  # Parameters:
  # 1: responses - matrix with 0s and 1s (no. respondents X no. items)
  # 2: quadtps - number - number of quadrature points to use in models creation
  # 3: NCYCLES - number - maximum number of cycles
  # 4: p - number - p-value - threshold for misfits removal
  # 5: outputFile - string - name of file with output
  # 6: maxN - number - maximum number of models to create, default is ncol(responses)
  ###

  items_removed <- list()
  items_nr <- ncol(responses)

  for (n in 1:ncol(responses)){

    cat(paste("\n -- MODEL", n, "@", Sys.time(), "-- \n"))

    if (n == maxN + 1){

      cat(paste0("\n=====\n\nMaximum number of models i.e. ", maxN, " created"))
      break

    } else {

      mod <- mirt(data = responses, model = 1, SE = TRUE, quadpts = quadtps, technical = list(NCYCLES = NCYCLES))
      cat(paste0("\nCalculating item fit..."))
      item_fit <- itemfit(mod, method = "MAP")
      items_to_remove <- item_fit[item_fit$p.S_X2 < p | is.na(item_fit$p.S_X2), "item"]

      if (length(items_to_remove) == 0){

        cat(paste0("\n=====\n\nModel with no misfits obtained. Created with ", items_nr, " items"))
        break

      } else {

        items_removed[[n]] <- colnames(responses)[which(colnames(responses) %in% items_to_remove)]
        items_removed_nr <- length(items_removed[[n]])
        items_nr <- items_nr - items_removed_nr
        responses <- responses[, !colnames(responses) %in% items_to_remove]
        cat(paste0("\n", items_removed_nr, " item(s) needed to be removed for p = ", p, "\n", items_nr, " items left\n"))

      }

    }

  }

  model <- list(mod, items_removed, item_fit)
  cat(paste("\nFinished @", Sys.time()))
  if (!is.null(output_file)){
    save(model, file = output_file)
    cat(paste0("\nOutput is saved as ", output_file, " in ", getwd()))
  }
  beep()
  return(model)

}


cramers_V_df <- function(residuals, ind, cdi, item_fit = NULL) {
  items_1 <- cdi[ind[, 1], colnames(cdi)]
  items_2 <- cdi[ind[, 2], colnames(cdi)]
  names(items_1) <- paste0(names(items_1), "_1")
  names(items_2) <- paste0(names(items_2), "_2")
  ret <- bind_cols(items_1, items_2, CramersV = residuals[ind])
  if(! is.null(item_fit)) {
    ret %>%
      left_join(item_fit[, 1:2], by = join_by(item_id_1 == item)) %>%
      rename(item1_fit = S_X2) %>%
      left_join(item_fit[, 1:2], by = join_by(item_id_2 == item)) %>%
      rename(item2_fit = S_X2) %>% return()
  } else return(ret)
}
