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
    
    cat(paste0("\n -- ", "MODEL ", n, " -- \n"))
    
    if (n == maxN + 1){
      
      cat(paste0("\n=====\n\nMaximum number of models i.e. ", maxN, " created"))
      break
      
    } else {
      
      mod <- mirt(data = responses, model = 1, SE = TRUE, quadpts = quadtps, technical = list(NCYCLES = NCYCLES))
      itemfit <- itemfit(mod, method = "MAP")
      items_to_remove <- itemfit[itemfit$p.S_X2 < p | is.na(itemfit$p.S_X2), "item"]
      
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
  
  output <- list(mod, items_removed)
  if (!is.null(output_file)){
    save(output, file = output_file)
    cat(paste0("\nOutput is saved as ", output_file, " in ", getwd()))
  } 
  return(output) 

}