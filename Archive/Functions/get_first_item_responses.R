get_first_item_responses <- function(sim_results, responses){
  
  first_items <- laply(sim_results, function(x) x$items_answered[1])
  raw_responses <- laply(sim_results, function(x) x$raw_responses)
  first_item_responses <- mapply(
    function(subject, first_item){
      return(raw_responses[subject, first_item])
    },
    c(1:nrow(responses)),
    first_items
  )
  tab <- round(table(first_item_responses) / nrow(responses), 3) #2 means selecting an item
  names(tab) <- c("No", "Yes")
  return(tab) 
  
}