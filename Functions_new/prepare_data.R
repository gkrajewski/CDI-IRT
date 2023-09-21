prepare_data <- function(data, version, production = T){
  
  # ---
  # Prepares IRMiK data for IRT analyses and CAT simulations
  # 
  # Parameters:
  # 0: data - .csv file downloaded from google drive
  # 1: version: "G" (SiG) or "Z" (SiZ)
  # 2: production: TRUE (default) or FALSE (takes comprehension, works only with SiG)
  # ---
  
  cat("\n-- Preparing IRMiK data --")
  
  #Prepare technical global variable 'd'
  d <<- data
  
  #Count age in days
  cat("\nCounting children's age in days...")
  d$WiekDni <<- difftime(d$DataWypAnkietyRodzic, d$DataUrDziecka, units = c("days"))
  d$WiekDni <<- ceiling(d$WiekDni)
  colnames(d)[which(names(d) == "Wiek")] <<- "WiekMiesiace"
  
  #Remove unnecessary columns
  cat("\nRemoving unnecessary columns...")
  d <<- d[, c("DZIECKO_ID", "KOD", "WiekDni", "WiekMiesiace", "Plec", "Wersja", "Kategoria", "Numer", "NumerSiG", "Pozycja", "R", "M")]
  
  #Create df corresponding to the CDI's content
  cat("\nCreating data frame corresponding to the CDI's content...")
  cdi <<- d[, c("Kategoria", "Numer", "NumerSiG", "Pozycja")]
  cdi <<- unique(cdi)
  cdi <<- cdi[order(cdi$Numer),]
  row.names(cdi) <<- NULL
  colnames(cdi) <<- c("category", "number.ws", "number.wg", "position")
  
  if (version == "G") {
    cdi <<- cdi[order(cdi$number.wg), ]
    cdi <<- cdi[!cdi$position %in% c("brzydko", "ciepło", "dobrze", "gorąco", "?adnie", "mokro", "zimno"), ] #Remove some doubled words
    cdi <<- na.omit(cdi) 
  }

  #Choose version
  cat("\nChoosing inventory version...")
  d <<- d[data$Wersja == version,]
  
  #Reshape into wide format
  cat("\nReshaping into wide format...")
  if(version == "G") {
    timevar <- "NumerSiG"
    number.drop <- "Numer"
    if (production){
      v.names <- "M"
      v.names.drop <- "R"
    } else {
      v.names <- "R"
      v.names.drop <- "M"
    }
  } else {
    timevar <- "Numer"
    number.drop <- "NumerSiG"
    v.names <- "M"
    v.names.drop <- "R"
  }
      
  d <<- reshape(d,                           
                timevar = timevar,            
                idvar = c("KOD","WiekDni","WiekMiesiace","Plec"),        
                direction = "wide",           
                v.names = v.names,           
                drop = c("DZIECKO_ID", v.names.drop , "Pozycja", "Kategoria", number.drop, "Wersja"))
  
  #Prepare responses df with demographic variables
  cat("\nPreparing responses data frame...")
  responses_demo <<- as.matrix(d[,5:ncol(d)])
  
  colnames(responses_demo) <<- paste0("item", substring(colnames(responses_demo), 3))
  responses_demo <<- responses_demo[, order(as.integer(substring(colnames(responses_demo), 5)))]
  responses_demo <<- merge(d[1:4], responses_demo, by=0)
  responses_demo <<- responses_demo[order(responses_demo$KOD), ]
  
  responses_demo$Row.names <<- NULL
  rownames(responses_demo) <<- NULL
  
  names(responses_demo)[names(responses_demo) == 'KOD'] <<- 'participant'
  names(responses_demo)[names(responses_demo) == 'WiekDni'] <<- 'days'
  names(responses_demo)[names(responses_demo) == 'WiekMiesiace'] <<- 'months'
  names(responses_demo)[names(responses_demo) == 'Plec'] <<- 'gender'
  
  cat("\n-- Data prepared --")
}