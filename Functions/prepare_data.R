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

  #Count age in days
  cat("\nCounting children's age in days...")
  data$WiekDni <- difftime(data$DataWypAnkietyRodzic, data$DataUrDziecka, units = c("days"))
  data$WiekDni <- ceiling(data$WiekDni)
  colnames(data)[which(names(data) == "Wiek")] <- "WiekMiesiace"

  #Remove unnecessary columns
  cat("\nRemoving unnecessary columns...")
  data <- data[, c("DZIECKO_ID", "KOD", "WiekDni", "WiekMiesiace", "Płeć", "Wersja",
                   "Kategoria", "Numer", "NumerSiG", "Pozycja", "R", "M")]

  #Create df corresponding to the CDI's content
  cat("\nCreating data frame corresponding to the CDI's content...")
  cdi <<- data[, c("Kategoria", "Numer", "NumerSiG", "Pozycja")]
  cdi <<- unique(cdi)
  cdi <<- cdi[order(cdi$Numer),]
  row.names(cdi) <<- NULL
  colnames(cdi) <<- c("category", "item_id", "item_id_wg", "item_definition")

  if (version == "G") {
    cdi <<- cdi[order(cdi$item_id_wg), ]
    cdi <<- cdi[!cdi$item_definition %in% c("brzydko", "ciepło", "dobrze", "gorąco", "ładnie", "mokro", "zimno"), ] #Remove some doubled words
    cdi <<- na.omit(cdi)
    colnames(cdi) <<- c("category", "item_id_ws", "item_id", "item_definition")
  }

  #Choose version
  cat("\nChoosing inventory version...")
  data <- data[data$Wersja == version,]

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

  data <- reshape(data,
                  timevar = timevar,
                  idvar = c("KOD","WiekDni","WiekMiesiace","Płeć"),
                  direction = "wide",
                  v.names = v.names,
                  drop = c("DZIECKO_ID", v.names.drop , "Pozycja", "Kategoria", number.drop, "Wersja"))

  #Prepare responses df with demographic variables
  cat("\nPreparing responses data frame...")
  responses_demo <<- as.matrix(data[,5:ncol(data)])

  colnames(responses_demo) <<- paste0("item", substring(colnames(responses_demo), 3))
  responses_demo <<- responses_demo[, order(as.integer(substring(colnames(responses_demo), 5)))]
  responses_demo <<- merge(data[1:4], responses_demo, by=0)
  responses_demo <<- responses_demo[order(responses_demo$KOD), ]

  responses_demo$Row.names <<- NULL
  rownames(responses_demo) <<- NULL

  names(responses_demo)[names(responses_demo) == 'KOD'] <<- 'child_id'
  names(responses_demo)[names(responses_demo) == 'WiekDni'] <<- 'age_days'
  names(responses_demo)[names(responses_demo) == 'WiekMiesiace'] <<- 'age'
  names(responses_demo)[names(responses_demo) == 'Płeć'] <<- 'sex'

  cat("\n-- Data prepared --\n")
}
