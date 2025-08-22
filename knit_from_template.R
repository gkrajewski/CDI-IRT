output_dir <- "Reports"

# Get params
pick_params <- function() {
     ui <- miniUI::miniPage(
               miniUI::gadgetTitleBar("Report & Dataset details", right = miniUI::miniTitleBarButton("done", "Knit", primary = TRUE)),
               miniUI::miniContentPanel(
                    shiny::radioButtons("kind", "Kind of Report:",
                                        choiceNames = c("IRT Model Fitting", "Analyses"),
                                        choiceValues = c("fit", "sim")),
                    shiny::textInput("language", "CDI Adaptation Language (preferably as appears in Wordbank):", value = ""),
                    shiny::radioButtons("scale", "CDI Scale:",
                                        choices = c("WG Comprehension", "WG Production", "WG Gestures", "WS"))
               )
          )
     server <- function(input, output, session) {
          shiny::observeEvent(input$done, {
               shiny::stopApp(list(kind = input$kind, language = input$language, scale = input$scale))
          })
     }
     shiny::runGadget(ui, server)
}
input_params <- pick_params()
if(exists("params")) rm(params)

# Parse params
paste0(input_params$kind, "-") -> kind
input_params$kind <- NULL
version <- strsplit(input_params$scale, " ")[[1]]
subscale <- "Prod"
if(length(version) > 1) substr(version[2], 1, 4) -> subscale
version <- version[1]
if(version == "WS") {
     file_name <- tolower(paste(input_params$language, version, sep="_"))
} else {
     file_name <- tolower(paste(input_params$language, version, subscale, sep="_"))
}


# Render
tryCatch(
     rmarkdown::render(
          input       = paste0(kind, "template.Rmd"),
          params      = c(input_params, knitting = TRUE),
          output_file = paste0(output_dir, "/", kind, file_name, ".html"),
          envir       = new.env()
     ),
     error = function(e) stop(e)
)
