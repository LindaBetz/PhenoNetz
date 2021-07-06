

library(shiny)

shinyApp(
  # define user interface object
  ui = fluidPage(
    titlePanel("Generate Data Check for PhenoNetz-Study", windowTitle = "PhenoNetz-Data Check"),
    mainPanel(
      width = '100%',
      helpText(
        "This app allows you to generate the data check for a given participant in the PhenoNetz-study. You need the UKKVIZ-ID, and the most recent data and participant .csv-files (as downloaded from https://uk1.insightsapp.org/). The app will only work with .csv-files. DO NOT OPEN THE FILES IN EXCEL BEFORE."
      ),
      textInput(
        "participant_code",
        label = "Enter the UKKVIZ-ID you want to generate the data check for in the field below. Make sure to avoid typos.",
        width = '100%',
        value = ""
      ),
      fileInput(
        "data",
        label = "Select the most recent data file (starts with Questionnaire_Abfrage_)",
        multiple = FALSE,
        accept = ".csv",
        width = '100%',
        buttonLabel = "Browse...",
        placeholder = "No file selected"
      ),
      fileInput(
        "participant_ids",
        label = "Select the most recent participant file (starts with Participants_Study_Time_Range_)",
        multiple = FALSE,
        accept = ".csv",
        width = '100%',
        buttonLabel = "Browse...",
        placeholder = "No file selected"
      ),
      helpText(
        "Click on this button to generate the .html-data check file and save it."
      ),
      downloadButton("report", "Generate data check")
    )
  ),
  # define server() function
  server = function(input, output) {
    output$report <- downloadHandler(
      filename = function() {
        paste(input$participant_code, "_data_check", ".html", sep = "")
      },
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <-
          file.path(tempdir(), "shiny_generate_data_check.Rmd")
        file.copy("shiny_generate_data_check.Rmd", tempReport, overwrite = TRUE)
        
        # Set up parameters to pass to .Rmd document
        params <- list(
          participant_code = input$participant_code,
          participant_ids = input$participant_ids$datapath,
          data = input$data$datapath
        )
        
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(
          tempReport,
          output_file = file,
          params = params,
          envir = new.env(parent = globalenv()) # this is important!!!
        )
      }
    )
  }
)