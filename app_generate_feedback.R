

library(shiny)

shinyApp(
  # define user interface object
  ui = fluidPage(
    titlePanel("Generate Feedback Report for PhenoNetz-Study", windowTitle = "PhenoNetz-Feedback"),
    mainPanel(
      width = '100%',
      helpText(
        "This app allows you to create the feedback report for a given participant in the PhenoNetz-study. You need the UKKVIZ-ID, and the most recent data and participant .csv-files (as downloaded from https://uk1.insightsapp.org/). The app will only work with .csv-files. DO NOT OPEN THE FILES IN EXCEL BEFORE."
      ),
      textInput(
        "participant_code",
        width = '100%',
        label = "Enter the UKKVIZ-ID you want to get feedback for in the field below. Make sure to avoid typos.",
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
        label = "Select the most recent participant file here (starts with Participants_Study_Time_Range)",
        multiple = FALSE,
        accept = ".csv",
        width = '100%',
        buttonLabel = "Browse...",
        placeholder = "No file selected"
      ),
      checkboxInput(
        "contemp_only",
        label =  "Tick this box in case of errors or weird results, e.g. due to many missings. Then a more conservative estimation method will be used.",
        value = FALSE,
        width = '100%'
      ),
      helpText(
        "Click on this button to generate the .pdf-feedback file and save it. Note that it can take a few minutes for the file to finish processing"
      ),
      downloadButton("report", "Generate feedback")
    )
  ),
  # define server() function
  server = function(input, output) {
    output$report <- downloadHandler(
      filename = function() {
        paste(input$participant_code, "_feedback", ".pdf", sep = "")
      },
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <-
          file.path(getwd(), "shiny_generate_feedback.Rmd")
        
        # Set up parameters to pass to .Rmd document
        params <- list(
          contemp_only = input$contemp_only,
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
