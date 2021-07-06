# PhenoNetz
Shiny app code to generate data checks and feedback reports for participants in "PhenoNetz"-study at www.kambeitzlab.com in an automated way.

The "app_" files can be opened in RStudio and run locally as Shiny apps. Within the app, the user can specify the input IDs and data files. Internally, the "_.Rmd" files are then rendered based on the specified input parameters and data files. The resulting .html (data check) and .pdf (feedback report) files can then be downloaded using the Shiny app. Intended for users in our group who are inexperienced with R/RStudio and Markdown.
