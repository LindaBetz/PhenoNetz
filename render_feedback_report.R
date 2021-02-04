# Code to render .Rmd-File with Participant Code in output_name.pdf

library(tidyverse)

# load data
participant_code <- "UKK1VIZYJMBRRACL" # participant code (in "")

rmarkdown::render(
  'generate_feedback_report.Rmd',
  output_file = paste('Feedback_', participant_code,
                      '.pdf', sep = '')
)