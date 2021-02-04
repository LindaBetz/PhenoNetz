# Code to render .Rmd-File with Participant Code in output_name.pdf

library(tidyverse)

# load data
participant_code <- "UKK1VIZYJMBRRACL" # participant code (in "")

rmarkdown::render(
  'data_check_report.Rmd',
  output_file = paste('Data_Check_', participant_code,
                      '.html', sep = '')
)