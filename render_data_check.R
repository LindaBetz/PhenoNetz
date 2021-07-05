# Code to render .Rmd-File with Participant Code in output_name.pdf

# load data
participant_code <- "UKK1VIZYRNBMKRBF" # participant code (in "")

rmarkdown::render(
  'generate_data_check.Rmd',
  output_file = paste('Data_Check_', participant_code,
                      '.html', sep = '')
)