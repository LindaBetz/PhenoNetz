# Code to render .Rmd-File with Participant Code in output_name.pdf

# load data
participant_code <- "UKK1VIZYBPUFFDKG" # participant code (in "")

rmarkdown::render(
  'generate_feedback.Rmd',
  output_file = paste('Feedback_', participant_code,
                      '.pdf', sep = '')
)
