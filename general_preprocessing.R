# -------- 0: load libraries, custom functions ---------
library(tidyverse)
library(mlVAR)

set.seed(123)

# dropout codes to be inserted here
dropouts <-
  c("UKK1VIZYYFOOKKBDR", "UKK1VIZYXFWIZJIM", "UKK1VIZYCAATYTOJ")

# load data
Questionnaire_Abfrage <- read_csv("Questionnaire_Abfrage.csv")
Participants_Study <- read_csv("Participants_Study_Time_Range.csv")

# get IDs corresponding to dropout-codes
dropout_IDs <-
  Participants_Study %>%
  filter(Participant_Code %in% dropouts) %>%
  select(Participant_ID)

# preprocess data for all participants except dropouts
preprocessed_data <- Questionnaire_Abfrage %>%
  filter(!(Participant_ID %in% dropout_IDs)) %>%
  mutate(date_ESM = as.Date(DateTime),
         time_ESM = DateTime) %>%
  group_by(Participant_ID) %>%
  nest() %>%
  mutate(data = map(
    data,
    list(
      . %>% mutate(
        Day_Study = date_ESM - .$date_ESM[1] + 1,
        DateTime = str_replace(DateTime, "UTC", "CET"),
        Query_of_Day = case_when(
          as.POSIXct(paste(date_ESM, "08:30:00")) <= DateTime &
            as.POSIXct(paste(date_ESM, "10:30:00")) >= DateTime ~ 1,
          as.POSIXct(paste(date_ESM, "11:30:00")) <= DateTime &
            as.POSIXct(paste(date_ESM, "13:30:00")) >= DateTime ~ 2,
          as.POSIXct(paste(date_ESM, "14:30:00")) <= DateTime &
            as.POSIXct(paste(date_ESM, "16:30:00")) >= DateTime ~ 3,
          as.POSIXct(paste(date_ESM, "17:30:00")) <= DateTime &
            as.POSIXct(paste(date_ESM, "19:30:00")) >= DateTime ~ 4,
          as.POSIXct(paste(date_ESM, "20:30:00")) <= DateTime &
            as.POSIXct(paste(date_ESM, "22:30:00")) >= DateTime ~ 5
        )
      )  %>%
        mutate(
          Lead_Day = lead(Query_of_Day, 1),
          Query_of_Day = case_when(
            Lead_Day == Query_of_Day ~ Query_of_Day - 1,
            is.na(Lead_Day == Query_of_Day) ~ Query_of_Day,
            TRUE ~ Query_of_Day
          )
        ) %>%
        mutate(
          Ref_Time = case_when(
            Query_of_Day == 1 ~ as.POSIXct(paste(date_ESM, "09:30:00")),
            Query_of_Day == 2  ~  as.POSIXct(paste(date_ESM, "12:30:00")),
            Query_of_Day == 3 ~ as.POSIXct(paste(date_ESM, "15:30:00")),
            Query_of_Day == 4 ~ as.POSIXct(paste(date_ESM, "18:30:00")),
            Query_of_Day == 5 ~ as.POSIXct(paste(date_ESM, "21:30:00"))
          )
        ) %>%
        mutate(Diff_Time = difftime(DateTime, Ref_Time,  units = "mins")) %>%
        mutate(Query_of_Day = ifelse(Diff_Time > 60, NA, Query_of_Day)) %>%
        rename_at(vars(matches("-1-0")), ~ final_vars) %>%
        mutate(across(matches("var_"), function(x)
          ifelse(is.na(Query_of_Day), NA, x))) %>%
        select(-c(Timestamp, Lead_Day, Ref_Time, Diff_Time))
    )
  )) %>%
  mutate(percent_missing = map_dbl(data, ~ sum(is.na(.$var_1)) / nrow(.))) %>%
  left_join(Participants_Study %>% select(Participant_ID, Participant_Code),
            by = "Participant_ID")

# histogram with missingness
preprocessed_data %>%
  ggplot(aes(x = percent_missing)) +
  geom_histogram(binwidth = 0.05, fill = "steelblue") +
  theme_minimal() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 13)) +
  xlab("\nMissing Beeps (%)") +
  ylab("Number of Participants\n") +
  scale_x_continuous(breaks = seq(0, 0.7, 0.1))


# first look at multilevel vector-autoregressive model
label_vars = c(
  "Ich bin traurig",
  "Ich nehme Dinge wahr, die andere \n    Menschen nicht wahrnehmen können",
  "Ich habe Schwierigkeiten, \n    mich zu konzentrieren",
  "Ich bin kontaktfreudig",
  "Ich fühle mich gestresst",
  "Ich bin zufrieden mit mir",
  "Ich fühle mich ängstlich",
  "Es fällt mir schwer, \n    mich zu Dingen zu motivieren",
  "Ich bin misstrauisch \n    gegenüber anderen Menschen",
  "Ich grüble"
)

ml_var_data <-
  preprocessed_data %>% select(Participant_ID, data) %>%
  unnest(data)

res_mlVAR <- mlVAR(
  data = ml_var_data,
  vars = label_vars,
  beepvar = "Query_of_Day",
  idvar =  "Participant_ID"
)

plot(res_mlVAR,
     nodeNames = label_vars,
     labels = TRUE,
     legend = TRUE)