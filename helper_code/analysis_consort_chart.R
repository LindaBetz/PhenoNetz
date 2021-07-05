library(tidyverse)


data <- read_csv("Questionnaire_Abfrage.csv")

Participants_Study <- read_csv("Participants_Study_Time_Range.csv")
Participants_Study <-
  Participants_Study %>% filter(!Participant_Code %in% c("UKK1VIZYFTXUDQQV", "UKK1VIZYCAATYTOJ")) # versehentlich erstellte ID, KEIN DROPOUT; Majds ID


Participants_Study %>%
  filter(
    !Participant_Code %in% c(
      "UKK1VIZYXFWIZJIM",
      "UKK1VIZYNEMXSMUK",
      "UKK1VIZYFOOKKBDR",
      "UKK1VIZYIAJHBQQD"
    )
  ) %>% .$Participant_ID  -> dropouts_in_study

dropouts_in_study[!dropouts_in_study %in% out$Participant_ID]

length(Participants_Study$Participant_ID)
length(dropouts_in_study)

data %>%
  filter(Participant_ID %in% IDs_in_study$Participant_ID) %>%
  mutate(date_ESM = as.Date(DateTime),
         time_ESM = DateTime) %>%
  mutate(
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
      Query_of_Day == 2 ~ as.POSIXct(paste(date_ESM, "12:30:00")),
      Query_of_Day == 3 ~ as.POSIXct(paste(date_ESM, "15:30:00")),
      Query_of_Day == 4 ~ as.POSIXct(paste(date_ESM, "18:30:00")),
      Query_of_Day == 5 ~ as.POSIXct(paste(date_ESM, "21:30:00"))
    )
  ) %>%
  mutate(Diff_Time = difftime(DateTime, Ref_Time,  units = "mins")) %>%
  mutate(Query_of_Day = ifelse(abs(Diff_Time) > 60, NA, Query_of_Day)) %>%
  rename_at(vars(matches("-1-0")), ~ final_vars) %>%
  mutate(across(matches("var_"), function(x)
    ifelse(is.na(.$Query_of_Day), NA, x))) %>%
  select(-c(Timestamp, Lead_Day, Ref_Time, Diff_Time)) %>%
  group_by(Participant_ID) %>%
  nest()