data <- Questionnaire_Abfrage

out <- data %>%
  mutate(date_ESM = as.Date(DateTime),
         time_ESM = DateTime) %>%
  filter(Participant_ID == 2) %>%
  mutate(
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
  select(-c(Timestamp, Lead_Day, Ref_Time, Diff_Time, Study_Day, Questionnaire_of_Day)) %>%
  mutate_at(vars(final_vars), scale) %>%
  removeLinearTrends() %>%
  mutate_at(vars(final_vars), scale) %>% mutate(idvar=1) 



data <- as.data.frame(out)
vars <- final_vars

# Only retain important columns:
data <- data[,c(vars,idvar,dayvar,beepvar)]


# From mlVAR: Augment data:
# Augment the data
augData <- data
beepvar = "Query_of_Day"
dayvar = "date_ESM"
idvar = "idvar"

# Add missing rows for missing beeps
beepsPerDay <-  eval(substitute(dplyr::summarize_(data %>% group_by_(idvar,dayvar), 
                                                  first = ~ min(beepvar,na.rm=TRUE),
                                                  last = ~ max(beepvar,na.rm=TRUE)), 
                                list(beepvar = as.name(beepvar))))

# all beeps (with one extra beep before each measurement of the first day:
allBeeps <- expand.grid(unique(data[[idvar]]),unique(data[[dayvar]]),seq(min(data[[beepvar]],na.rm=TRUE)-1,max(data[[beepvar]],na.rm=TRUE))) 
names(allBeeps) <- c(idvar,dayvar,beepvar)

# Left join the beeps per day:
allBeeps <- eval(substitute({
  allBeeps %>% dplyr::left_join(beepsPerDay, by = c(idvar,dayvar)) %>% 
    dplyr::group_by_(idvar,dayvar) %>% dplyr::filter_(~BEEP >= first-1, ~BEEP <= last)%>%
    dplyr::arrange_(idvar,dayvar,beepvar)
},  list(BEEP = as.name(beepvar))))


# Enter NA's:
augData <- augData %>% dplyr::right_join(allBeeps, by = c(idvar,dayvar,beepvar)) 
