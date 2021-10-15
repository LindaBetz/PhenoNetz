library(tidyverse)
library(psychonetrics)
library(mlVAR)


# function to determine zero range
zeroRange <- function(x, tol = .Machine$double.eps ^ 0.5) {
  x <- x %>% na.omit()
  if (length(x) == 1)
    return(TRUE)
  x <- range(x) / mean(x)
  isTRUE(all.equal(x[1], x[2], tolerance = tol))
}

# function to remove linear trends
removeLinearTrends <- function(df) {
  df <- as.data.frame(df)
  for (v in seq_along(Vars)) {
    if (zeroRange(df[, Vars[v]]) == TRUE) {
      df[, Vars[v]] <- df[, Vars[v]]
    } else {
      lmResult <- lm(unlist(df[, Vars[v]]) ~ df$time_ESM)
      if (anova(lmResult)[["Pr(>F)"]][1] < .05) {
        df[, Vars[v]][!is.na(df[, Vars[v]])] <- residuals(lmResult)
      }
    }
  }
  return(as_tibble(df))
}


# function to compute model per person
getPersonalizedModel <-
  function(df,
           beepvar = "Query_of_Day",
           dayvar = "date_ESM",
           vars = final_vars) {
    tryCatch(
      expr = {
        model_pruned <- gvar(
          df,
          vars = vars,
          beta = "empty",
          omega_zeta = "empty",
          beepvar = beepvar,
          dayvar = dayvar,
          verbose = TRUE,
          standardize = "z",
          optimizer = "nlminb",
          estimator = "FIML"
        ) %>% runmodel %>% stepup(alpha = .05) %>% psychonetrics::prune(
          alpha = .05,
          recursive = F,
          adjust = "none",
          startreduce = 0.95
        )
        # modelsearch(prunealpha = alpha, addalpha = alpha)
        return(model_pruned)
      },
      error = function(e) {
        return(NULL)
        message("Error...")
      }
    )
  }

# load data
data <- read_csv("Questionnaire_Abfrage.csv")
Participants_Study <- read_csv("Participants_Study_Time_Range.csv")

all_vars <- c("var_1", "var_5"  , "var_7"  , "var_9")

# define dropouts, irrelevant Test-IDs, etc. and remove them from df
Participants_Study %>%
  filter(
    !Participant_Code %in% c(
      "UKK1VIZYXFWIZJIM",
      "UKK1VIZYNEMXSMUK",
      "UKK1VIZYFOOKKBDR",
      "UKK1VIZYIAJHBQQD",
      "UKK1VIZYFTXUDQQV",
      "UKK1VIZYGJZWBENV",
      "UKK1VIZYTDGGOAFC",
      "UKK1VIZYCAATYTOJ",
      "UKK1VIZYPHWVDQPA",
      "UKK1VIZYTBLBGTBY"
    )
  ) %>% .$Participant_ID  -> final_participants

# basic preprocessing
data %>%
  filter(Participant_ID %in% final_participants) %>%
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
  nest() %>%
  mutate(detrended_data = map(data, ~ removeLinearTrends(.))) %>%
  select(Participant_ID, detrended_data) %>%
  unnest(cols = c(detrended_data)) %>%
  ungroup() -> detrended_data


# compute models per person in those with >= 20 valid beeps
# if there is any completely constant variable, remove it for network computation
models_per_person_data <- detrended_data %>%
  group_by(Participant_ID) %>%
  nest() %>%
  mutate(valid_beeps = map_int(data, ~ sum(!is.na(.$var_1)))) %>%
  filter(valid_beeps >= 20) %>%
  select(-valid_beeps) %>%
  mutate(
    constant_vars = map(
      data,
      ~ .x %>% select(all_of(all_vars)) %>%
        summarise(across(matches("var"), ~ length(unique(
          .
        ))))  %>%
        select(which(. <= 3)) %>% # <= 3 unique values (constant)
        colnames(.)
    ),
    final_vars = map(constant_vars, ~ subset(all_vars,!(all_vars %in% .x)))
  ) %>%
  mutate(individualized_model = map2(data,
                                     final_vars,
                                     ~ getPersonalizedModel(df = .x, vars = .y)))

# we need to "insert" the constant variables into the networks for compatibility
network_per_person_data <- models_per_person_data %>%
  mutate(
    contemp_net = map(individualized_model, ~ getmatrix(., "omega_zeta")),
    position_constant_var = map(constant_vars, ~ match(., all_vars)),
    contemp_net_2 = map2(contemp_net, position_constant_var, ~
                           if (length(.y) >= 1) {
                             new_mat <-
                               matrix(0, nrow = length(all_vars), ncol = length(all_vars))
                             new_mat[-.y,-.y] <-
                               .x
                             new_mat
                           } else {
                             .x
                           })
  ) %>% select(Participant_ID, contemp_net)


# other option: MLvar
# prepare data
mlvar_data <- detrended_data %>%
  group_by(Participant_ID) %>%
  nest() %>%
  mutate(valid_beeps = map_int(data, ~ sum(!is.na(.$var_1)))) %>%
  filter(valid_beeps >= 20) %>%
  select(-valid_beeps) %>%
  unnest(cols = c(data))

# compute mlvar
res_mlvar <- mlVAR(
  mlvar_data,
  vars = all_of(all_vars),
  idvar = "Participant_ID",
  beepvar = "Query_of_Day",
  dayvar = "date_ESM",
  lag = 1
)

# extract individual networks
individual_networks_mlvar <- c()

for (i in 1:length(unique(mlvar_data$Participant_ID))) {
  individual_networks_mlvar[[i]] <-
    getNet(res_mlvar, "temporal", subject = i)
}