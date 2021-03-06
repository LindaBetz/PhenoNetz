set.seed(1)
library(tidyverse)
library(psychonetrics)
library(qgraph)
library(scales)
alpha <-
  .01 # auf .05 setzen, wenn beide Netzwerke sonst empty wären
ID <- 7
# ... function to compute model per person
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
          beta = "full",
          omega_zeta = "full",
          beepvar = beepvar,
          dayvar = dayvar,
          verbose = TRUE,
          estimator = "FIML"
          
          
        )  %>% runmodel %>% prune(alpha = alpha, recursive = F) %>% stepup(alpha = alpha)
        return(model_pruned)
      },
      error = function(e) {
        return(NULL)
        message("Error...")
      }
    )
  }
# ... function to check and remove linear trends
removeLinearTrends <- function(df) {
  df <- as.data.frame(df)
  for (v in seq_along(Vars)) {
    lmResult <- lm(unlist(df[, Vars[v]]) ~ df$time_ESM)
    if (anova(lmResult)[["Pr(>F)"]][1] < .05) {
      df[, Vars[v]][!is.na(df[, Vars[v]])] <- residuals(lmResult)
    }
  }
  return(as_tibble(df))
}
# load data
Questionnaire_Abfrage <- read_csv("Questionnaire_Abfrage.csv")
data <- Questionnaire_Abfrage
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
final_vars <- Vars <- paste0("var_", c(as.character(1:10)))
model_personalized <- data %>%
  mutate(date_ESM = as.Date(DateTime),
         time_ESM = DateTime) %>%
  filter(Participant_ID == ID) %>%
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
  select(-c(Timestamp, Lead_Day, Ref_Time, Diff_Time)) %>%
  mutate_at(vars(final_vars), scale) %>%
  removeLinearTrends() %>%
  #mutate_at(vars(final_vars), scale) %>%
  getPersonalizedModel()
# strength of experiences
severity_plot <- data %>%
  mutate(date_ESM = as.Date(DateTime),
         time_ESM = DateTime) %>%
  filter(Participant_ID == ID) %>%
  rename_at(vars(matches("1-0")), ~ label_vars) %>%
  select(label_vars) %>%
  summarise_all(mean) %>%
  pivot_longer(cols = everything(),
               names_to = "Erlebnis",
               values_to = "Wert") %>%
  mutate(Erlebnis = reorder(Erlebnis, Wert)) %>%
  ggplot(., aes(x = Erlebnis, y = Wert, fill = Erlebnis)) +
  geom_bar(stat = "identity") +
  xlab("") +
  ylab("\n Durchschnittliche Stärke (%)") +
  coord_flip() + theme_classic() +
  theme(
    legend.position  = "none",
    axis.title = element_text(size = 14, colour = "black"),
    axis.text = element_text(size = 14, color = "black")
  ) +
  scale_fill_viridis_d()
# fluctuation of experiences
most_variable_items <- data %>%
  mutate(date_ESM = as.Date(DateTime),
         time_ESM = DateTime) %>%
  filter(Participant_ID == ID) %>%
  rename_at(vars(matches("1-0")), ~ label_vars) %>%
  select(label_vars) %>%
  summarise_all(sd) %>%
  pivot_longer(cols = everything(),
               names_to = "Erlebnis",
               values_to = "Wert") %>%
  top_n(3, Wert)
fluctuation_plot <- data %>%
  mutate(date_ESM = as.Date(DateTime),
         time_ESM = DateTime) %>%
  filter(Participant_ID == ID) %>%
  rename_at(vars(matches("1-0")), ~ label_vars) %>%
  select(most_variable_items$Erlebnis, time_ESM) %>%
  pivot_longer(cols = most_variable_items$Erlebnis,
               names_to = "Erlebnis",
               values_to = "Wert") %>%
  ggplot(., aes(x = time_ESM, y = Wert, color = Erlebnis)) + facet_wrap(~
                                                                          Erlebnis, ncol = 1) +  geom_line(size =
                                                                                                             1) +
  geom_smooth(method = "loess", formula = y ~ x) +
  theme_classic() +
  xlab("\n Datum") +
  theme(
    legend.position  = "none",
    strip.text = element_text(size = 14, colour = "black"),
    axis.title = element_text(size = 14, colour = "black"),
    axis.text = element_text(size = 14, colour = "black")
  ) +
  scale_color_viridis_d() +
  scale_y_continuous(limits = c(-10, 100),
                     breaks = c(0, 25, 50, 75, 100)) +
  scale_x_datetime(breaks =  date_breaks("2 days"), labels = date_format("%d.%m."))
