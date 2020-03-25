library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(graphicalVAR)
library(psychonetrics)
library(mlr)

# ... data


# ... function to extract model per person (contemporaneous & temporal)
getPersonalizedModel <- function(df,  beepvar = "Questionnaire_of_Day",
                                 dayvar = "date_ESM", vars = final_vars) {
  tryCatch(
    expr = {
      model_pruned <- gvar(
        df,
        vars = vars,
        beta = "full",
        beepvar = beepvar,
        dayvar = dayvar
        
      ) %>% runmodel %>% prune(alpha=.05, recursive = F) %>% modelsearch(prunealpha = .05, addalpha = .05)
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
  for (v in seq_along(Vars)) {
    lmResult <- lm(unlist(df[, Vars[v]]) ~ df$time_ESM)
    if (anova(lmResult)[["Pr(>F)"]][1] < 0.05) {
      df[, Vars[v]][!is.na(df[, Vars[v]])] <- residuals(lmResult)
    }
  }
  return(df)
}


# load data
data <- Questionnaire_Abfrage_2020_03_20_08_42_34
label_vars = c(
  "Ich bin traurig.",
  "Ich nehme Dinge wahr, die andere Menschen nicht wahrnehmen können.",
  "Ich habe Schwierigkeiten, mich zu konzentrieren.",
  "Ich bin kontaktfreudig.",
  "Ich fühle mich gestresst.",
  "Ich bin zufrieden mit mir.",
  "Ich fühle mich ängstlich.",
  "Es fällt mir schwer, mich zu Dingen zu motivieren.",
  "Ich bin misstrauisch gegenüber anderen Menschen.",
  "Ich grüble."
)

final_vars <- Vars <- paste0("var_", c(as.character(1:10)))

data$DateTime <-
  as.POSIXct(as.POSIXlt.character(data$DateTime, format = "%d.%m.%y %H:%M", tz = "GMT"))


model_personalized <- data %>%
  mutate(date_ESM = as.Date(DateTime),
         time_ESM = DateTime) %>%
  filter(Participant_ID == 3) %>%
  rename_at(vars(matches("1_1")), ~ final_vars) %>%
  .[3:nrow(.),] %>%
  mutate_at(vars(final_vars), scale) %>%
  removeLinearTrends() %>%
  mutate_at(vars(final_vars), scale) %>%
  getPersonalizedModel()

qgraph::qgraph(
  model_personalized$PDC,
  theme = "Borkulo",
  labels = 1:10,
  layoutOffset = c(-0.05,0),
  nodeNames = label_vars,
  label.cex = 2,
  legend.cex=0.75,
  GLratio = 1,
vsize=5,
edge.width = 3
)