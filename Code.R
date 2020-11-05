library(tidyverse)
library(psychonetrics)
library(qgraph)

# ... data


# ... function to compute model per person
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
        
      )  %>% runmodel %>% prune(alpha = .05, recursive = F) %>% stepup(alpha = .05)
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
  return(df)
}
# load data
data <- Questionnaire_Items_2020_04_02_16_02_06
label_vars = c(
  "Ich bin traurig.",
  "Ich nehme Dinge wahr, \n die andere Menschen nicht wahrnehmen können.",
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


model_personalized <- data %>%
  mutate(date_ESM = as.Date(DateTime),
         time_ESM = DateTime) %>%
  filter(Participant_ID == 6) %>%
  rename_at(vars(matches("1_1")), ~ final_vars) %>%
  mutate_at(vars(final_vars), scale) %>%
  removeLinearTrends() %>%
  mutate_at(vars(final_vars), scale) %>%
  getPersonalizedModel()


# fluctuation of experiences
most_variable_items <- data %>%
  mutate(date_ESM = as.Date(DateTime),
         time_ESM = DateTime) %>%
  filter(Participant_ID == 6) %>%
  rename_at(vars(matches("1_1")), ~ label_vars) %>%
  select(label_vars) %>%
  summarise_all(sd) %>%
  pivot_longer(cols = everything(),
               names_to = "Erlebnis",
               values_to = "Wert") %>%
  top_n(3, Wert)

sequence_dates <- data$DateTime[round(seq(1,  length(data$DateTime), length.out = 6))]

data %>%
  mutate(date_ESM = as.Date(DateTime),
         time_ESM = DateTime) %>%
  filter(Participant_ID == 6) %>%
  rename_at(vars(matches("1_1")), ~ label_vars) %>%
  select(most_variable_items$Erlebnis, time_ESM) %>%
  pivot_longer(cols = most_variable_items$Erlebnis,
               names_to = "Erlebnis",
               values_to = "Wert") %>%
  ggplot(., aes(x=time_ESM, y = Wert, color = Erlebnis)) + facet_wrap(~Erlebnis, ncol = 1) + geom_line(size=1) +
  theme_classic() +
  xlab("\n Datum") +
  theme(legend.position  = "none", 
        strip.text = element_text(size = 14, colour = "black"),
        axis.title = element_text(size = 14, colour = "black"), axis.text = element_text(size = 14, colour = "black")) +
  scale_color_viridis_d() +
  scale_x_datetime(breaks =  date_breaks("4 days"), labels = date_format("%d.%m.%Y"))



# strength of experiences
severity_plot <- data %>%
  mutate(date_ESM = as.Date(DateTime),
         time_ESM = DateTime) %>%
  filter(Participant_ID == 6) %>%
  rename_at(vars(matches("1_1")), ~ label_vars) %>%
  select(label_vars) %>%
  summarise_all(mean) %>%
  pivot_longer(cols = everything(),
               names_to = "Erlebnis",
               values_to = "Wert") %>%
  mutate(Erlebnis = reorder(Erlebnis, Wert)) %>%
  ggplot(., aes(x = Erlebnis, y = Wert, fill = Erlebnis)) +
  geom_bar(stat = "identity") +
  xlab("") +
  ylab("Durchschnittliche Ausprägung") +
  coord_flip() + theme_classic() +
  theme(legend.position  = "none", axis.title = element_text(size = 13), axis.text = element_text(size = 12)) +
  scale_fill_viridis_d()

qgraph::qgraph(
  getmatrix(model_personalized, "omega_zeta"),
  theme = "Borkulo",
  layout="spring",
  labels = 1:10,
  layoutOffset = c(-0.05, 0),
  nodeNames = label_vars,
  label.cex = 1.75,
  legend.cex = 0.75,
  GLratio = 0.9,
  vsize = 5,
  edge.width = 1.5
)



qgraph::qgraph(
  getmatrix(model_personalized, "PDC"),
  theme = "Borkulo",
  layout="spring",
  labels = 1:10,
  layoutOffset = c(-0.05, 0),
  nodeNames = label_vars,
  label.cex = 1.75,
  legend.cex = 0.7,
  GLratio = 1.25,
  vsize = 5,
  edge.width = 1.5
)
