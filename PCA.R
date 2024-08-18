library(dplyr)
library(tidyr)
library(psych)

create_wide_data <- function(data, variable, labels) {
  data %>%
    transmute(household, var = unlist(strsplit(as.character(get(variable)), " "))) %>%
    mutate(var = factor(var, levels = names(labels))) %>%
    spread(var, 1, fill = 0) %>%
    select(-one_of(c("88", "98"))) 
}

symptom_labels <- c("1" = "Flu-like symptoms", "2" = "Dry cough", "3" = "Cough with sputum",
                    "4" = "Fever", "5" = "Headaches", "6" = "Sore throat",
                    "7" = "Difficulty breathing", "8" = "Muscle pain", "9" = "Vomiting")

transmission_labels <- c("1" = "Inhalation of air", "2" = "Droplets", "3" = "Touching mouth",
                         "4" = "Touching eyes/nose", "6" = "Touching hands",
                         "7" = "Hugging", "8" = "Kissing", "9" = "Touching person", "10" = "Touching fomite")

prevention_labels <- c("1" = "Wash hands with soap", "2" = "Avoid touching mouth", "3" = "Avoid touching eyes",
                       "4" = "Avoid touching nose", "5" = "Use alcohol", "6" = "Wear mask", "7" = "Quarantine",
                       "8" = "Social distancing", "9" = "Avoid crowds", "10" = "Avoid traveling")

dat_symp <- create_wide_data(dat.1, "symptoms", symptom_labels)
dat_trans <- create_wide_data(dat.1, "how_transmitted", transmission_labels)
dat_prev <- create_wide_data(dat.1, "how_prevent", prevention_labels)

run_pca <- function(data) {
  pca <- principal(data, rotate = "varimax", nfactors = 1, scores = TRUE)
  pca_scores <- as.vector(pca$scores)
  list(pca_scores = pca_scores)
}

symp_results <- run_pca(select(dat_symp, -household))
trans_results <- run_pca(select(dat_trans, -household, -`Inhalation of air`))
prev_results <- run_pca(select(dat_prev, -household, -`Wash hands with soap`, -`Wear mask`))

final_data <- dat.1 %>%
  select(household) %>%
  mutate(
    symp_pca = symp_results$pca_scores,
    trans_pca = trans_results$pca_scores,
    prev_pca = prev_results$pca_scores
  )

