library(dplyr)
library(mice)

variables <- c('sex', 'religion', 'marital', 'age.cat', 'under.5', 'above.60', 'pregnant',
               'occupation', 'education', 'hh.size', 'ethnicity', 'WealthIndex', 
               'had.symptoms', 'Community.leaders', 'Hospital', 'Radio', 
               'SMS.Whatsapp', 'TV')

results <- lapply(c(symp.pca = 'Knowledge of symptoms', 
                    trans.pca = 'Knowledge of transmission', 
                    prev.pca = 'Knowledge of prevention'), function(pca_type) {
                      
                      df <- do.call(rbind, lapply(variables, function(var) {
                        fit <- with(imp1, glm(as.formula(paste0(pca_type, " ~ ", var)), family = quasipoisson))
                        summary(pool(fit), conf.int = TRUE)[c(1, 2, 7, 8)] %>% 
                          mutate(var = var)
                      }))
                      
                      df[-1, ] %>% mutate(type = pca_type)
                    })

names(results) <- c("symp", "trans", "prev")
combined_results <- bind_rows(results)
