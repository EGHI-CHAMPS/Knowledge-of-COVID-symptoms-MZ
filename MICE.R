library(dplyr)
library(mice)

dat.2 <- dat.1 %>%
  select(
    sex, religion, marital, age.cat, under.5, above.60, pregnant,
    occupation, education, hh.size, ethnicity, WealthIndex,
    heard.corona, heard.covid, covid.exists, know.transmit, know.prevent, had.symptoms,
    Community.leaders, Hospital, Radio, SMS.Whatsapp, TV,
    Call.the.community.leader, Call.the.hospital, Go.to.the.hospital,
    Quarantine, Treat.symptoms.at.home,
    symp.pca, trans.pca, prev.pca
  ) %>%
  mutate(across(c(age.cat, sex, ethnicity, religion, education, occupation, marital,
                  hh.size, under.5, above.60, pregnant, WealthIndex,
                  Community.leaders, Hospital, Radio, SMS.Whatsapp, TV, had.symptoms),
                ~ relevel(as.factor(.), ref = as.character(min(as.integer(levels(as.factor(.))))))))

names(dat.2) <- make.names(names(dat.2), unique = TRUE)

set.seed(11)
imp1 <- mice(dat.2, m = 20, method = 'pmm', seed = 11)