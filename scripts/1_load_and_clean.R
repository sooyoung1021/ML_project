pacman::p_load(tidyverse, ggplot2, dplyr, magrittr, ggthemr, survey)

hp <- rio::import(here::here("data", "pulse2021_puf_25.csv"))

# clean the variables
hp_new <- clean_vars(hp)

# Spent most of stimuls check on essential items (excluding recreational goods, household items (i.e. TV, electronics), donation, and savings)
table(hp_new$stimulus)
# Very difficult to pay household expenses in past 7 days
table(hp_new$fin_hardship)
# Sometimes/often have not enough food
table(hp_new$food_insecurity)
# Not caught up with rent or mortgage
table(hp_new$housing_insecurity)

hp_short <- hp_new %>% select(
  # outcome vars
  fin_hardship,
  housing_insecurity,
  food_insecurity,
  stimulus,
  
  # predictors
  age,
  gender,
  race,
  educ,
  income,
  married,
  insur_type,
  covid_hist,
  job_loss,
  employment,
  ui_benefit,
  ssi_benefit,
  urban,
  phq4,
  housing,
  region
)

hp_short %<>% 
  mutate_if(is.character, as.factor) %>% na.omit()
