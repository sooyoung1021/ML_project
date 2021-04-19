pacman::p_load(tidyverse, ggplot2, dplyr, magrittr, ggthemr, gt)
## use svymean and shit
table_1 <- cbind(gen_tab_1(hp_short),
                 gen_tab_1(hp_short %>% filter(fin_hardship==1)),
                 gen_tab_1(hp_short %>% filter(fin_hardship==0))) %>% as.data.frame()

colnames(table_1) <- c("Total", "Financial hardship", "No financial hardship")


table_1 %>% gt(rownames_to_stub=T) %>%
  tab_row_group(
    group = "Household income",
    rows = c("Less than $25,000",
             "$25,000 - $34,999",
             "$35,000 - $49,999",
             "$50,000 - $74,999",
             "$75,000 - $99,999",   
             "$100,000 - $149,999",
             "$150,000 - $199,999",
             "$200,000 and above" )
  ) %>%
  tab_row_group(
    group = "Highest educational attainment",
    rows = c("Highschool_or_below",
             "College_or_associate",
             "University",
             "Graduate")
  ) %>%
  tab_row_group(
    group = "Marital status",
    rows = c("Married", "Was married", "Never married")
  )%>%
  tab_row_group(
    group = "Race/ethnicity",
    rows = c("White", "Black", "Hispanic", "Asian", "Other")
  ) %>%
  tab_row_group(
    group = "Sex",
    rows = c("Male", "Female")
  ) %>%
  tab_row_group(
    group = "",
    rows = c("n", "Age")
  ) %>%
  tab_options(
    table.font.size = 8
  )
  
 
