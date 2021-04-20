# functions

clean_vars <- function(data, long=F){
  pacman::p_load(tidyverse, dplyr, magrittr)
  
  data_new <- data %>% mutate(
    ### RECODING
    
    # outcome - food insecurity, housing insecurity, financial hardship, stimulus check
    
    ## FInancial hardship - "Very difficult" to pay household expenses in past 7 days
    fin_hardship = case_when(
      EXPNS_DIF == 4 ~ 1,
      EXPNS_DIF %in% c(1, 2, 3) ~ 0,
      TRUE ~ NaN
    ),
    
    ## Housing insecurity (not caught up with rent or mortgage)
    housing_insecurity = case_when(
      RENTCUR ==2 | MORTCUR==2 ~ 1,
      RENTCUR ==1 | MORTCUR==1 ~ 0,
      TRUE ~ NaN
    ),
    
    ## Food insecurity (sometimes/often not enough food)
    food_insecurity = case_when(
      CURFOODSUF %in% c(3,4) ~ 1,
      CURFOODSUF %in% c(1,2) ~ 0,
      TRUE ~ NaN
    ),
    
    ## Stimulus check usage
    stimulus = case_when(
      EIP == 1 & EIPSPND5 !=1 & EIPSPND4 !=1 & EIPSPND11 !=1 & EIPSPND12 != 1 ~ 1,
      TRUE ~ 0
    ),
    # predictors
    age = 2021 -TBIRTH_YEAR,
    gender = ifelse(EGENDER == 1, "Male", 
                    ifelse(EGENDER == 2, "Female", NA)),
    race = case_when(
      RHISPANIC==1 ~ "Hispanic",
      RRACE == 1 ~ "White",
      RRACE == 2 ~ "Black",
      RRACE == 3 ~ "Asian",
      RRACE == 4 ~ "Other",
      TRUE ~ NA_character_
    ),
    educ = case_when(
      EEDUC %in% c(1, 2, 3) ~ "Highschool or below",
      EEDUC %in% c(4,5) ~ "College or Associate degree",
      EEDUC == 6 ~ "University degree",
      EEDUC == 7 ~ "Graduate degree",
      TRUE ~ NA_character_
    ),
    income = case_when(
      INCOME == 1 ~ "Less than $25,000",
      INCOME == 2 ~ "$25,000 - $34,999",
      INCOME == 3 ~ "$35,000 - $49,999",
      INCOME == 4 ~ "$50,000 - $74,999",
      INCOME == 5 ~ "$75,000 - $99,999",   
      INCOME == 6 ~ "$100,000 - $149,999",
      INCOME == 7 ~ "$150,000 - $199,999",
      INCOME == 8 ~ "$200,000 and above",
      TRUE ~ NA_character_
    ),

  # insurance type
    insur_type = case_when(
      HLTHINS1 == 2 & HLTHINS2 ==2 & HLTHINS3 ==2 & HLTHINS4 ==2 & 
        HLTHINS5 == 2 & HLTHINS6 ==2 & HLTHINS7==2 & HLTHINS8==2 ~ "No insurance",
      HLTHINS1==1 ~ "Employer-provided",
      HLTHINS2==1 ~ "Privately purchased",
      HLTHINS3==1 | HLTHINS4==1 ~ "Medicare/Medicaid",
      HLTHINS5==1 | HLTHINS6==1 | HLTHINS7==1|HLTHINS8==1 ~ "Other",
      TRUE ~ NA_character_
    ),
  
  # covid history
    covid_hist = ifelse(HADCOVID==1, 1,
                        ifelse(HADCOVID==2, 0, NA)),
  # job loss in p7d
    job_loss = ifelse(WRKLOSS==1, 1,
                      ifelse(WRKLOSS==2, 0, NA)),
  
    # employment condition
    employment = case_when(
      ANYWORK==2 ~ "Unemployed",
      ANYWORK==1 & KINDWORK == 1 ~ "Government",
      ANYWORK==1 & KINDWORK == 2 ~ "Private",
      ANYWORK==1 & KINDWORK == 3 ~ "NGO",
      ANYWORK==1 & KINDWORK %in% c(4,5) ~ "Self- or family business",
      TRUE ~ NA_character_
    ),
    
    # UI status
    ui_benefit = case_when(
      UI_APPLY==2 ~ "Not applied",
      UI_APPLY==1 & UI_RECV==1 ~ "Received",
      UI_APPLY==1 & UI_RECV==2 ~ "Applied but not received",
      TRUE ~ NA_character_
    ),
    
    # SSI status
    ssi_benefit = case_when(
      SSA_APPLY==2 ~ "Not applied",
      SSA_APPLY==1 & SSA_RECV==1 ~ "Received",
      SSA_APPLY==1 & SSA_RECV==2 ~ "Applied but not received",
      TRUE ~ NA_character_
    ),
    #urban vs. rural
    urban = ifelse(is.na(EST_MSA), 0, 1),
  
    #PHQ-4 (mental health score)
    phq4 = apply(select(., ANXIOUS, WORRY, INTEREST, DOWN),
                 1,
                 function(x){
                   x[x<0]<-NA
                   return(sum(x, na.rm=T)-4)
                 }
    ),
     # housing
    housing = case_when(
      LIVQTR %in% c(1, 10) ~ "Mobile home, boat, RV, van",
      LIVQTR %in% c(2, 3) ~ "One-faimly house",
      LIVQTR %in% c(4,5,6,7) ~ "Building with <20 units",
      LIVQTR %in% c(8,9) ~ "Building with >20 units",
      TRUE ~ NA_character_
    ),
  
    # region
    region = case_when(
      REGION==1 ~ "Northeast",
      REGION==2 ~ "South",
      REGION==3 ~ "Midwest",
      REGION==4 ~ "West",
      TRUE ~ NA_character_
    ),
    
    # marrital status
    married = case_when(
      MS==1 ~ "Married",
      MS %in% c(2, 3, 4) ~ "Was married",
      MS ==5 ~ "Never married",
      TRUE ~ NA_character_
    )
  
  ) %>% mutate(
    # factorize the categorical variables
    insur_type = factor(insur_type, 
                        levels = c("No insurance",
                                   "Employer-provided",
                                   "Privately purchased",
                                   "Medicare/Medicaid",
                                   "Other")),
    educ = factor(educ,
                  levels = c("Highschool or below",
                             "College or Associate degree",
                             "University degree",
                             "Graduate degree")),
    income = factor(income, 
                    levels = c("Less than $25,000",
                               "$25,000 - $34,999",
                               "$35,000 - $49,999",
                               "$50,000 - $74,999",
                               "$75,000 - $99,999",   
                               "$100,000 - $149,999",
                               "$150,000 - $199,999",
                               "$200,000 and above"
                    )
    )
  ) 
  
  if("GETVACC" %in% colnames(data)){
    data_new %<>% mutate(
      ## RECVDVACC # received vaccine
      received_vaccine = case_when(
        RECVDVACC==1 ~ 1,
        RECVDVACC==2 ~ 0,
        TRUE ~ NaN),
      ## GETVACC # willingness to get vaccinated
      vacc_hesitant = case_when(
        GETVACC %in% c(1, 2) ~ 0,
        GETVACC %in% c(3, 4) ~ 1,
        TRUE ~ NaN
      ),
    )
  }else{
    data_new %<>% mutate(
      received_vaccine = NA,
      vacc_hesitant = NA
    )
  }
  
  # only use data points with available information on vulnerabilities
  data_new %<>% filter(
    !is.na(apply(select(., housing_insecurity, food_insecurity, stimulus, fin_hardship), 
          1, function(x) return(ifelse(sum(x)==0, 0, 1))))
  )
  
  # return data
  return(data_new)
}


gen_tab_1 <- function(data){
  pacman::p_load(tidyverse, dplyr, magrittr, survey)


  table_1 <- data %>% summarize(
  # Total N
   n = n(),
    # age  
  Age = sprintf("%.1f (%.1f)", mean(age), sd(age)),
    
    # sex
  Male = sprintf("%i (%.1f%%)", sum(gender=="Male"), sum(gender=="Male")/nrow(data)*100),
  Female = sprintf("%i, (%.1f%%)", sum(gender =="Female"), sum(gender=="Female")/nrow(data)*100),
    
    # race
  White = sprintf("%i (%.1f%%)", sum(race=="White"), sum(race=="White")/nrow(data)*100),
  Hispanic= sprintf("%i (%.1f%%)", sum(race=="Hispanic"), sum(race=="Hispanic")/nrow(data)*100),
  Black= sprintf("%i (%.1f%%)", sum(race=="Black"), sum(race=="Black")/nrow(data)*100),
  Asian= sprintf("%i (%.1f%%)", sum(race=="Asian"), sum(race=="Asian")/nrow(data)*100),
  Other= sprintf("%i (%.1f%%)", sum(race=="Other"), sum(race=="Other")/nrow(data)*100),
    
    # educ
  Highschool_or_below = sprintf("%i (%.1f%%)", sum(educ=="Highschool or below"), sum(educ=="Highschool or below")/nrow(data)*100),
  College_or_associate = sprintf("%i (%.1f%%)", sum(educ=="College or Associate degree"), sum(educ=="College or Associate degree")/nrow(data)*100),
  University = sprintf("%i (%.1f%%)", sum(educ=="University degree"), sum(educ=="University degree")/nrow(data)*100),
  Graduate = sprintf("%i (%.1f%%)", sum(educ=="Graduate degree"), sum(educ=="Graduate degree")/nrow(data)*100),
    
    # income
  `Less than $25,000` = sprintf("%i (%.1f%%)", sum(income=="Less than $25,000", na.rm=T), sum(income=="Less than $25,000", na.rm=T)/nrow(data)*100),
  `$25,000 - $34,999`= sprintf("%i (%.1f%%)", sum(income=="$25,000 - $34,999", na.rm=T), sum(income=="$25,000 - $34,999", na.rm=T)/nrow(data)*100),
  `$35,000 - $49,999`= sprintf("%i (%.1f%%)", sum(income=="$35,000 - $49,999", na.rm=T), sum(income=="$35,000 - $49,999", na.rm=T)/nrow(data)*100),
  `$50,000 - $74,999`= sprintf("%i (%.1f%%)", sum(income=="$50,000 - $74,999", na.rm=T), sum(income=="$50,000 - $74,999", na.rm=T)/nrow(data)*100),
  `$75,000 - $99,999`= sprintf("%i (%.1f%%)", sum(income=="$75,000 - $99,999", na.rm=T), sum(income=="$75,000 - $99,999", na.rm=T)/nrow(data)*100),
  `$100,000 - $149,999`= sprintf("%i (%.1f%%)", sum(income=="$100,000 - $149,999", na.rm=T), sum(income=="$100,000 - $149,999", na.rm=T)/nrow(data)*100),
  `$150,000 - $199,999`= sprintf("%i (%.1f%%)", sum(income=="$150,000 - $199,999", na.rm=T), sum(income=="$150,000 - $199,999", na.rm=T)/nrow(data)*100),
  `$200,000 and above`= sprintf("%i (%.1f%%)", sum(income=="$200,000 and above", na.rm=T), sum(income=="$200,000 and above", na.rm=T)/nrow(data)*100),
  
  # married
  `Married` = sprintf("%i (%.1f%%)", sum(married == "Married"),
                      sum(married == "Married")/nrow(data)*100),
  `Was married` = sprintf("%i (%.1f%%)", sum(married == "Was married"),
                          sum(married == "Was married")/nrow(data)*100),
  `Never married` = sprintf("%i (%.1f%%)", sum(married == "Never married"),
                          sum(married == "Never married")/nrow(data)*100),
  # insur_type
  `Uninsured` = sprintf("%i (%.1f%%)", sum(insur_type == "No insurance"),
                        sum(insur_type == "No insurance")/nrow(data)*100),
  # employment
  `Unemployed` = sprintf("%i (%.1f%%)", sum(employment == "Unemployed"),
                         sum(employment=="Unemployed")/nrow(data)*100)
  )
  
  return(table_1 %>% t())
}

# helper function for CV for logistic regression
cost <- function(r, pi=0) mean(abs(r-pi)> 0.5)

pred_test <- function(mod, dat, var, logmod = F, cutoff=0.5){
  if(logmod){
    pred <- ifelse(predict(mod, newdata = dat, type = "response")>cutoff, 1, 0)
  }else{
    pred <- predict(mod, newdata = dat)
  }
  tab_log <- table(Predicted = pred, Expected = dat[,var])
  test_err_log <- (tab_log[1,2]+tab_log[2,1])/nrow(dat)
  
  print(sprintf("Test error rate = %.4f", test_err_log))
  
  return(list(test_err_log, tab_log))
}

# specificity
specificity <- function(mod, dat, var, cutoff = 0.5, logmod = F){
  if(logmod){
    pred <- ifelse(predict(mod, newdata = dat, type = "response")>cutoff, 1, 0)
  }else{
    pred <- predict(mod, newdata = dat)
  }
  tab_log <- table(Predicted = pred, Expected = dat[,var])
  fdr_log <- tab_log[1,1]/(tab_log[1,1]+tab_log[1,2])
  
  print(sprintf("Specificity rate  = %.4f", fdr_log))
  
  return(fdr_log)
}

#sensitivity
sensitivity <- function(mod, dat, var, cutoff = 0.5, logmod = F){
  if(logmod){
    pred <- ifelse(predict(mod, newdata = dat, type = "response")>cutoff, 1, 0)
  }else{
    pred <- predict(mod, newdata = dat)
  }
  tab_log <- table(Predicted = pred, Expected = dat[,var])
  for_log <- tab_log[2,2]/(tab_log[2,2]+tab_log[2,1])
  
  print(sprintf("Sensitivity rate  = %.4f", for_log))
  
  return(for_log)
}

