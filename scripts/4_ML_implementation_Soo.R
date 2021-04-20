pacman::p_load(glmulti, boot,ggthemr, e1071)
ggthemr("fresh")

# I haven't done anything yet

# divide the data into train vs. test set (75:25)
N <- nrow(hp_short)
ind_train <- sample(c(rep(F,10152),rep(T, N-10152)), N, rep = F)

hp_train <- hp_short[ind_train,]
hp_test <- hp_short[!ind_train,]

# logistic regression (AIC-based model selection and 10-fold CV to choose the best model)

## prep the template to fill in the result
tab_err <- data.frame(
  formula = rep(NA, 10),
  cv.err = rep(NA, 10)
)

## subset selection function to keep 10 best models (takes long to run because running 2^15=32768 models)
mod_log <- glm(fin_hardship ~.,
               data = hp_train %>% select(-housing_insecurity, -food_insecurity, -stimulus),
               family = binomial)
  
choice_log <- glmulti(mod_log,
                      level = 1, # no interaction terms
                      method = "h", # exhaustive approach
                      crit = "aic",
                      confsetsize = 10 # keep 10 best models
                      ) # no interim reports or plot 


## 10-fold cross-validation to choose the best model              
for(i in 1:10){
  # formula
  tab_err$formula[i] <- choice_log@formulas[[i]] %>% as.character()%>% .[3]
  # 10-fold CV error
  mod <- glm(formula = as.formula(paste0(choice_log@formulas[[i]][c(2,3)], collapse="~")),
             data = hp_train,
             family = binomial)
  tab_err$cv.err[i] <-cv.glm(hp_train, mod, 
                             K=10,
                             cost = cost)$delta[1]
}

## plot the CV error 
tab_err %>% mutate(
  min = ifelse(cv.err == min(tab_err$cv.err), "Minimum", "")
  )%>%
ggplot(aes(factor(formula), cv.err, group=1)) + 
  geom_point(aes(color = min)) + geom_line() + 
  scale_y_continuous(limits = c(0, 0.11)) +
  scale_x_discrete(labels = paste0("Model ", 1:10))+
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Subset selection model") +
  ylab("10-fold Cross-validation error rate")


# Final model
mod_log_final <- glm(formula = as.formula(paste0(choice_log@formulas[[which.min(tab_err$cv.err)]][c(2,3)], collapse="~")),
                     data = hp_train,
                     family = binomial)


# Test error
pred_test(mod_log_final, hp_test, "fin_hardship", logmod=T)

# FDR and FOR
specificity(mod_log_final, hp_test, "fin_hardship", logmod=T)
sensitivity(mod_log_final, hp_test, "fin_hardship", logmod=T)


# see if changing the cutoff results in better performance
cut_change <- data.frame(
  cutoff = seq(0.1, 0.9, 0.05),
  test.err = rep(NA, length(seq(0.1, 0.9, 0.05))),
  Specificity = rep(NA, length(seq(0.1, 0.9, 0.05))),
  Sensitivity = rep(NA, length(seq(0.1, 0.9, 0.05)))
)

for(cut in seq(0.1, 0.9, 0.05)){
  cut_change$test.err[which(cut_change$cutoff==cut)] <-
    pred_test(mod_log_final, hp_test, "fin_hardship", cutoff = cut, logmod=T)[[1]]
  cut_change$Specificity[which(cut_change$cutoff==cut)] <-
    specificity(mod_log_final, hp_test, "fin_hardship", cutoff = cut, logmod=T)[[1]]
  cut_change$Sensitivity[which(cut_change$cutoff==cut)] <-
    sensitivity(mod_log_final, hp_test, "fin_hardship", cutoff = cut, logmod=T)[[1]]
}

ggplot(cut_change) + 
  geom_point(aes(cutoff, unlist(test.err), color = "Misclassification rate")) + geom_path(aes(cutoff, unlist(test.err), color = "Misclassification rate")) +
  geom_point(aes(cutoff, unlist(Specificity), color = "Specificity")) + geom_path(aes(cutoff, unlist(Specificity), color = "Specificity")) +
  geom_point(aes(cutoff, unlist(Sensitivity), color = "Sensitivity")) + geom_path(aes(cutoff, unlist(Sensitivity), color = "Sensitivity")) +
  xlab("Classification cutoff") +
  ylab("Test error rate") +
  theme(legend.position = c(0.8, 0.4))
  

# SVM - doesn't work with the full traning set

## subsetting the random ~ 10% of training set
hp_train_small <- hp_train[sample(1:nrow(hp_train), 3000, replace = F),] %>%
  select(-housing_insecurity, -food_insecurity, -stimulus)

## polynomial kernel
mod_svm <- tune(svm,
                factor(fin_hardship)~., 
                data = hp_train_small,
                kernel = "polynomial",
                ranges = list(
                  cost = c(0.1 ,1 ,10 ,100 ,1000),
                  gamma = c(0.5, 1, 2, 3, 4)
                ),
                scale = FALSE)

## radial kernel
mod_svm2 <- tune(svm,
                factor(fin_hardship)~., 
                data = hp_train_small,
                kernel = "radial",
                ranges = list(
                  cost = c(0.1 ,1 ,10 ,100 ,1000),
                  gamma = c(0.5, 1, 2, 3, 4)
                ),
                scale = FALSE)

## Linear kernel
mod_svm3 <- tune(svm,
                 factor(fin_hardship)~., 
                 data = hp_train_small,
                 kernel = "linear",
                 ranges = list(
                   cost = c(0.1 ,1 ,10 ,100 ,1000)
                 ),
                 scale = FALSE)

## compare the test error

### Radial kernel
# Test error
pred_test(mod_svm2$best.model, hp_test, "fin_hardship")

# FDR and FOR
specificity(mod_svm2$best.model, hp_test, "fin_hardship")
sensitivity(mod_svm2$best.model, hp_test, "fin_hardship")

### Linear kernel
# Test error
pred_test(mod_svm3$best.model, hp_test, "fin_hardship")

# FDR and FOR
specificity(mod_svm3$best.model, hp_test, "fin_hardship")
sensitivity(mod_svm3$best.model, hp_test, "fin_hardship")
