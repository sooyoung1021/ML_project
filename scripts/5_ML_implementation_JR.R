library(tidyverse)
library(MASS)

# dat=read.csv("pulse2021_puf_25.csv")
# hp_short <- hp_new %>% select(
#   # outcome vars
#   fin_hardship,
#   
#   # predictors
#   age,
#   gender,
#   race,
#   educ,
#   income,
#   married,
#   insur_type,
#   covid_hist,
#   job_loss,
#   employment,
#   ui_benefit,
#   ssi_benefit,
#   urban,
#   phq4,
#   housing,
#   region
# )
# 
# # outcome vector
# outcomes <- c("fin_hardship")
# 
# hp_short %<>% 
#   mutate_if(is.character, as.factor) %>% na.omit()
# 
# hp_short$fin_hardship = as.factor(hp_short$fin_hardship)
# 
# write.csv(hp_short, "hp_short.csv")

hp_short = read.csv("/Users/jessicarandazzo/OneDrive - nyu.edu/Spring 2021/Machine Learning/project/hp_short.csv")
hp_short = hp_short %>% subset(select=(-X)) %>%  mutate_if(is.character, as.factor)
hp_short$fin_hardship=as.factor(hp_short$fin_hardship)

# divide the data into train vs. test set (75:25)
N <- nrow(hp_short)
ind_train <- sample(c(rep(F,10152),rep(T, N-10152)), N, rep = F)

hp_train <- hp_short[ind_train,]
hp_test <- hp_short[!ind_train,]


##################################################
######## LINEAR DISCRIMINANT ANALYSIS  ###########
##################################################

#financial hardship outcome
set.seed(1)
lda.fit = lda(fin_hardship ~ ., data=hp_train)
lda.fit
 #89% of the training obs correspond to no financial hardship

set.seed(1)
lda.predict = predict(lda.fit, hp_train) 
lda.class = lda.predict$class
table(lda.class, hp_train$fin_hardship)
mean(lda.class!=hp_train$fin_hardship)
  #missclass = 10.3% 

saveRDS(lda.fit, file = "ldafit.rds")


#LDA is popular when we have more than 2 response classes. LDA is more stable 
  #when n is small. LDA is better than logistic when the classes are well separated.
  #probably not necessary to use LDA for our data based on these characertistics of LDA.


##################################################
####### QUADRATIC DISCRIMINANT ANALYSIS  #########
##################################################

#financial hardship outcome
set.seed(1)
qda.fit=qda(fin_hardship ~ ., data=hp_train)
qda.predict = predict(qda.fit, hp_train) 
qda.class = qda.predict$class
table(qda.class, hp_train$fin_hardship)
mean(qda.class!=hp_train$fin_hardship)
  #18.7% missclassification

saveRDS(qda.fit, file = "qdafit.rds")

##################################################
### TREES (classification, random forests,etc) ###
##################################################
library(tree)
##### Classification trees
set.seed(1)
tree.fin = tree(fin_hardship~., data=hp_train)
summary(tree.fin) #missclass= 10.5%
plot(tree.fin)
text(tree.fin)

  #using test and training data
set.seed(1)
tree.pred=predict(tree.fin ,hp_test ,type ="class")
table(tree.pred, hp_test$fin_hardship)
mean(tree.pred!=hp_test$fin_hardship) #missclass = 10.9%

saveRDS(tree.fin, file = "tree.rds")

  #cross validation to prune the tree
set.seed(1)
cv.fin = cv.tree(tree.fin, FUN=prune.misclass)
cv.fin
plot(cv.fin$size, cv.fin$dev, type="b")
plot(cv.fin$k, cv.fin$dev, type="b")

  #tree with 7 or 4 nodes has smallest cross-validation errors. original had 7
  #nodes so pruning may not be necessary. 

  # pruned tree
set.seed(1)
prune.fin =prune.misclass(tree.fin ,best =4)
plot(prune.fin)
text(prune.fin,pretty =0)

set.seed(1)
tree.pred=predict(prune.fin, hp_test ,type="class")
table(tree.pred, hp_test$fin_hardship)
mean(tree.pred!=hp_test$fin_hardship)
  #pruning to 4 results in same misclass error rate

###### Bagging
library(randomForest)

bag.fin = randomForest(fin_hardship~., data=hp_train, mtry=16, importance=T)
bag.fin

set.seed(1)
yhat.bag = predict(bag.fin, newdata=hp_test, type="class")
table(yhat.bag, hp_test$fin_hardship)
mean(yhat.bag!=hp_test$fin_hardship) #missclass = 10.4%

saveRDS(bag.fin, file = "bagging.rds")

###### Random Forests
set.seed(1)
rf.fin = randomForest(fin_hardship ~.,data=hp_train, importance=TRUE)
rf.fin

saveRDS(rf.fin, file = "randomforest.rds")

yhat.rf = predict(rf.fin, newdata=hp_test, type="class")
table(yhat.rf, hp_test$fin_hardship)
mean(yhat.rf!=hp_test$fin_hardship)  #missclass=10% - better than both tree methods

importance(rf.fin)
varImpPlot(rf.fin, main="Variable Importance on Financial Hardship in the Last 7 Days")
  #most important vars are phq4, job loss, age, income, insur_type, employment



#### Random forest leads to classification with smallest error rate. 

