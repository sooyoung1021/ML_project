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

hp_short = read.csv("hp_short.csv")

# divide the data into train vs. test set (75:25)
N <- nrow(hp_short)
ind_train <- sample(c(rep(F,10152),rep(T, N-10152)), N, rep = F)

hp_train <- hp_short[ind_train,]
hp_test <- hp_short[!ind_train,]

##################################################
##########    LOGISTIC REGRESSION  ###############
##################################################
glm.fit1 = glm(fin_hardship ~ ., data= hp_short, family='binomial');
summary(glm.fit1)

glm.prob1 = predict(glm.fit1, type = 'response')
glm.predict1 = ifelse(glm.prob1 > 0.5, 1, 0)
table(glm.predict1, hp_short$fin_hardship)
mean(glm.predict1 !=hp_short$fin_hardship)
  #10% missclassification (miss more that are having a financial hardship than not)


##################################################
######## LINEAR DISCRIMINANT ANALYSIS  ###########
##################################################

#financial hardship outcome
lda.fit = lda(fin_hardship ~ ., data=hp_train)
 #88% of the training obs correspond to no financial hardship

lda.predict = predict(lda.fit, hp_train) 
lda.class = lda.predict$class
table(lda.class, hp_train$fin_hardship)
mean(lda.class!=hp_train$fin_hardship)
  #missclass = 10.1% 

#LDA is popular when we have more than 2 response classes. LDA is more stable 
  #when n is small. LDA is better than logistic when the classes are well separated.
  #probably not necessary to use LDA for our data based on these characertistics of LDA.


##################################################
####### QUADRATIC DISCRIMINANT ANALYSIS  #########
##################################################

#financial hardship outcome
qda.fit=qda(fin_hardship ~ ., data=hp_train)
qda.predict = predict(qda.fit, hp_train) 
qda.class = qda.predict$class
table(qda.class, hp_train$fin_hardship)
mean(qda.class!=hp_train$fin_hardship)
  #18.6% missclassification


##################################################
### TREES (classification, random forests,etc) ###
##################################################
library(tree)
##### Classification trees
tree.fin = tree(fin_hardship~., data=hp_train)
summary(tree.fin) #missclass= 10.5%
plot(tree.fin)
text(tree.fin)

  #using test and training data
tree.pred=predict (tree.fin ,hp_test ,type ="class")
table(tree.pred, hp_test$fin_hardship)
mean(tree.pred!=hp_test$fin_hardship) #missclass = 10.9%

  #cross validation to prune the tree
set.seed(1)
cv.fin = cv.tree(tree.fin, FUN=prune.misclass)
cv.fin
plot(cv.fin$size, cv.fin$dev, type="b")
plot(cv.fin$k, cv.fin$dev, type="b")

  #tree with 7 or 4 nodes has smallest cross-validation errors. original had 7
  #nodes so pruning may not be necessary. 

###### Bagging
library(randomForest)

bag.fin = randomForest(fin_hardship~., data=hp_train, mtry=16, importance=T)
bag.fin

yhat.bag = predict(bag.fin, newdata=hp_test, type="class")
table(yhat.bag, hp_test$fin_hardship)
mean(yhat.bag!=hp_test$fin_hardship) #missclass = 11.1%


###### Random Forests
set.seed(1)
rf.fin = randomForest(fin_hardship ~.,data=hp_train, importance=TRUE)
rf.fin

yhat.rf = predict(rf.fin, newdata=hp_test, type="class")
table(yhat.rf, hp_test$fin_hardship)
mean(yhat.rf!=hp_test$fin_hardship)  #missclass=10.5% - slightly better than bagging,
                                      #but similar to classification trees

importance(rf.fin)
varImpPlot(rf.fin)
  #most important vars are phq4, job loss, age, income, insur_type, employment

###### Boosting
library(gbm)

set.seed(1)
boost.fin =gbm(fin_hardship~., data=hp_train, distribution ="bernoulli", n.trees=500, 
               interaction.depth = 10)
summary(boost.fin) #idk why this doesnt work

par(mfrow=c(1,2))
#plot(boost.fin, i="rm")
#plot(boost.fin, i="lstat")

#yhat.boost = predict(boost.fin, newdata=hp_test, n.trees=500)
#table(yhat.boost, hp_test$fin_hardship)
#mean(yhat.boost!=hp_test$fin_hardship)





#### Random forest leads to classification with smallest error rate. ?

