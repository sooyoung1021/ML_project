# load models
bagging <- readRDS(here::here("results", "bagging.rds"))
lda <- readRDS(here::here("results", "ldafit.rds"))
qda <- readRDS(here::here("results", "qdafit.rds"))
rf <- readRDS(here::here("results", "randomforest.rds"))
tree <- readRDS(here::here("results", "tree.rds"))

# load packages
pacman::p_load(randomForest, MASS, tree)

## Bagging
# Test error
pred_test(bagging, hp_test, "fin_hardship")
# specificity and sesntivitiy
specificity(bagging, hp_test, "fin_hardship")
sensitivity(bagging, hp_test, "fin_hardship")

## Random forest
pred_test(rf, hp_test, "fin_hardship")
# specificity and sesntivitiy
specificity(rf, hp_test, "fin_hardship")
sensitivity(rf, hp_test, "fin_hardship")

## LDA
pred_test(lda, hp_test, "fin_hardship", type = "lda")
# specificity and sesntivitiy
specificity(lda, hp_test, "fin_hardship", type = "lda")
sensitivity(lda, hp_test, "fin_hardship", type = "lda")

## QDA
pred_test(qda, hp_test, "fin_hardship", type = "lda")
# specificity and sesntivitiy
specificity(qda, hp_test, "fin_hardship", type = "lda")
sensitivity(qda, hp_test, "fin_hardship", type = "lda")

## Tree
pred_test(tree, hp_test, "fin_hardship", type = "class")
# specificity and sesntivitiy
specificity(tree, hp_test, "fin_hardship", type = "class")
sensitivity(tree, hp_test, "fin_hardship", type = "class")
