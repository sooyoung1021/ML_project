# check for missingness

# outcome vector
outcomes <- c("fin_hardship",
                "housing_insecurity",
                "food_insecurity",
                "stimulus")
              
# predictor vector
features <- colnames(hp_short)[!colnames(hp_short) %in% c("fin_hardship",
                                                          "housing_insecurity",
                                                          "food_insecurity",
                                                          "stimulus",
                                                          "EST_ST")]


# identify variables with missing observations
missing <- rep(NA, length(features))
for(i in 1:length(features)){
  missing[i] <- ifelse(sum(is.na(hp_short[,features[i]]))==0,F, T)
}

print("features with missing observations:")
features[missing]
print("features without missing observatons:")
features[!missing]

# Create missingness vector
missing_matrix <- matrix(nrow = nrow(hp_short),
                         ncol = length(features[missing]))

for(i in 1:length(features[missing])){
  missing_matrix[,i] <- ifelse(is.na(hp_short[,features[missing][i]]), 1, 0)
}

missing_corr <- data.frame(
  var1 = character(),
  var2 = character(),
  coef = numeric(),
  pval = numeric()
)

cat_var <- c("gender", "income", "married","insur_type", "employment", "ui_benefit", "ssi_benefit", "housing", "race", "educ", "region")

cat_result <- list()
cat_ind <- 2
for(i in 1:length(features[missing])){
  for(var in c(features, outcomes)){
    if(features[missing][i] == var) next
    if(var %in% cat_var){
      temp <- glm(missing_matrix[,i] ~ hp_short[,var], family = binomial)
      if(sum(summary(temp)$coefficients[-1,4]<0.05, na.rm=T)>0){
        missing_corr %<>% rbind(data.frame(var1 = features[missing][i],
                                           var2 = sprintf("%s (categorical)", var),
                                           coef = sprintf("Refer to Table S.%i", cat_ind),
                                           pval = NaN))
        cat_result[[paste0(features[missing][i], "~", var)]] <- temp
        cat_ind <- cat_ind+1
      }
    }else{
      temp <- cor.test(missing_matrix[,i], hp_short[,var])
      missing_corr %<>% rbind(data.frame(var1 = features[missing][i], 
                                         var2 = var,
                                         coef = round(temp$estimate,3),
                                         pval = round(temp$p.value, 3)))
    }
  }
}

