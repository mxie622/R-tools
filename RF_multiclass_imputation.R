# MultiClass imputation by RandomForest


# Start!!!!!!

library(dplyr)
library(e1071)
library(caret)
library(MASS)
library(randomForest)
#--------------------------------



RF_imputation_multiclass <- function(data, variables, outcome, mtry){
  # data : Raw dataset; data.frame
  # variables : Column names in df; Must be numeric
  # outcome : 'variable' of 'data' to be filled; categorical variable 
  # mtry : Number of variables randomly sampled. 

  if (missing(mtry)){
    mtry <- sqrt(ncol(data) - 1)
  }
  data$index_mikexie = 1 : nrow(data)
  No_NA_df <- data[complete.cases(data), ] # Complete set with no NA
  NA_df <- data[!complete.cases(data), ] # Take a subset
  rfgrid <- expand.grid(.mtry = mtry)

  f <- as.formula(
    paste(outcome,
          paste(variables, collapse = " + "),
          sep = " ~ "))
  
  par_rf <- train(form = f, 
                  data = No_NA_df, 
                  method = "rf", 
                  metric = 'Accuracy', 
                  tuneGrid = rfgrid)
  
  best_mtry = par_rf$results$mtry[which(par_rf$results$Accuracy == max(par_rf$results$Accuracy))][1]

  model_to_fill_NA = randomForest(f, data = No_NA_df, mtry = best_mtry)

  temp0 <- NA_df[rowSums(is.na(NA_df[!names(NA_df) %in% c(outcome)])) == 0, ][variables]
  
  NA_df[rowSums(is.na(NA_df[!names(NA_df) %in% c(outcome)])) == 0, ][, outcome] = predict(model_to_fill_NA, temp0)
  
  new_df = rbind(NA_df, No_NA_df)
  new_df = new_df[order(new_df$index_mikexie), ]
  new_df$index_mikexie = NULL
  new_df
}

# -----------------------------
# Example: KosteckiDillon data
# Dataset setting

data('KosteckiDillon')
rawdata = KosteckiDillon[sample(nrow(KosteckiDillon )), ] # Shuffle data
# rawdata$sex <- as.numeric(rawdata$sex) - 1
# Aids2$status <- as.numeric(Aids2$status) - 1

summary(rawdata) # Check which variable is binary.

splitIndex <- createDataPartition(rawdata[, 2], p = .8, list = FALSE, times = 1)
df = df_train <- rawdata[splitIndex, ] # df : imputation set; df_train : Raw training set
df_test  <- rawdata[-splitIndex, ] # Testing set for imputation quality


# categorical variables are sex and status
categorical_variables = c('hatype', 'medication', 'headache', 'sex')
df[11 : 30, categorical_variables[1]] = NA
df[401 : 500, categorical_variables[2]] = NA
df[2001 : 2100, categorical_variables[3]] = NA
df[3000 : 3100, categorical_variables[4]] = NA
summary(df)
data <- df
nrow(df[complete.cases(df), ]) # completed samples are 3002 ****

length(which(df$hatype != df_train$hatype)) # 

outcome <- categorical_variables[1]
variables <- colnames(df[, c(2, 3, 5, 6)])

new_df <- RF_imputation_multiclass(data = df, 
                                   outcome = categorical_variables[1], 
                                   variables = colnames(df[, c(2, 3, 5, 6)]))
nrow(new_df[complete.cases(new_df), ]) # completed samples are  3022 ****

length(which(new_df$hatype != df_train$hatype)) # 1 misclassified.

multiple_df <- list()
multiple_df[[1]] <- new_df
categorical_variables

# [1] "hatype"     "medication" "headache"   "sex" 
j = 2


for (i in 1 : length(categorical_variables)){ # Predictors must be numeric
  multiple_df[[j]] <- RF_imputation_multiclass(data = multiple_df[[j - 1]],
                                            outcome = categorical_variables[i],
                                            variables = colnames(df[, c(2, 3, 5, 6)]))
  j = j + 1
  i = i + 1
}

final_set = multiple_df[[j - 1]]

# Check
summary(final_set[complete.cases(final_set), ]) # completed samples have no NA. ****

# Check imputation quality, comparied to the raw data : See the difference between final_set and df_train

length(which(final_set$hatype != df_train$hatype)) # 0 / 20 difference on variable 'hatype'
length(which(final_set$medication != df_train$medication)) # 1 / 100 difference on variable 'medication'
length(which(final_set$headache != df_train$headache)) # 30 / 100 difference on variable 'headache'
length(which(final_set$sex != df_train$sex)) # 1 / 101 difference on variable 'sex'

# Most results are reliable

# End!!!!
