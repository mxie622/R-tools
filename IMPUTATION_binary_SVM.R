



# Binary imputation by SVM : Work where exists single NA in a row **** 


# Start!!!!!!


library(e1071)
library(caret)
library(MASS)
data(Aids2)

#--------------------------------

SVM_imputation_binary <- function(data, variables, outcome, method, kernel, C){
  # data : Raw dataset; data.frame
  # variables : Column names in df; Must be numeric
  # outcome : 'variable' of 'df' to be filled; numeric variable 
  # method : Option: 'svmRadial',  'svmLinear', 'svmPoly'
  # kernel : Option: 'radial',  'linear', 'polynomial'
  # C : Possible values; For example c(0.05,0.0456,0.0577) # 
  data$index_mikexie = 1 : nrow(data)
  if (missing(C)){
    C = seq(1.6, 2.2, by = 0.2)
  }
  
  if (missing(method)){
    kernel = 'radial'
    method = 'svmRadial'
    SVMgrid <- expand.grid(sigma = seq(0.04, 0.08, by = 0.02), 
                           C = C)
  }
  if (method == 'svmLinear'){
    kernel = 'linear'
    SVMgrid <- expand.grid(C = C)
  }
  if (method == 'svmRadial'){
    kernel = 'radial'
    SVMgrid <- expand.grid(sigma = seq(0.04, 0.08, by = 0.02), 
                           C = C)
  }
  if (method == 'svmPoly'){
    kernel = 'polynomial'
    SVMgrid <- expand.grid(degree = 2:4, 
                           scale = .1,
                           C = C)
  }
  
  No_NA_df <- data[complete.cases(data), ] # Complete set with no NA
  NA_df <- data[!complete.cases(data), ] # Take a subset
  
  f <- as.formula(
    paste(outcome,
          paste(variables, collapse = " + "),
          sep = " ~ "))
  par_svm = train(form = f,
        method = method,
        tuneGrid = SVMgrid, 
        data = No_NA_df)
  
  best_degree = par_svm$results$degree[which(par_svm$results$Accuracy == max(par_svm$results$Accuracy))][1]
  best_cost = par_svm$results$C[which(par_svm$results$Accuracy == max(par_svm$results$Accuracy))][1]
  
  if (kernel == 'linear'){
    model_to_fill_NA = svm(f, data = No_NA_df, kernel = kernel, cost = best_cost)
  }
  if (kernel == 'radial'){
    model_to_fill_NA = svm(f, data = No_NA_df, kernel = kernel, cost = best_cost)
  }
  if (kernel == 'polynomial'){
    model_to_fill_NA = svm(f, data = No_NA_df, kernel = kernel, degree = best_degree, cost = best_cost)
  }
  
  
  temp0 <- NA_df[rowSums(is.na(NA_df[!names(NA_df) %in% c(outcome)])) == 0, ][variables]
  
  NA_df[rowSums(is.na(NA_df[!names(NA_df) %in% c(outcome)])) == 0, ][, outcome] = predict(model_to_fill_NA, temp0)
  
  new_df = rbind(NA_df, No_NA_df)
  new_df = new_df[order(new_df$index_mikexie), ]
  new_df$index_mikexie = NULL
  new_df
  
}

# -----------------------------
# Example: Aids data
# Dataset setting
library(dplyr)
set.seed(100)
data('Aids2')
rawdata = Aids2[sample(nrow(Aids2)), ] # Shuffle data
# rawdata$sex <- as.numeric(rawdata$sex) - 1
# Aids2$status <- as.numeric(Aids2$status) - 1

summary(rawdata) # Check which variable is binary.

splitIndex <- createDataPartition(rawdata[, 2], p = .8, list = FALSE, times = 1)
df = df_train <- rawdata[splitIndex, ] # df : imputation set; df_train : Raw training set
df_test  <- rawdata[-splitIndex, ] # Testing set for imputation quality

# Binary variables are sex and status
binary_variables = c('sex', 'status')
df[1:5, binary_variables[1]] = NA
df[6:100, binary_variables[2]] = NA
summary(df)
nrow(df[complete.cases(df), ]) # completed samples are 2176 ****

new_df <- SVM_imputation_binary(data = df, outcome = binary_variables[2], variables = colnames(df)[3:4])
nrow(new_df[complete.cases(new_df), ]) # completed samples are  2236 ****

length(which(new_df$status != df_train$status)) # There are 4 misclassified.

multiple_df <- list()
multiple_df[[1]] <- new_df
binary_variables

# [1] "sex"    "status"
j = 2


for (i in 1 : length(binary_variables)){ # Predictors must be numeric 
  multiple_df[[j]] <- SVM_imputation_binary(data = multiple_df[[j - 1]], 
                                            outcome = binary_variables[i], 
                                            variables = colnames(df)[3 : 4])
  j = j + 1
  i = i + 1
}

final_set = multiple_df[[3]]

# Check
summary(final_set[complete.cases(final_set), ]) # completed samples have no NA. **** 

# Check imputation quality, comparied to the raw data : See the difference between final_set and df_train

length(which(final_set$sex != df_train$sex)) # 0 difference on variable 'sex'
length(which(final_set$status != df_train$status)) # 4 difference on variable 'status'

# 91 / 95 = 95.78% correct rate

# End!!!!




