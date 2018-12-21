##### Imputation by KNN

# Start!!!!!!

#------------------------

KNN_imputation_numeric = function(df, outcome, k){
  # df : Raw dataset; data.frame
  # Outcome : 'variable' of 'df' to be filled; numeric variable 
  # k : The number of nearest neighbour; a number or a sequence of possible k
  
  # outcome <- 'Sepal.Length' # To fill
  variables <- names(df[!names(df) %in% c(outcome)]) # predictors
  f <- as.formula(
    paste(outcome, 
          paste(variables, collapse = " + "), 
          sep = " ~ "))
  
  library(caret)
  # Split raw data into 2 sets
  No_NA_df <- df[complete.cases(df), ] # Complete set with no NA
  NA_df <- df[!complete.cases(df), ] # Take a subset
  selecting_k <- train(form = f,
        method = "knn",
        data = No_NA_df,
        tuneGrid = expand.grid(k = k)) # Select the best k
  
  best_k = selecting_k$results$k[which(selecting_k$results$RMSE == min(selecting_k$results$RMSE))] # Select the best k with minimum RMSE
  model_to_fill_NA <- knnreg(formula = f, data = No_NA_df, k = best_k)
  
  temp0 <- NA_df[rowSums(is.na(NA_df[!names(NA_df) %in% c(outcome)])) == 0, ]

  NA_df[rowSums(is.na(NA_df[!names(NA_df) %in% c(outcome)])) == 0, ][, outcome] = predict(model_to_fill_NA, temp0)
  
  rbind(NA_df, No_NA_df)
}


#--------------------------------
# Example: Iris data
# Dataset setting
library(dplyr)

data("iris")
iris = iris[sample(nrow(iris)), ] # Shuffle data
df = df_train = iris[1:100, ] # df : imputation set; df_train : Raw training set
df_test = iris[101 : nrow(iris), ] # Testing set for imputation quality

df[1:5, 1] = NA
df[11:15, 2] = NA
df[21:25, 3] = NA
df[31:35, 4] = NA

nrow(df[complete.cases(df), ]) # completed samples are 80 ****


# 1 variable filling 
new_df <- KNN_imputation_numeric(df = df, outcome = 'Sepal.Length', k = c(1, 3, 7, 9))
nrow(new_df[complete.cases(new_df), ]) # completed samples are 85 ****

# Multiple filling *************

multiple_df <- list()
multiple_df[[1]] <- new_df
variables <- names(new_df)
variables 
# [1] "Sepal.Length" "Sepal.Width"  "Petal.Length" "Petal.Width"  "Species" 
j = 2

for (i in 1 : (length(variables) - 1)){ # Species is not a numeric variable.
  multiple_df[[j]] <- KNN_imputation_numeric(df = multiple_df[[j-1]], outcome = variables[i], k = c(1,3,7,9))
  j = j + 1
  i = i + 1
}

final_set = multiple_df[[5]]
# final_set[,1:4] - iris[, 1:4]
# Check
summary(final_set[complete.cases(final_set), ]) # completed samples have no NA. **** 

# Check imputation quality, comparied to the raw data
library(rpart)
model_imputation <- rpart(Species ~., data = final_set)
model_raw <- rpart(Species ~., data = df_train)

prediction_imputation <- predict(model_imputation, df_test, type = 'class')
prediction_raw <- predict(model_raw, df_test, type = 'class')

table(prediction_imputation, df_test$Species)
table(prediction_raw, df_test$Species)

# Exactly the same
# End!!!!


