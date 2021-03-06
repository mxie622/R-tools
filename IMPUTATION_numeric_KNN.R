



##### Imputation by KNN: Work where exists single NA in a row **** 

# Start!!!!!!

#------------------------
set.seed(100)
KNN_imputation_numeric = function(df, outcome, variables, k){
  # df : Raw dataset; data.frame
  # outcome : 'variable' of 'df' to be filled; numeric variable 
  # variables: predictors # Be careful when filling multiple NA from different columns
  # k : The number of nearest neighbour; a number or a sequence of possible k
  
  # outcome <- 'Sepal.Length' # To fill for example
  
  df$index_mikexie = 1 : nrow(df)
  if (missing(variables)){
    variables = names(df[!names(df) %in% c(outcome)])
    variables = variables[-length(variables)]
  }
  df2 = df[c(variables, outcome, 'index_mikexie')]
  
  f <- as.formula(
    paste(outcome, 
          paste(variables, collapse = " + "), 
          sep = " ~ "))
  
  library(caret)
  # Split raw data into 2 sets
  No_NA_df <- df2[complete.cases(df2), ] # Complete set with no NA
  NA_df <- df2[!complete.cases(df2), ] # Take a subset
  selecting_k <- train(form = f,
                       method = "knn",
                       data = No_NA_df,
                       tuneGrid = expand.grid(k = k)) # Select the best k
  
  best_k = selecting_k$results$k[which(selecting_k$results$RMSE == min(selecting_k$results$RMSE))] # Select the best k with minimum RMSE
  model_to_fill_NA <- knnreg(formula = f, data = No_NA_df, k = best_k)
  
  temp0 <- NA_df[rowSums(is.na(NA_df[!names(NA_df) %in% c(outcome)])) == 0, ][variables]
  
  NA_df[rowSums(is.na(NA_df[!names(NA_df) %in% c(outcome)])) == 0, ][, outcome] = predict(model_to_fill_NA, temp0)
  
  new_df = rbind(NA_df, No_NA_df)
  new_df = new_df[order(new_df$index_mikexie), ]
  df[, outcome] = new_df[, outcome]
  df$index_mikexie = NULL
  return(df)
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


# 1 variable filling ************
new_df <- KNN_imputation_numeric(df = df, 
                                 outcome = 'Sepal.Length', 
                                 k = c(1, 3, 7, 9))
nrow(new_df[complete.cases(new_df), ]) # completed samples are 85 ****

# Multiple filling *************

multiple_df <- list()
multiple_df[[1]] <- new_df
variables <- names(new_df)
variables 
# [1] "Sepal.Length" "Sepal.Width"  "Petal.Length" "Petal.Width"  "Species" 
j = 2

for (i in 1 : (length(variables) - 1)){ # Species is not a numeric variable.
  multiple_df[[j]] <- KNN_imputation_numeric(df = multiple_df[[j - 1]], 
                                             outcome = variables[i],                                              
                                             k = c(1, 3, 7, 9))
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

# prediction_imputation setosa versicolor virginica
# setosa         16          0         0
# versicolor      0         17         1
# virginica       0          2        14

table(prediction_raw, df_test$Species)

# prediction_raw setosa versicolor virginica
# setosa         21          0         0
# versicolor      0         13         2
# virginica       0          1        13



# Prediction even better than the raw data
# End!!!!


