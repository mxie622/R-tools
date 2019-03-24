


# ( Problematic for some cases!!!!!!!!!!!!!!!!!!!!!!!!!)




# Categorical imputation by KNN

#  Filling multiple missing values of a row **** Predictor NOT necessary to be NUMERIC


# Start!!!!!!

library(e1071)
library(caret)
library(MASS)
library(randomForest)
#--------------------------------

set.seed(20181227)

KNN_imputation_multi_NA <- function(data, outcome, k){

  # data : Raw dataset; data.frame
  # outcome : 'variable' of 'data' to be filled; factorial variable 

  # k : The number of nearest neighbour; a number or a sequence of possible k
  
  # outcome <- 'medication' # To fill for example

  data$index_mikexie = 1 : nrow(data)
  
  library(caret)
  # Split raw data into 2 sets
  No_NA_df <- data[complete.cases(data), ] # Complete set with no NA
  NA_df <- data[!complete.cases(data), ] # Take a subset
  
  temp_df <- NA_df[!names(NA_df) %in% c(outcome)] #*****
  temp_df <- temp_df[-ncol(temp_df)] #******
  No_NA_columns <- temp_df[!apply(temp_df, 2, function(x) any(is.na(x)))] #***** Non-NA variables to predict
  NA_columns <- temp_df[apply(temp_df, 2, function(x) any(is.na(x)))] 

  f <- as.formula(
    paste(outcome, 
          paste(colnames(No_NA_columns), collapse = " + "), 
          sep = " ~ "))
  
  selecting_k <- train(form = f,
                       method = "knn",
                       data = No_NA_df,
                       tuneGrid = expand.grid(k = k)) # Select the best k
  
  best_k = selecting_k$results$k[which(selecting_k$results$Accuracy == max(selecting_k$results$Accuracy))] # Select the best k with minimum RMSE
  model_to_fill_NA <- knn3(formula = f, data = No_NA_df, k = best_k)
  
  temp0 <- NA_df[, colnames(No_NA_columns)]
  prediction <- round(predict(model_to_fill_NA, temp0))
  i = 1
  output = character()
  for (i in 1 : nrow(prediction)){
    output[i] = ifelse((rowSums(prediction) == 1)[i], 
                       names(which(prediction[i, ] == 1)),
                       names(prediction[1, ])[1])
  }
  NA_df[rowSums(is.na(NA_df[!names(NA_df) %in% c(outcome, names(NA_columns))])) == 0, ][, outcome] = output

  new_df = rbind(NA_df, No_NA_df)
  new_df = new_df[order(new_df$index_mikexie), ]
  new_df$index_mikexie = NULL
  new_df
}

# -----------------------------
# Example: KosteckiDillon data
# Dataset setting
library(carData)
data('KosteckiDillon')
rawdata = KosteckiDillon[sample(nrow(KosteckiDillon)), ] # Shuffle data

summary(rawdata) # Check which variable is binary.

splitIndex <- createDataPartition(rawdata[, 2], p = .8, list = FALSE, times = 1)
df = df_train <- rawdata[splitIndex, ] # df : imputation set; df_train : Raw training set
df_test  <- rawdata[-splitIndex, ] # Testing set for imputation quality


# predictors

df[1 : 100, 7] = NA
df[1 : 100, 8] = NA
df[1 : 100, 9] = NA

summary(df)
data <- df
nrow(df[complete.cases(df), ]) # completed samples are 3223 ****

outcome <- colnames(df[, 7:9]) # 'medication', 'headache', 'sex'
# variables <- colnames(df[!names(df) %in% c(outcome)])

new_df <- KNN_imputation_multi_NA(data = df, 
                                  outcome = outcome[1], 
                                  k = c(1, 3, 5, 7))
summary(new_df[, outcome[1]]) # completed ****

length(which(new_df$medication != df_train$medication)) # 4 / 100 misclassified.

multiple_df <- list()
multiple_df[[1]] <- new_df

j = 2
i = 2
for (i in 2 : length(outcome)){ # Predictors must be numeric
  multiple_df[[j]] <- KNN_imputation_multi_NA(data = multiple_df[[j - 1]],
                                               outcome = outcome[i],
                                               k = seq(1, 7, 2))
  j = j + 1
  i = i + 1
}

final_set = multiple_df[[j - 1]]
final_set
# Check
summary(final_set[complete.cases(final_set), ]) # completed samples have no NA. ****

# Check imputation quality, comparied to the raw data : See the difference between final_set and df_train


length(which(final_set$medication != df_train$medication)) # 7 / 100 difference on variable 'medication'
length(which(final_set$headache != df_train$headache)) # 31 / 100 difference on variable 'headache'
length(which(final_set$sex != df_train$sex)) # 3 / 100 difference on variable 'sex'

# Most results are OK

# End!!!!


