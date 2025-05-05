install.packages(c("xgboost", "caret", "Matrix"))

# Load the packages
library(xgboost)
library(caret)
library(Matrix)
library(plm)
library(dplyr)


# upload panel data
pdata <- read.csv("Capstone/capstone/panel_data.csv", check.names = FALSE)
pdata <- pdata.frame(pdata, index = c("State", "Year"))


pdata_complete <- pdata

# remove rPCE from predictors
predictors <- pdata_complete %>%
  select(-rPCE) %>%
  as.data.frame()

# convert to matrix format for XGBoost
predictors_matrix <- model.matrix(~ . - 1, data = predictors) # -1 is to remove intercept

# target variable
target <- pdata_complete$rPCE

# seed for reproducibility
set.seed(123)

# train/test split indices (80% train, 20% test)
train_index <- createDataPartition(target, p = 0.8, list = FALSE)





# First, check the dimensions of your data
dim(predictors_matrix)
length(target)

# Check the maximum index in train_index
max(train_index)



# Split the data
X_train <- predictors_matrix[train_index,]
X_test <- predictors_matrix[-train_index,]
y_train <- target[train_index]
y_test <- target[-train_index]

# Create DMatrix objects (XGBoost's optimized data structure)
dtrain <- xgb.DMatrix(data = X_train, label = y_train)
dtest <- xgb.DMatrix(data = X_test, label = y_test)

# Set XGBoost parameters
params <- list(
  objective = "reg:squarederror",  # For regression
  eta = 0.1,                       # Learning rate
  max_depth = 6,                   # Maximum tree depth
  subsample = 0.8,                 # Subsample ratio of training instances
  colsample_bytree = 0.8           # Subsample ratio of columns for each tree
)

# Train the model
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 100,                  # Number of boosting rounds
  watchlist = list(train = dtrain, test = dtest),
  early_stopping_rounds = 10,     # Stop if no improvement for 10 rounds
  verbose = 1
)

# Make predictions
predictions <- predict(xgb_model, dtest)

# Evaluate the model
rmse <- sqrt(mean((predictions - y_test)^2))
mae <- mean(abs(predictions - y_test))
r_squared <- 1 - (sum((y_test - predictions)^2) / sum((y_test - mean(y_test))^2))

# Print evaluation metrics
print(paste("RMSE:", round(rmse, 2)))
print(paste("MAE:", round(mae, 2)))
print(paste("R-squared:", round(r_squared, 2)))

# Get feature importance
importance_matrix <- xgb.importance(feature_names = colnames(X_train), model = xgb_model)

# Plot feature importance
xgb.plot.importance(importance_matrix, top_n = 10)

# Plot the first tree
xgb.plot.tree(model = xgb_model, trees = 0)

# Perform cross-validation
cv_results <- xgb.cv(
  params = params,
  data = dtrain,
  nrounds = 100,
  nfold = 5,               # 5-fold cross-validation
  showsd = TRUE,
  stratified = FALSE,      # No stratification for regression
  early_stopping_rounds = 10,
  verbose = 1
)

# Get the best number of rounds
best_nrounds <- cv_results$best_iteration
print(paste("Best number of rounds:", best_nrounds))
print(paste("Best number of rounds:", best_nrounds))
