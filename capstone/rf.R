# libraries
library(randomForest)
library(Metrics)
library(plm)


# upload panel data
pdata <- read.csv("Capstone/capstone/panel_data.csv", check.names = FALSE)
pdiff <- read.csv("Capstone/capstone/panel_data_diff.csv", check.names = FALSE)

pdata <- pdata.frame(pdata, index = c("State", "Year"))
pdiff <- pdata.frame(pdiff, index = c("State", "Year"))
                     
# split into train/test
set.seed(123)
train_index <- sample(1:nrow(pdiff), 0.7 * nrow(pdiff))
train_data <- pdiff[train_index, ]
test_data <- pdiff[-train_index, ]

# train random forest model
rf_model <- randomForest(
  rPCE ~ electricity + auto + ng + house_own + food,
  data = train_data,
  importance = TRUE,
  ntree = 500
)

# predict
predictions <- predict(rf_model, newdata = test_data)
actual <- test_data$rPCE

# Show variable importance
importance(rf_model)
varImpPlot(rf_model)

# accuracy metrics
rmse <- sqrt(mean((predictions - actual)^2))
mae <- mean(abs(predictions - actual))
mape <- mean(abs((predictions - actual) / actual)) * 100
r_squared <- 1 - (sum((actual - predictions)^2) / sum((actual - mean(actual))^2))

# Print metrics
cat("RMSE:", rmse, "\n")
cat("MAE:", mae, "\n")
cat("MAPE:", mape, "%\n")
cat("R-squared:", r_squared, "\n")