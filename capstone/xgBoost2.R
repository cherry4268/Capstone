# libraries
library(plm)
library(tseries)
library(purrr)
library(dplyr)
library(xgboost)


# upload panel data
pdata <- read.csv("Capstone/capstone/panel_data.csv", check.names = FALSE)
pdata$state <- as.factor(pdata$State)
pdata$year <- as.numeric(as.character(pdata$Year))
pdata <- pdata.frame(pdata, index = c("State", "Year"))

pdata %>% filter(if_any(everything(), is.na))


pdata <- pdata %>%
  group_by(State) %>%
  arrange(Year) %>%
  mutate(house_own_diff = house_own - lag(house_own),
         house_rent_diff = house_rent - lag(house_rent),
         elec_diff = electricity - lag(electricity),
         auto_diff = auto - lag(auto),
         ng_diff = ng - lag(ng),
         food_diff = food - lag(food)) %>%
  ungroup()
pdata <- pdata.frame(pdata, index = c("State", "Year"))

# Filter out NA rows due to differencing
pdata_clean <- pdata %>%
  filter(
    !is.na(house_own_diff),
    !is.na(house_rent_diff),
    !is.na(elec_diff),
    !is.na(auto_diff),
    !is.na(ng_diff),
    !is.na(food_diff)
  )

# Prepare features and labels
features <- pdata_clean %>%
  select(house_own_diff, house_rent_diff, elec_diff, auto_diff, ng_diff, food_diff)

labels <- pdata_clean$rPCE

# Split into train/test sets (70/30)
set.seed(123)
train_idx <- sample(1:nrow(features), 0.7*nrow(features))

dtrain <- xgb.DMatrix(data = as.matrix(features[train_idx, ]), label = labels[train_idx])
dtest <- xgb.DMatrix(data = as.matrix(features[-train_idx, ]), label = labels[-train_idx])

# define params for regression
params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse"
)

# train XBGoost model
model <- xgboost(
  params = params,
  data = dtrain,
  nrounds = 100,
  verbose = 0
)

# predict and evaluate
preds <- predict(model, dtest)
actuals <- labels[-train_idx]

rmse <- sqrt(mean((preds - actuals)^2))
cat("Test RMSE:", round(rmse, 2), "\n")

# Additional evaluation metrics
mae <- mean(abs(preds - actuals))
sst <- sum((actuals - mean(actuals))^2)
sse <- sum((actuals - preds)^2)
r_squared <- 1 - (sse / sst)
mape <- mean(abs((actuals - preds) / actuals)) * 100

# Print all metrics
cat("Test RMSE:", round(rmse, 2), "\n")
cat("Test MAE:", round(mae, 2), "\n")
cat("Test R-squared:", round(r_squared, 4), "\n")
cat("Test MAPE:", round(mape, 2), "%\n")


########### same tests with log-transformed rPCE
pdata_clean <- pdata_clean %>%
  mutate(log_pce = log(rPCE))

# Prepare features and labels
features <- pdata_clean %>%
  select(house_own_diff, house_rent_diff, elec_diff, auto_diff, ng_diff, food_diff)

labels <- pdata_clean$log_pce

# Split into train/test sets (70/30)
set.seed(123)
train_idx <- sample(1:nrow(features), 0.7*nrow(features))

dtrain <- xgb.DMatrix(data = as.matrix(features[train_idx, ]), label = labels[train_idx])
dtest <- xgb.DMatrix(data = as.matrix(features[-train_idx, ]), label = labels[-train_idx])

# define params for regression
params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse"
)

# train XBGoost model
model <- xgboost(
  params = params,
  data = dtrain,
  nrounds = 100,
  verbose = 0
)

# predict and evaluate
preds <- predict(model, dtest)
actuals <- labels[-train_idx]

rmse <- sqrt(mean((preds - actuals)^2))
cat("Test RMSE:", round(rmse, 2), "\n")

# Additional evaluation metrics
mae <- mean(abs(preds - actuals))
sst <- sum((actuals - mean(actuals))^2)
sse <- sum((actuals - preds)^2)
r_squared <- 1 - (sse / sst)
mape <- mean(abs((actuals - preds) / actuals)) * 100

# Print all metrics
cat("Test RMSE:", round(rmse, 2), "\n")
cat("Test MAE:", round(mae, 2), "\n")
cat("Test R-squared:", round(r_squared, 4), "\n")
cat("Test MAPE:", round(mape, 2), "%\n")