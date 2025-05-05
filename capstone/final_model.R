# ======================
# Step 0: Load libraries
# ======================
library(dplyr)
library(tidyr)
library(forecast)
library(purrr)
library(data.table)
library(plm)
library(xgboost)
library(Matrix)
library(ggplot2)

# ================================
# Step 1: Upload and prepare data
# ================================
pdata_raw <- read.csv("Capstone/capstone/panel_data.csv", check.names = FALSE)

# Sort by State and Year
pdata_raw <- pdata_raw %>%
  arrange(State, Year)

# Log transform variables 
pdata_raw <- pdata_raw %>%
  mutate(
    lrPCE = log(rPCE),
    lutilities = log(utilities),
    lfood = log(food),
    lauto = log(auto),
    lhouse_own = log(house_own),
    lgas = log(gas),
    Year_center = Year -2015
  )

# Convert to pdata.frame (important: this makes diff() work by panel!)
pdata_logdiff <- pdata.frame(pdata_raw, index = c("State", "Year"))

# Create first differences (after pdata.frame!)
pdata_logdiff$dlrPCE        <- diff(pdata_logdiff$lrPCE)
pdata_logdiff$dlutilities <- diff(pdata_logdiff$lutilities)
pdata_logdiff$dlfood        <- diff(pdata_logdiff$lfood)
pdata_logdiff$dlauto         <- diff(pdata_logdiff$lauto)
pdata_logdiff$dhouse_own    <- diff(pdata_logdiff$house_own)
pdata_logdiff$dlgas         <- diff(pdata_logdiff$lgas)

# Create lags of differenced variables
pdata_logdiff <- pdata_logdiff %>%
  group_by(State) %>%
  mutate(
    dlrPCE_lag1 = dplyr::lag(dlrPCE),
    dlutilities_lag1 = dplyr::lag(dlutilities),
    dlfood_lag1 = dplyr::lag(dlfood),
    dlauto_lag1 = dplyr::lag(dlauto),
    dhouse_own_lag1 = dplyr::lag(dhouse_own),
    dlgas_lag1 = dplyr::lag(dlgas)
  ) %>%
  ungroup()

# Drop NA rows (from differencing + lagging)
pdata_logdiff <- na.omit(pdata_logdiff)

# ===============================
# Step 2: Forecast predictors
# ===============================

forecast_by_state <- function(data, var) { 
  data %>%
    mutate(Year = as.numeric(as.character(Year))) %>%
    select(State, Year, !!sym(var)) %>%
    group_by(State) %>%
    arrange(Year) %>%
    nest() %>%
    mutate(
      model = map(data, ~ auto.arima(ts(.x[[var]], start = min(.x$Year)))),
      forecast = map(model, ~ forecast(.x, h = 5)),
      pred_years = map(data, ~ seq(max(.x$Year) + 1, length.out = 5))
    ) %>%
    mutate(
      forecast_df = map2(pred_years, forecast, function(years, f) {
        tibble(
          Year = years,
          !!paste0(var, "_forecast") := as.numeric(f$mean)
        )
      })
    ) %>%
    unnest(forecast_df) %>%
    select(State, Year, matches("_forecast$"))
}

# Forecast original raw (not logged) predictors
utilities_f <- forecast_by_state(pdata_raw, "utilities")
food_f        <- forecast_by_state(pdata_raw, "food")
auto_f        <- forecast_by_state(pdata_raw, "auto")
house_own_f   <- forecast_by_state(pdata_raw, "house_own")
gas_f         <- forecast_by_state(pdata_raw, "gas")

# Combine forecasts
predictor_forecasts <- reduce(list(utilities_f, food_f, auto_f, house_own_f, gas_f), full_join, by = c("State", "Year"))
setDT(predictor_forecasts)

# ===============================
# Step 3: Add 2022 actuals + lags
# ===============================

# Add 2022 actual values
last_obs <- pdata_raw %>%
  filter(Year == 2022) %>%
  select(State, Year, utilities, food, auto, house_own, gas) %>%
  rename_with(~ paste0(.x, "_forecast"), -c(State, Year))

predictor_forecasts <- bind_rows(last_obs, predictor_forecasts) %>%
  arrange(State, Year) %>%
  as.data.table()

# Create lags of forecasted predictors
vars <- c("utilities", "food", "auto", "house_own", "gas")
for (v in vars) {
  predictor_forecasts[, paste0(v, "_lag1") := shift(get(paste0(v, "_forecast")), 1), by = State]
}

# ===========================
# Step 4: Estimate panel ARDL
# ===========================

# Now run the panel ARDL model
pardl_model <- plm(
  formula = dlrPCE ~ dlrPCE_lag1 +
    dlutilities + dlutilities_lag1 +
    dlfood + dlfood_lag1 +
    dlauto + dlauto_lag1 +
    dhouse_own + dhouse_own_lag1 +
    dlgas + dlgas_lag1 +
    Year_center,
  data = pdata_logdiff,
  model = "within",
  effect = "individual"
)

summary(pardl_model)

# Extract coefficients
coefs <- coef(pardl_model)
b <- as.list(coefs)

# ===========================
# Step 5: Recursive forecasting (completely revised)
# ===========================

# Log transform forecasted predictors
predictor_forecasts[, `:=`(
  utilities_forecast_log = log(utilities_forecast),
  food_forecast_log = log(food_forecast),
  auto_forecast_log = log(auto_forecast),
  house_own_forecast_log = house_own_forecast,
  gas_forecast_log = log(gas_forecast)
)]

# Log transform lagged predictors
predictor_forecasts[, `:=`(
  utilities_lag1_log = ifelse(!is.na(utilities_lag1), log(utilities_lag1), NA),
  food_lag1_log = ifelse(!is.na(food_lag1), log(food_lag1), NA),
  auto_lag1_log = ifelse(!is.na(auto_lag1), log(auto_lag1), NA),
  house_own_lag1_log = house_own_lag1,
  gas_lag1_log = ifelse(!is.na(gas_lag1), log(gas_lag1), NA)
)]

# Get fixed effects from the model
fixed_effects <- fixef(pardl_model)
fe_dt <- data.table(State = names(fixed_effects), fe = as.numeric(fixed_effects))
predictor_forecasts <- merge(predictor_forecasts, fe_dt, by = "State", all.x = TRUE)

# Get initial rPCE values for 2022 (starting point)
rPCE_2022 <- pdata_raw %>%
  filter(Year == 2022) %>%
  select(State, rPCE) %>%
  mutate(log_rPCE = log(rPCE)) %>%
  select(State, log_rPCE)

# Add initial values to forecasts
predictor_forecasts <- merge(predictor_forecasts, rPCE_2022, by = "State", all.x = TRUE)
predictor_forecasts[Year == 2022, rPCE_dollars := exp(log_rPCE)]

# Extract coefficients from the model
coefs <- coef(pardl_model)
b <- as.list(coefs)

# Forecast for each state separately
states <- unique(predictor_forecasts$State)

for (state in states) {
  state_data <- predictor_forecasts[State == state]
  state_data <- state_data[order(Year)]
  
  # For each future year
  for (i in 2:nrow(state_data)) {
    curr_yr <- state_data$Year[i]
    if (curr_yr <= 2022) next  # Skip if not a forecast year
    
    # Get current and previous values
    prev_log_rPCE <- state_data$log_rPCE[i-1]
    
    # Compute differences for all variables
    dlrPCE <- 0  # Initialize (will be updated if we have enough history)
    if (i > 2) {
      prev_prev_log_rPCE <- state_data$log_rPCE[i-2]
      dlrPCE <- prev_log_rPCE - prev_prev_log_rPCE
    }
    
    # Current differences for predictors
    dlutilities <- state_data$utilities_forecast_log[i] - state_data$utilities_lag1_log[i]
    dlfood <- state_data$food_forecast_log[i] - state_data$food_lag1_log[i]
    dlauto <- state_data$auto_forecast_log[i] - state_data$auto_lag1_log[i]
    dlhouse <- state_data$house_own_forecast_log[i] - state_data$house_own_lag1_log[i]
    dlgas <- state_data$gas_forecast_log[i] - state_data$gas_lag1_log[i]
    
    # Lagged differences for predictors 
    dlutilities_lag <- 0
    dlfood_lag <- 0
    dlauto_lag <- 0
    dlhouse_lag <- 0
    dlgas_lag <- 0
    
    if (i > 2) {
      dlutilities_lag <- state_data$utilities_lag1_log[i] - state_data$utilities_lag1_log[i-1]
      dlfood_lag <- state_data$food_lag1_log[i] - state_data$food_lag1_log[i-1]
      dlauto_lag <- state_data$auto_lag1_log[i] - state_data$auto_lag1_log[i-1]
      dlhouse_lag <- state_data$house_own_lag1_log[i] - state_data$house_own_lag1_log[i-1]
      dlgas_lag <- state_data$gas_lag1_log[i] - state_data$gas_lag1_log[i-1]
    }
    
    # Compute next value using ARDL model
    growth <- state_data$fe[i] +
      b$dlrPCE_lag1 * dlrPCE +
      b$dlutilities * dlutilities +
      b$dlutilities_lag1 * dlutilities_lag +
      b$dlfood * dlfood +
      b$dlfood_lag1 * dlfood_lag +
      b$dlauto * dlauto +
      b$dlauto_lag1 * dlauto_lag +
      b$dhouse_own * dlhouse +
      b$dhouse_own_lag1 * dlhouse_lag +
      b$dlgas * dlgas +
      b$dlgas_lag1 * dlgas_lag +
      b$Year_center * (state_data$Year[i] - 2015)
    
    # Update state data
    new_log_rPCE <- prev_log_rPCE + growth
    state_data$log_rPCE[i] <- new_log_rPCE
    state_data$rPCE_dollars[i] <- exp(new_log_rPCE)
  }
  
  # Update main data table
  predictor_forecasts[State == state, `:=`(
    log_rPCE = state_data$log_rPCE,
    rPCE_dollars = state_data$rPCE_dollars
  )]
}

# ===========================
# Step 6: XGBoost forecasting
# ===========================

# Create training dataset from pdata_raw
pdata_for_xgb <- pdata_raw %>%
  arrange(State, Year) %>%
  group_by(State) %>%
  mutate(rPCE_lag1 = lag(rPCE)) %>%
  ungroup() %>%
  filter(!is.na(rPCE_lag1))

# Create training matrix (level units)
x_train <- pdata_for_xgb %>%
  select(rPCE_lag1, utilities, food, auto, house_own, gas, Year) %>%
  mutate(across(everything(), as.numeric)) %>%
  bind_cols(as.data.frame(model.matrix(~ State - 1, data = pdata_for_xgb))) %>%
  as.matrix()

# Target = rPCE (in dollars)
y_train <- pdata_for_xgb$rPCE

# Train XGBoost model
dtrain <- xgb.DMatrix(data = x_train, label = y_train)

xgb_model <- xgboost(
  data = dtrain,
  nrounds = 200,         # maybe 200+ rounds now
  objective = "reg:squarederror",
  eval_metric = "rmse",
  max_depth = 5,
  eta = 0.01,
  subsample = 0.8,
  colsample_bytree = 0.8,
  verbose = 0
)

# Prepare future predictors

# 1. Create lagged rPCE (already forecasted by ARDL step)
predictor_forecasts <- predictor_forecasts %>%
  arrange(State, Year) %>%
  group_by(State) %>%
  mutate(rPCE_lag1 = lag(rPCE_dollars)) %>%
  ungroup()


future_data <- predictor_forecasts %>%
  filter(!is.na(rPCE_dollars)) %>%
  select(State, rPCE_lag1, utilities_forecast, food_forecast, auto_forecast, house_own_forecast, gas_forecast, Year) %>%
  rename(
    utilities = utilities_forecast,
    food = food_forecast,
    auto = auto_forecast,
    house_own = house_own_forecast,
    gas = gas_forecast
  )

# 2. Convert only numeric columns (leave State alone!)
future_data_numeric <- future_data %>%
  select(rPCE_lag1, utilities, food, auto, house_own, gas, Year) %>%
  mutate(across(everything(), as.numeric))

# 3. Add dummy variables for State
future_dummies <- as.data.frame(model.matrix(~ State - 1, data = future_data))

# 4. Bind predictors + dummies
x_future_matrix <- bind_cols(future_data_numeric, future_dummies) %>%
  as.matrix()

# Predict using XGBoost
dfuture <- xgb.DMatrix(data = x_future_matrix)

rPCE_xgb_predictions <- predict(xgb_model, newdata = dfuture)

# Write predictions back
forecast_indices <- which(!is.na(predictor_forecasts$rPCE_dollars))

predictor_forecasts$rPCE_xgb <- NA_real_
predictor_forecasts$rPCE_xgb[forecast_indices] <- rPCE_xgb_predictions

predictor_forecasts <- predictor_forecasts %>%
  mutate(rPCE_final = ifelse(!is.na(rPCE_dollars) & !is.na(rPCE_xgb),
                             0.5 * rPCE_dollars + 0.5 * rPCE_xgb,
                             NA_real_))



# Load xgboost library
library(xgboost)
# Predict on training data
train_predictions <- predict(xgb_model, newdata = dtrain)

# Create a dataframe
train_results <- data.frame(
  Actual = y_train,
  Predicted = train_predictions
)

# Plot
library(ggplot2)

print(ggplot(train_results, aes(x = Predicted, y = Actual)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "XGBoost Training Predictions vs Actual",
       x = "Predicted rPCE",
       y = "Actual rPCE") +
  theme_minimal())




train_predictions <- predict(xgb_model, newdata = dtrain)

# Actual values
y_actual <- y_train

# Calculate Metrics
rmse <- sqrt(mean((y_actual - train_predictions)^2))
mae <- mean(abs(y_actual - train_predictions))
mape <- mean(abs((y_actual - train_predictions) / y_actual)) * 100
median_ae <- median(abs(y_actual - train_predictions))

# Output
cat("RMSE:", round(rmse, 2), "\n")
cat("MAE:", round(mae, 2), "\n")
cat("MAPE (%):", round(mape, 2), "\n")
cat("Median Absolute Error:", round(median_ae, 2), "\n")

saveRDS(predictor_forecasts, file = "predictor_forecasts.rds")
write.csv(predictor_forecasts, "predictor_forecasts.csv", row.names=FALSE)



######### more visuals

ggplot(predictor_forecasts %>% filter(Year >= 2023), 
       aes(x = Year, y = utilities_forecast, group = State)) +
  geom_line(alpha = 0.4) +
  labs(title = "Forecasted Utilities (2023–2027)", x = "Year", y = "Utilities ($)") +
  theme_minimal()

ggplot(train_results, aes(x = Predicted, y = Actual)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "XGBoost Training Predictions vs Actual",
       x = "Predicted rPCE",
       y = "Actual rPCE") +
  theme_minimal()

ggplot(predictor_forecasts %>% filter(Year %in% c(2023, 2027)), 
       aes(x = reorder(State, rPCE_final), y = rPCE_final, fill = as.factor(Year))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Forecasted rPCE by State (2023 vs 2027)", x = "State", y = "rPCE ($)", fill = "Year") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, size = 10),
    axis.title.x = element_text(margin = margin(t = 10)),
    panel.grid.major.x = element_blank()
  ) +
  scale_x_discrete(expand = expansion(add = 0.5)) 


## testing the pardl model accuracy

pdata_logdiff$Year <- as.numeric(as.character(pdata_logdiff$Year))
pdata_train <- pdata_logdiff %>% filter(Year <= 2021)


pardl_model_2021 <- plm(
  formula = dlrPCE ~ dlrPCE_lag1 +
    dlutilities + dlutilities_lag1 +
    dlfood + dlfood_lag1 +
    dlauto + dlauto_lag1 +
    dhouse_own + dhouse_own_lag1 +
    dlgas + dlgas_lag1 +
    Year_center,
  data = pdata_train,
  model = "within",
  effect = "individual"
)

# Get fixed effects and coefficients
b <- as.list(coef(pardl_model_2021))
fixed_effects <- fixef(pardl_model_2021)
fe_dt <- data.frame(State = names(fixed_effects), fe = as.numeric(fixed_effects))

# Get last available input values (from 2021) for each state
pdata_2021 <- pdata_logdiff %>%
  filter(Year == 2021) %>%
  select(State, dlrPCE, dlrPCE_lag1,
         dlutilities, dlutilities_lag1,
         dlfood, dlfood_lag1,
         dlauto, dlauto_lag1,
         dhouse_own, dhouse_own_lag1,
         dlgas, dlgas_lag1,
         Year_center)

# Add fixed effects
pdata_2021 <- pdata_2021 %>%
  left_join(fe_dt, by = "State")

# Compute predicted delta log(rPCE)
pdata_2021 <- pdata_2021 %>%
  mutate(dlrPCE_pred = fe +
           b$dlrPCE_lag1     * dlrPCE_lag1 +
           b$dlutilities     * dlutilities +
           b$dlutilities_lag1 * dlutilities_lag1 +
           b$dlfood          * dlfood +
           b$dlfood_lag1     * dlfood_lag1 +
           b$dlauto          * dlauto +
           b$dlauto_lag1     * dlauto_lag1 +
           b$dhouse_own      * dhouse_own +
           b$dhouse_own_lag1 * dhouse_own_lag1 +
           b$dlgas           * dlgas +
           b$dlgas_lag1      * dlgas_lag1 +
           b$Year_center     * (2022 - 2015))  # hard-coded for Year_center in 2022

# Get log(rPCE) for 2021 from pdata_raw
rPCE_2021 <- pdata_raw %>%
  filter(Year == 2021) %>%
  select(State, lrPCE_2021 = lrPCE)

pdata_2021 <- pdata_2021 %>%
  left_join(rPCE_2021, by = "State") %>%
  mutate(lrPCE_2022_pred = lrPCE_2021 + dlrPCE_pred,
         rPCE_2022_pred = exp(lrPCE_2022_pred))

# Get actual rPCE for 2022
rPCE_2022_actual <- pdata_raw %>%
  filter(Year == 2022) %>%
  select(State, rPCE_2022_actual = rPCE)

# Compare predicted vs. actual
rPCE_compare <- pdata_2021 %>%
  select(State, rPCE_2022_pred) %>%
  left_join(rPCE_2022_actual, by = "State") %>%
  mutate(
    error = rPCE_2022_pred - rPCE_2022_actual,
    abs_error = abs(error),
    pct_error = abs(error) / rPCE_2022_actual * 100
  )

# Forecast accuracy metrics
ardl_2022_eval <- rPCE_compare %>%
  summarise(
    RMSE = sqrt(mean(error^2, na.rm = TRUE)),
    MAE  = mean(abs_error, na.rm = TRUE),
    MAPE = mean(pct_error, na.rm = TRUE)
  )

print(ardl_2022_eval)



library(tidyverse)
library(forecast)
library(scales)
avg_national <- pdata_raw %>%
  group_by(Year) %>%
  summarise(
    rPCE = mean(rPCE, na.rm = TRUE),
    utilities = mean(utilities, na.rm = TRUE),
    food = mean(food, na.rm = TRUE),
    auto = mean(auto, na.rm = TRUE),
    gas = mean(gas, na.rm = TRUE),
    house_own = mean(house_own, na.rm = TRUE),
    house_rent = mean(house_rent, na.rm = TRUE)
  ) %>%
  pivot_longer(-Year, names_to = "Variable", values_to = "Value")

# Step 2: Forecast each variable's national average using ARIMA
forecast_vars <- avg_national %>%
  pivot_wider(names_from = Variable, values_from = Value)

# Create a forecast dataframe
forecast_df <- tibble()

for (var in names(forecast_vars)[-1]) {
  ts_data <- ts(forecast_vars[[var]], start = min(forecast_vars$Year))
  model <- auto.arima(ts_data)
  f <- forecast(model, h = 5)
  
  temp <- tibble(
    Year = 2023:2027,
    Variable = var,
    Value = as.numeric(f$mean)
  )
  forecast_df <- bind_rows(forecast_df, temp)
}

# Combine historical and forecasted data
avg_combined <- bind_rows(avg_national, forecast_df)

# Step 3: Plot
ggplot(avg_combined, aes(x = Year, y = Value, color = Variable)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  labs(
    title = "National Average of Cost-of-Living Components (2015–2027)",
    y = "Average Price ($)",
    x = "Year"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = dollar_format()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))