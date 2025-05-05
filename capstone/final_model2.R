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

# Log transform variables (except house_own)
pdata_raw <- pdata_raw %>%
  mutate(
    lrPCE = log(rPCE),
    lutilities = log(utilities),
    lfood = log(food),
    lauto = log(auto),
    lhouse_rent = log(house_rent),
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
pdata_logdiff$dlhouse_rent    <- diff(pdata_logdiff$lhouse_rent)
pdata_logdiff$dlgas           <- diff(pdata_logdiff$lgas)


# Create lags of differenced variables
pdata_logdiff <- pdata_logdiff %>%
  group_by(State) %>%
  mutate(
    dlrPCE_lag1 = dplyr::lag(dlrPCE),
    dlutilities_lag1 = dplyr::lag(dlutilities),
    dlfood_lag1 = dplyr::lag(dlfood),
    dlauto_lag1 = dplyr::lag(dlauto),
    dlhouse_rent_lag1 = dplyr::lag(dlhouse_rent),
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
house_rent_f   <- forecast_by_state(pdata_raw, "house_rent")
gas_f          <- forecast_by_state(pdata_raw, "gas")

# Combine forecasts
predictor_forecasts <- reduce(list(utilities_f, food_f, auto_f, house_rent_f, gas_f), full_join, by = c("State", "Year"))
setDT(predictor_forecasts)

# ===============================
# Step 3: Add 2022 actuals + lags
# ===============================

# Add 2022 actual values
last_obs <- pdata_raw %>%
  filter(Year == 2022) %>%
  select(State, Year, utilities, food, auto, house_rent, gas) %>%
  rename_with(~ paste0(.x, "_forecast"), -c(State, Year))

predictor_forecasts <- bind_rows(last_obs, predictor_forecasts) %>%
  arrange(State, Year) %>%
  as.data.table()

# Create lags of forecasted predictors
vars <- c("utilities", "food", "auto", "house_rent", "gas")
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
    dlhouse_rent + dlhouse_rent_lag1 +
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
  house_rent_forecast_log = log(house_rent_forecast),
  gas_forecast_log = log(gas_forecast)
)]

# Log transform lagged predictors
predictor_forecasts[, `:=`(
  utilities_lag1_log = ifelse(!is.na(utilities_lag1), log(utilities_lag1), NA),
  food_lag1_log = ifelse(!is.na(food_lag1), log(food_lag1), NA),
  auto_lag1_log = ifelse(!is.na(auto_lag1), log(auto_lag1), NA),
  house_rent_lag1_log = ifelse(!is.na(house_rent_lag1), log(house_rent_lag1), NA),
  gas_lag1_log = ifelse(!is.na(gas_lag1), log(gas_lag1), NA)
)]


fixed_effects <- fixef(pardl_model)
fe_dt <- data.table(State = names(fixed_effects), fe = as.numeric(fixed_effects))
predictor_forecasts <- merge(predictor_forecasts, fe_dt, by = "State", all.x = TRUE)

# Merge real growth
rPCE_lags <- pdata_raw %>%
  filter(Year %in% c(2021, 2022)) %>%
  select(State, Year, rPCE) %>%
  pivot_wider(names_from = Year, values_from = rPCE, names_prefix = "rPCE_") %>%
  mutate(rPCE_2021_2022_growth = log(rPCE_2022) - log(rPCE_2021))

predictor_forecasts <- merge(predictor_forecasts, rPCE_lags[, c("State", "rPCE_2021_2022_growth")], by = "State", all.x = TRUE)

# Merge 2022 log_rPCE
rPCE_2022 <- pdata_raw %>%
  filter(Year == 2022) %>%
  select(State, rPCE) %>%
  mutate(log_rPCE = log(rPCE)) %>%
  select(State, log_rPCE)

predictor_forecasts <- merge(predictor_forecasts, rPCE_2022, by = "State", all.x = TRUE)

# Set rPCE_dollars for 2022
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
    if (curr_yr == 2023) {
      dlrPCE <- state_data$rPCE_2021_2022_growth[i]
    } else {
      dlrPCE <- 0
    }
    
    # Current differences for predictors
    dlutilities <- state_data$utilities_forecast_log[i] - state_data$utilities_lag1_log[i]
    dlfood <- state_data$food_forecast_log[i] - state_data$food_lag1_log[i]
    dlauto <- state_data$auto_forecast_log[i] - state_data$auto_lag1_log[i]
    dlhouse <- state_data$house_rent_forecast_log[i] - state_data$house_rent_lag1_log[i]
    dlgas <- state_data$gas_forecast_log[i] - state_data$gas_lag1_log[i]
    
    # Lagged differences for predictors 
    if (i > 2) {
      dlutilities_lag <- state_data$utilities_lag1_log[i] - state_data$utilities_lag1_log[i-1]
      dlfood_lag <- state_data$food_lag1_log[i] - state_data$food_lag1_log[i-1]
      dlauto_lag <- state_data$auto_lag1_log[i] - state_data$auto_lag1_log[i-1]
      dlhouse_lag <- state_data$house_rent_lag1_log[i] - state_data$house_rent_lag1_log[i-1]
      dlgas_lag <- state_data$gas_lag1_log[i] - state_data$gas_lag1_log[i-1]
    } else {
      dlutilities_lag <- 0
      dlfood_lag <- 0
      dlauto_lag <- 0
      dlhouse_lag <- 0
      dlgas_lag <- 0
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
      b$dlhouse_rent * dlhouse +
      b$dlhouse_rent_lag1 * dlhouse_lag +
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
  select(rPCE_lag1, utilities, food, auto, house_rent, gas, Year) %>%
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
  select(State, rPCE_lag1, utilities_forecast, food_forecast, auto_forecast, house_rent_forecast, gas_forecast, Year) %>%
  rename(
    utilities = utilities_forecast,
    food = food_forecast,
    auto = auto_forecast,
    house_rent = house_rent_forecast,
    gas = gas_forecast
  )

# 2. Convert only numeric columns 
future_data_numeric <- future_data %>%
  select(rPCE_lag1, utilities, food, auto, house_rent, gas, Year) %>%
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

# ===========================
# Step 7: Save dataframe
# ===========================
saveRDS(predictor_forecasts, file = "predictor_forecasts2.rds")
