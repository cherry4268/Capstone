# Load the packages
library(xgboost)
library(caret)
library(Matrix)
library(plm)
library(dplyr)
library(tidyr)

pdata <- read.csv("Capstone/capstone/panel_data.csv", check.names = FALSE)
pdata$state <- as.factor(pdata$State)
pdata$year <- as.numeric(as.character(pdata$Year))
pdata <- pdata.frame(pdata, index = c("State", "Year"))

# store results
results <- list()

# list of years in data
years <- sort(unique(pdata$year))

# loop over each year and run regression
for (yr in years) {
  year_data <- pdata %>% filter(year == yr)
  year_data <- year_data %>%
      filter(
        !is.na(rPCE),
        !is.na(house_own),
        !is.na(house_rent),
        !is.na(electricity),
        !is.na(auto),
        !is.na(ng),
        !is.na(food)
      )
      
      # Fit simple linear regression
      model <- lm(rPCE ~ house_own + electricity + auto + ng + food,
                  data = year_data)
      
      # Store the summary
      results[[as.character(yr)]] <- summary(model)
}

# Example: print coefficients for year 2018
results[["2018"]]$coefficients



############ same test using 1st differences
# Create lagged variables manually
pdata <- pdata %>%
  group_by(State) %>%
  mutate(
    rPCE_lag1 = lag(rPCE),
    electricity_lag1 = lag(electricity),
    food_lag1 = lag(food),
    ng_lag1 = lag(ng),
    house_own_lag1 = lag(house_own),
    auto_lag1 = lag(auto)
  ) %>%
  ungroup()

# loop over each year and run regression
for (yr in years) {
  year_data <- pdata %>% filter(year == yr)
  year_data <- year_data %>%
    filter(
      !is.na(rPCE_lag1),
      !is.na(house_own_lag1),
      !is.na(electricity_lag1),
      !is.na(auto_lag1),
      !is.na(ng_lag1),
      !is.na(food_lag1)
    )
  print(paste("Year", yr, "- rows after filtering:", nrow(year_data)))
  
  
  # Skip if no data
  if (nrow(year_data) == 0) {
    warning(paste("No non-NA rows for year", yr, "- skipping"))
    next
  }
  
  # Fit simple linear regression
  model <- lm(rPCE_lag1 ~ house_own_lag1 + electricity_lag1 + auto_lag1 + ng_lag1 + food_lag1,
              data = year_data)
  
  # Store the summary
  results[[as.character(yr)]] <- summary(model)
}

# Example: print coefficients for year 2018
results[["2018"]]$coefficients

##################### Assumption tests

# Linearity
plot(model, which = 1)
car::crPlots(model)

# Durbin-Waston (independence of Errors)
library(lmtest)
dwtest(model)

# homoskedasticity
library(plm)
bptest(model)
plot(model, which = 3)

# Shapiro-Wilk test (normality of errors)
shapiro.test(residuals(model))
  
# Variance Inflation Factor ()
library(car)
vif(model)

# model specification
resettest(model)

##### Robust SE
library(sandwich)
library(lmtest)
coeftest(model, vcov = vcovHC(model, type = "HC1"))

model_log <- lm(log(rPCE) ~ house_own + electricity + auto + ng + food, data = pdata)
plot(model, which = 4)  # Cook's distance

############# Accuracy metrics
predicted <- predict(model)
actual <- pdata$rPCE

# Metrics
rmse <- sqrt(mean((predicted - actual)^2))
mae <- mean(abs(predicted - actual))
r_squared <- summary(model)$r.squared
adj_r_squared <- summary(model)$adj.r.squared
mape <- mean(abs((predicted - actual) / actual)) * 100
smape <- mean(200 * abs(predicted - actual) / (abs(actual) + abs(predicted)))
rmsle <- sqrt(mean((log1p(predicted) - log1p(actual))^2))
median_ae <- median(abs(predicted - actual))
max_error <- max(abs(predicted - actual))

# Print all
cat("RMSE:", rmse, "\n",
    "MAE:", mae, "\n",
    "R-squared:", r_squared, "\n",
    "Adjusted R-squared:", adj_r_squared, "\n",
    "MAPE:", mape, "%\n",
    "SMAPE:", smape, "%\n",
    "RMSLE:", rmsle, "\n",
    "Median AE:", median_ae, "\n",
    "Max Error:", max_error, "\n")
