#################### Libraries + formatting
library(plm)
library(tseries)
library(lmtest)

# upload panel data
pdata <- read.csv("Capstone/capstone/panel_data.csv", check.names = FALSE)
pdiff <- read.csv("Capstone/capstone/panel_data_diff.csv", check.names = FALSE)

pdata <- pdata.frame(pdata, index = c("State", "Year"))
pdiff <- pdata.frame(pdiff, index = c("State", "Year"))


################## pre tests

## IPS
purtest(rPCE ~ 1, data = pdata, test = "ips", lags = 1)

## Hadri
purtest(rPCE ~1, data = pdata, test = "hadri", lags = 1)

## Pesaran 
pcdtest(rPCE ~ utilities + food + auto + gas + house_own, data = pdata, test = "cd")

# Fit fixed effects and pooled models
fe_model <- plm(rPCE ~ utilities + food + auto + gas + house_own,
                data = pdata, model = "within")
pooled_model <- plm(rPCE ~ utilities + food + auto + gas + house_own,
                    data = pdata, model = "pooling")

# Hausman test
phtest(fe_model, pooled_model)


pdiff <- pdata.frame(pdiff, index=c("State", "Year"))

## more indepth testing
# IPS test
purtest(utilities_diff ~ 1, data = pdiff, test = "ips", lags = 1)
purtest(auto_diff ~ 1, data = pdiff, test = "ips", lags = 1)
purtest(food_diff ~ 1, data = pdiff, test = "ips", lags = 1)
purtest(gas_diff ~ 1, data = pdiff, test = "ips", lags = 1)
purtest(house_own_diff ~ 1, data = pdiff, test = "ips", lags = 1)


# Hadri test
purtest(utilities_diff ~ 1, data = pdiff, test = "hadri", lags = 1)
purtest(auto_diff ~ 1, data = pdiff, test = "hadri", lags = 1)
purtest(food_diff ~ 1, data = pdiff, test = "hadri", lags = 1)
purtest(gas_diff ~ 1, data = pdiff, test = "hadri", lags = 1)
purtest(house_own_diff ~ 1, data = pdiff, test = "hadri", lags = 1)


## testing level stationarity

# Variables to test
vars <- c("utilities", "food", "auto", "gas", "house_own")

# Run IPS and Hadri tests on levels
for (v in vars) {
  cat("\n======", v, "(LEVEL) ======\n")
  formula <- as.formula(paste(v, "~ 1"))
  
  cat("IPS Test:\n")
  print(purtest(formula, data = pdata, test = "ips", lags = 1))
  
  cat("Hadri Test:\n")
  print(purtest(formula, data = pdata, test = "hadri", lags = 1))
}

## testing cointegration
library(plm)
# Re-create pdata just to be safe
pdata <- pdata.frame(pdata, index = c("State", "Year"))

# Check if the panel is balanced
is.pbalanced(pdata)

# Extract only the complete set of rows used in analysis
cointegration_vars <- c("rPCE", "utilities", "food", "auto", "gas", "house_own")
pdata_subset <- na.omit(pdata[, cointegration_vars])

# Create matrix of variables for po.test — Y first, then Xs
data_mat <- as.matrix(pdata_subset)

# Run Kao cointegration test
kao_result <- po.test(data_mat)

print(kao_result)


####################### Panel ARDL Model
#################### Libraries
library(plm)
library(dplyr)

#################### Step 1: Upload panel data
data <- read.csv("Capstone/capstone/panel_data.csv", check.names = FALSE)

#################### Step 2: Sort data by State and Year
data <- data %>%
  arrange(State, Year)

#################### Step 3: Log transform variables
data <- data %>%
  mutate(
    lrPCE = log(rPCE),
    lutilities = log(utilities),
    lfood = log(food),
    lauto = log(auto),
    lhouse_own = log(house_own),
    lgas = log(gas)
  )

#################### Step 4: Create panel data structure
pdata <- pdata.frame(data, index = c("State", "Year"))

#################### Step 5: Create first differences of **logged** variables
pdata$dlrPCE      <- diff(pdata$lrPCE)
pdata$dlutilities <- diff(pdata$lutilities)
pdata$dlfood      <- diff(pdata$lfood)
pdata$dlauto      <- diff(pdata$lauto)
pdata$dlhouse_own <- diff(pdata$lhouse_own)
pdata$dlgas       <- diff(pdata$lgas)

#################### Step 6: Create lags of differenced variables
pdata <- pdata %>%
  group_by(State) %>%
  mutate(
    dlrPCE_lag1      = dplyr::lag(dlrPCE),
    dlutilities_lag1 = dplyr::lag(dlutilities),
    dlfood_lag1      = dplyr::lag(dlfood),
    dlauto_lag1      = dplyr::lag(dlauto),
    dlhouse_own_lag1 = dplyr::lag(dlhouse_own),
    dlgas_lag1       = dplyr::lag(dlgas)
  ) %>%
  ungroup()

#################### Step 7: Drop NA values caused by differencing and lagging
pdata <- na.omit(pdata)

#################### Step 8: Estimate the panel ARDL model
model <- plm(formula = dlrPCE ~ dlrPCE_lag1 +
               dlutilities + dlutilities_lag1 +
               dlfood + dlfood_lag1 +
               dlauto + dlauto_lag1 +
               dlhouse_own + dlhouse_own_lag1 +
               dlgas + dlgas_lag1,
             data = pdata,
             model = "within")

#################### Step 9: View model summary
summary(model)

fe_estimates <- fixef(model)

# Convert to a data frame
fe_df <- data.frame(
  State = names(fe_estimates),
  FixedEffect = as.numeric(fe_estimates)
)

# Plot
library(ggplot2)

ggplot(fe_df, aes(x = reorder(State, FixedEffect), y = FixedEffect)) +
  geom_col() +
  coord_flip() +
  labs(title = "Fixed Effects by State",
       x = "State",
       y = "Fixed Effect Estimate") +
  theme_minimal()
