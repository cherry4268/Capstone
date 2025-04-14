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
pcdtest(rPCE ~ electricity + food + auto + ng + house_own, data = pdata, test = "cd")

# Fit fixed effects and pooled models
fe_model <- plm(rPCE ~ electricity + food + auto + ng + house_own,
                data = pdata, model = "within")
pooled_model <- plm(rPCE ~ electricity + food + auto + ng + house_own,
                    data = pdata, model = "pooling")

# Hausman test
phtest(fe_model, pooled_model)







####################### Panel ARDL Model

library(plm)
library(dplyr)

colnames(pdata)

# Create lagged variables manually
pdata <- pdata %>%
  group_by(State) %>%
  mutate(
    rPCE_lag1 = lag(rPCE),
    electricity_lag1 = lag(electricity),
    food_lag1 = lag(food),
    ng_lag1 = lag(ng),
    house_own_lag1 = lag(house_own)
  ) %>%
  ungroup()

# Run the panel ARDL model using plm
model <- plm(rPCE ~ rPCE_lag1 + 
               electricity + electricity_lag1 + 
               food + food_lag1 + 
               ng + ng_lag1 + 
               house_own + house_own_lag1,
             data = pdata, 
             model = "within")

# View the results
summary(model)
