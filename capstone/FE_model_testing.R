# libraries
library(plm)
library(tseries)
library(purrr)

# upload panel data
pdata <- read.csv("Capstone/capstone/panel_data.csv", check.names = FALSE)
pdiff <- read.csv("Capstone/capstone/panel_data_diff.csv", check.names = FALSE)

pdata <- pdata.frame(pdata, index = c("State", "Year"))
pdiff <- pdata.frame(pdiff, index = c("State", "Year"))



#### Im-Pesaran-Shin (IPS) test

purtest(electricity ~ 1, data = pdata, test = "ips", lags = 1)
purtest(food ~ 1, data = pdata, test = "ips", lags = 1)
purtest(auto ~ 1, data = pdata, test = "ips", lags = 1)
purtest(house_own ~ 1, data = pdata, test = "ips", lags = 1)
purtest(house_rent ~ 1, data = pdata, test = "ips", lags = 1)



## with first differenced values
purtest(house_own_diff ~ 1, data = pdiff, test = "ips", lags = 1)
purtest(house_rent_diff ~ 1, data = pdiff, test = "ips", lags = 1)
purtest(electricity_diff ~ 1, data = pdiff, test = "ips", lags = 1)
purtest(auto_diff ~ 1, data = pdiff, test = "ips", lags = 1)
purtest(ng_diff ~ 1, data = pdiff, test = "ips", lags = 1)




#### Maddala and Wu, Fisher-type test
purtest(house_rent ~ 1, data = pdata, test = "madwu", lags = 1)
purtest(house_own ~ 1, data = pdata, test = "madwu", lags = 1)
purtest(electricity ~ 1, data = pdata, test = "madwu", lags = 1)
purtest(auto ~ 1, data = pdata, test = "madwu", lags = 1)
purtest(ng ~ 1, data = pdata, test = "madwu", lags = 1)

## with first differenced values
purtest(house_own_diff ~ 1, data = pdiff, test = "madwu", lags = 1)
purtest(house_rent_diff ~ 1, data = pdiff, test = "madwu", lags = 1)
purtest(electricity_diff ~ 1, data = pdiff, test = "madwu", lags = 1)
purtest(auto_diff ~ 1, data = pdiff, test = "madwu", lags = 1)
purtest(ng_diff ~ 1, data = pdiff, test = "madwu", lags = 1)


#### Hausman test
fe_model <- plm(rPCE ~ electricity + auto + food + ng + house_own, data = pdata, model = "within")
re_model <- plm(rPCE ~ electricity + auto + food + ng + house_own, data = pdata, model = "random")

phtest(fe_model, re_model)

library(plm)

bptest(rPCE ~ electricity + auto + food + ng + house_own, # heteroskedasticity test
       data = pdata)

pbgtest(fe_model) # serial correlation test

# Driscoll-Kraay Standard Errors test because both serial correlation and heteroskedasticity detected from other tests
library(lmtest)
library(sandwich)

coeftest(fe_model, vcov = vcovSCC(fe_model, type = "HC1"))




############### Running the Fixed Effects Panel model ########################
library(plm)

# double check formatting
pdata <- pdata.frame(pdata, index = c("State", "Year"))

# model
fe_model <- plm(rPCE ~ electricity + food + auto + ng + house_own, 
                data = pdata, model = "within") # can also use house_rent here
summary(fe_model)

# test prediction accuracy
pdata$fitted_rPCE <- fitted(fe_model)

pdata$residuals <- residuals(fe_model)

rmse <- sqrt(mean(pdata$residuals^2, na.rm = TRUE))
print(rmse)

mae <- mean(abs(pdata$residuals), na.rm = TRUE)
print(mae)

mape <- mean(abs(pdata$residuals / pdata$rPCE), na.rm = TRUE) * 100
print(mape)



