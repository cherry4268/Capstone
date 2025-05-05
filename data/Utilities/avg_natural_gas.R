# Required libraries
library(tidyverse)
library(lubridate)
library(forecast)
library(tseries)
library(ggplot2)
library(scales)

ng <- read.csv("Capstone/data/Utilities/natural_gas.csv")

head(ng)

# Group data by State and Year
house_year <- house %>%
  mutate(year = year(date)) %>%
  group_by(State, year) %>%
  summarize(avg_price = mean(U.S..Price.of.Natural.Gas.Delivered.to.Residential.Consumers..Dollars.per.Thousand.Cubic.Feet., na.rm = TRUE))

