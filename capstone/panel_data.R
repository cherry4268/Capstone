# load libraries
library(tseries)
library(urca)
library(plm)
library(dplyr)
library(tidyverse)

# upload individual datasets
utilities <- read.csv("Capstone/data/Utilities/utilities.csv", check.names = FALSE)
auto <- read.csv("Capstone/data/Transport/auto.csv", check.names = FALSE)
house_own <- read.csv("Capstone/data/Housing/house_own.csv", check.names = FALSE)
house_rent <- read.csv("Capstone/data/Housing/house_rent.csv", check.names = FALSE)
food <- read.csv("Capstone/data/Grocery/state_food.csv", check.names = FALSE)
rPCE <- read.csv("Capstone/data/rPCE.csv")
gas <- read.csv("Capstone/data/Transport/gas.csv", check.names = FALSE)




################ Merging the Datasets ############################
utilities$Year <- as.numeric(utilities$Year)
auto$Year <- as.numeric(auto$Year)
house_own$Year <- as.numeric(house_own$Year)
house_rent$Year <- as.numeric(house_rent$Year)
food$Year <- as.numeric(food$Year)
rPCE$Year <- as.numeric(rPCE$Year)
gas$Year <- as.numeric(gas$Year)

# standardize each dataset
utilities <- utilities %>%
  mutate(Category = "utilities") %>%
  select(State, Year, Category, Price)

utilities <- utilities %>%
  mutate(
    Price = gsub(",", "", Price),           # Remove commas
    Price = na_if(Price, ""),                # Turn empty strings into real NA
    Price = na_if(Price, "-"),               # Turn dashes into real NA
    Price = as.numeric(Price),               # Now safely convert to numeric
    Year = as.numeric(Year)                  # Also make Year numeric
  )

auto <- auto %>%
  mutate(Category = "auto") %>%
  select(State, Year, Category, Price)

gas <- gas %>%
  mutate(Category = "gas") %>%
  select(State, Year, Category, Price)

food <- food %>%
  mutate(Category = "food") %>%
  select(State, Year, Category, Price)

house_own <- house_own %>%
  mutate(Category = "house_own") %>%
  select(State, Year, Category, Price)

house_rent <- house_rent %>%
  mutate(Category = "house_rent") %>%
  select(State, Year, Category, Price)

rPCE <- rPCE %>%
  mutate(Category = "rPCE") %>%
  select(State, Year, Category, Price)


# Merge all datasets
df <- bind_rows(utilities, auto, food, house_own, house_rent, rPCE, gas)
df %>% filter(if_any(everything(), is.na))


# clean up the datasets
df <- df %>%
  mutate(
    State = str_trim(State),             # Remove leading/trailing spaces
    State = str_replace_all(State, "\\.", " ")  # Replace periods with spaces
  )
df <- df %>%
  mutate(
    State = case_when(
      State == "Conneticut"     ~ "Connecticut",
      TRUE ~ State
    )
  )

df %>%
  group_by(Category) %>%
  summarise(States = list(unique(State)))

################################################################# Creating Panel data ##########################################################
# Create panel data df
panel_data <- df %>%
  pivot_wider(
    id_cols = c(State, Year),
    names_from = Category,
    values_from = Price
  )

print(panel_data %>% filter(if_any(everything(), is.na)))


panel_data <- panel_data %>%
  mutate(across(
    c(utilities, auto, house_own, house_rent, food, rPCE, gas),
    ~ as.numeric(.)
  ))



# Create Differenced variables
panel_data_diff <- panel_data %>%
  arrange(State, Year) %>%
  group_by(State) %>%
  mutate(
    utilities_diff = utilities - dplyr::lag(utilities),
    auto_diff = auto - dplyr::lag(auto),
    house_own_diff = house_own - dplyr::lag(house_own),
    house_rent_diff = house_rent - dplyr::lag(house_rent),
    food_diff = food - dplyr::lag(food),
    rPCE_diff = rPCE - dplyr::lag(rPCE),
    gas_diff = gas - dplyr::lag(gas)
  ) %>%
  ungroup()



panel_data %>% filter(if_any(everything(), is.na))


# since each observation will be NA when differenced, I'm removing them
panel_data_diff <- panel_data_diff %>%
  filter(!is.na(utilities_diff))

######## print csvs
write.csv(panel_data, "panel_data.csv", row.names=FALSE)
write.csv(panel_data_diff, "panel_data_diff.csv", row.names=FALSE)


####### Visualizations
# Number of unique states and years
panel_data %>%
  summarise(
    States = n_distinct(State),
    Years = n_distinct(Year),
    Observations = n()
  )

# variable min, max, mean, quartiles 
library(knitr)
panel_data %>%
  select(utilities, auto, food, house_own, house_rent, gas, rPCE) %>%
  summary() %>%
  capture.output() %>%
  cat(sep = "\n")

library(psych)
describe(panel_data %>% select(utilities, auto, food, house_own, gas, house_rent, rPCE))


# National average by year
panel_data %>%
  group_by(Year) %>%
  summarise(across(c(rPCE, utilities, auto, food, house_own, house_rent, gas), mean, na.rm = TRUE)) %>%
  pivot_longer(cols = -Year, names_to = "Variable", values_to = "Average") %>%
  ggplot(aes(x = Year, y = Average, color = Variable)) +
  geom_line(size = 1.2) +
  labs(title = "National Average of Cost-of-Living Components (2015–2022)",
       y = "Average Price ($)", x = "Year") +
  theme_minimal()


# Count of missing values per variable
panel_data %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Missing") %>%
  arrange(desc(Missing)) %>%
  knitr::kable()

# Distribution of rPCE by State
library(ggplot2)
panel_data %>%
  ggplot(aes(x = reorder(State, rPCE, FUN = median), y = rPCE)) +
  geom_boxplot() +
  coord_flip() +
  labs(title = "Distribution of rPCE by State (2015–2022)",
       x = "State", y = "rPCE ($)") +
  theme_minimal()
