# load libraries
library(tseries)
library(urca)
library(plm)
library(dplyr)
library(tidyverse)

# upload individual datasets
ng <- read.csv("Capstone/data/Utilities/ng.csv", check.names = FALSE)
electricity <- read.csv("Capstone/data/Utilities/electricity.csv", check.names = FALSE)
auto <- read.csv("Capstone/data/Transport/auto.csv", check.names = FALSE)
house_own <- read.csv("Capstone/data/Housing/house_own.csv", check.names = FALSE)
house_rent <- read.csv("Capstone/data/Housing/house_rent.csv", check.names = FALSE)
food <- read.csv("Capstone/data/Grocery/state_food.csv", check.names = FALSE)
rPCE <- read.csv("Capstone/data/rPCE.csv")




################ Merging the Datasets ############################
ng$Year <- as.numeric(ng$Year)
electricity$Year <- as.numeric(electricity$Year)
auto$Year <- as.numeric(auto$Year)
house_own$Year <- as.numeric(house_own$Year)
house_rent$Year <- as.numeric(house_rent$Year)
food$Year <- as.numeric(food$Year)
rPCE$Year <- as.numeric(rPCE$Year)

# standardize each dataset
electricity <- electricity %>%
  mutate(Category = "electricity") %>%
  select(State, Year, Category, Price)

auto <- auto %>%
  mutate(Category = "auto") %>%
  select(State, Year, Category, Price)

ng <- ng %>%
  mutate(Category = "ng") %>%
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
df <- bind_rows(electricity, auto, ng, food, house_own, house_rent, rPCE)
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
    c(electricity, auto, ng, house_own, house_rent, food, rPCE),
    ~ as.numeric(unlist(.))
  ))



# Create Differenced variables
panel_data_diff <- panel_data %>%
  arrange(State, Year) %>%
  group_by(State) %>%
  mutate(
    electricity_diff = electricity - lag(electricity),
    auto_diff = auto - lag(auto),
    ng_diff = ng - lag(ng),
    house_own_diff = house_own - lag(house_own),
    house_rent_diff = house_rent - lag(house_rent),
    food_diff = food - lag(food)
  ) %>%
  ungroup()


panel_data %>% filter(if_any(everything(), is.na))


# since each observation will be NA when differenced, I'm removing them
panel_data_diff <- panel_data_diff %>%
  filter(!is.na(electricity_diff))

######## print csvs
write.csv(panel_data, "panel_data.csv", row.names=FALSE)
write.csv(panel_data_diff, "panel_data_diff.csv", row.names=FALSE)




