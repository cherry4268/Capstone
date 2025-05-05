# libraries
library(dplyr)
library(tidyr)
library(readr)

gas <- read.csv("Capstone/data/Transport/gas.csv")

state_region_mapping <- tribble(
  ~State, ~Region,
  
  # Northeast
  "Maine", "Northeast",
  "New Hampshire", "Northeast",
  "Vermont", "Northeast",
  "Massachusetts", "Northeast",
  "Rhode Island", "Northeast",
  "Connecticut", "Northeast",
  "New York", "Northeast",
  "New Jersey", "Northeast",
  "Pennsylvania", "Northeast",
  
  # Midwest
  "Ohio", "Midwest",
  "Michigan", "Midwest",
  "Indiana", "Midwest",
  "Illinois", "Midwest",
  "Wisconsin", "Midwest",
  "Minnesota", "Midwest",
  "Iowa", "Midwest",
  "Missouri", "Midwest",
  "North Dakota", "Midwest",
  "South Dakota", "Midwest",
  "Nebraska", "Midwest",
  "Kansas", "Midwest"
)

gas <- gas %>%
  left_join(state_region_mapping, by = "Region") %>%
  select(Year, State, Region, Price)

head(gas)