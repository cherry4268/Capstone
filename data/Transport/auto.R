# libraries
library(dplyr)
library(tidyr)
library(tseries)
library(urca)
library(plm)
library(dplyr)
library(tidyverse)

auto <- read.csv("Capstone/data/Transport/auto.csv")

# Reshape auto insurance
auto <- auto %>%
  mutate(across(-State, ~str_replace_all(., ",", ""))) 

colnames(auto) <- gsub("^X", "", colnames(auto))


auto <- auto %>%
  pivot_longer(
    cols = -State, # Pivot all columns except State
    names_to = "Year",
    values_to = "Price"
  ) 

# define regions 
northeast <- c("Maine", "New Hampshire", "Vermont", "Massachusetts", "Rhode Island", 
               "Connecticut", "New York", "New Jersey", "Pennsylvania")
midwest <- c("Ohio", "Indiana", "Michigan", "Illinois", "Wisconsin", 
             "Minnesota", "Iowa", "Missouri", "North Dakota", "South Dakota", 
             "Nebraska", "Kansas")
south <- c("Delaware", "Maryland", "Virginia", "West Virginia", "North Carolina",
           "South Carolina", "Georgia", "Florida", "Kentucky", "Tennessee", 
           "Alabama", "Mississippi", "Arkansas", "Louisiana", "Oklahoma", "Texas")
west <- c("Montana", "Idaho", "Wyoming", "Colorado", "New Mexico", "Arizona", 
          "Utah", "Nevada", "Washington", "Oregon", "California", "Alaska", "Hawaii")





# differentiate regional data
auto <- auto %>%
  mutate(Region = case_when(
    State %in% northeast ~ "Northeast",
    State %in% midwest ~ "Midwest",
    State %in% south ~ "South",
    State %in% west ~ "West",
    TRUE ~ "Other"
  ))

auto$Year <- as.integer(auto$Year)
auto$Price <- as.numeric(auto$Price)

write.csv(auto, "auto.csv", row.names=FALSE)
