# Required libraries
library(tidyverse)
library(lubridate)
library(forecast)
library(tseries)
library(ggplot2)
library(scales)
library(dplyr)
library(tidyr)

####################################################### Data Prep ###########################################################

# Upload the data properly
utilities <- read.csv("Capstone/data/Utilities/utilities.csv", fileEncoding = "UTF-8-BOM")

# Drop all the junk columns
utilities <- utilities[, 1:9]  # Keep only State + 2015-2022

# Fix column names
names(utilities)[1] <- "State"

# Format the state name
utilities <- utilities %>%
  mutate(State = state.name[match(State, state.abb)])

# Drop garbage rows
utilities <- utilities[-c(52:56), ]

# Fill in District of Columbia manually
utilities <- utilities %>%
  mutate(State = ifelse(is.na(State), "District of Columbia", State))

# Define regions
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

# Add the Region column BEFORE reshaping
utilities <- utilities %>%
  mutate(Region = case_when(
    State %in% northeast ~ "Northeast",
    State %in% midwest ~ "Midwest",
    State %in% south ~ "South",
    State %in% west ~ "West",
    State == "District of Columbia" ~ "South",  # ✅ You can choose South or make a custom "DC"
    TRUE ~ "Other"
  ))

# Now reshape to long format (keeping Region!)
# Reshape to long format (keeping Region!) and extract Year directly
utilities <- utilities %>%
  pivot_longer(cols = c(-State, -Region), names_to = "Year", values_to = "Price") %>%
  mutate(Year = str_extract(Year, "\\d{4}"))  # Extract 2015, 2016, etc.

write.csv(utilities, "utilities.csv", row.names=FALSE)
