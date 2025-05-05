# libraries
library(dplyr)
library(tidyr)

# Load data
ng <- read.csv("Capstone/data/Utilities/natural_gas.csv")

# format the data
ng <- ng |>
  pivot_longer(-Year, names_to = "State", values_to = "Price_per_1000ft3")

# calculate annual cost (assuming 60,000 ft^3 per household/year - comes from EIA avg household data)
ng <- ng |>
  mutate(Price = Price_per_1000ft3 * 60)

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
ng <- ng %>%
  mutate(Region = case_when(
    State %in% northeast ~ "Northeast",
    State %in% midwest ~ "Midwest",
    State %in% south ~ "South",
    State %in% west ~ "West",
    TRUE ~ "Other"
  ))

# get csv
write.csv(ng, "ng.csv", row.names=FALSE)
