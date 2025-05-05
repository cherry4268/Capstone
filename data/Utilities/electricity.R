# libraries
library(dplyr)
library(tidyr)

# Load data
electricity <- read.csv("Capstone/data/Utilities/energy.csv")
colnames(electricity) <- gsub("^X", "", colnames(electricity))


# format the data
electricity <- electricity |>
  pivot_longer(-State, names_to = "Year", values_to = "Price_per_kwh")

# calculate annual cost (assuming 60,000 ft^3 per household/year - comes from EIA avg household data)
electricity <- electricity |>
  mutate(Price = (Price_per_kwh /100) * 10632)

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
electricity <- electricity %>%
  mutate(Region = case_when(
    State %in% northeast ~ "Northeast",
    State %in% midwest ~ "Midwest",
    State %in% south ~ "South",
    State %in% west ~ "West",
    TRUE ~ "Other"
  ))

# get csv
write.csv(electricity, "electricity.csv", row.names=FALSE)
