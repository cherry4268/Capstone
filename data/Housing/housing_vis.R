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

# Upload the data
house <- read.csv("Capstone/data/Housing/housing.csv")

house_own <- read.csv("Capstone/data/Housing/house_own.csv")
house_rent <- read.csv("Capstone/data/Housing/house_rent.csv")


# Step 1: Group by state and calculate monthly averages across cities
house_own <- house_own %>%
  select(-RegionName) %>%
  group_by(StateName) %>%
  summarise(across(everything(), mean, na.rm = TRUE))

house_rent <- house_rent %>%
  select(-RegionName) %>%
  group_by(StateName) %>%
  summarise(across(everything(), mean, na.rm = TRUE))



# Step 2: Convert monthly values to annual by multiplying by 12
house_own <- house_own %>%
  mutate(across(-StateName, ~ .x * 12))

house_rent <- house_rent %>%
  mutate(across(-StateName, ~ .x * 12))




# Step 3: Reshape to long format and extract year
house_own <- house_own %>%
  pivot_longer(-StateName, names_to = "Date", values_to = "Price") %>%
  mutate(Year = str_extract(Date, "\\d{4}"))

house_rent <- house_rent %>%
  pivot_longer(-StateName, names_to = "Date", values_to = "Price") %>%
  mutate(Year = str_extract(Date, "\\d{4}"))


# Step 4: Group by state and year, and average across months
house_own <- house_own %>%
  group_by(State = StateName, Year) %>%
  summarise(Price = mean(Price, na.rm = TRUE), .groups = "drop")

house_rent <- house_rent %>%
  group_by(State = StateName, Year) %>%
  summarise(Price = mean(Price, na.rm = TRUE), .groups = "drop")


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
house_own <- house_own %>%
  mutate(Region = case_when(
    State %in% northeast ~ "Northeast",
    State %in% midwest ~ "Midwest",
    State %in% south ~ "South",
    State %in% west ~ "West",
    TRUE ~ "Other"
  ))

house_rent <- house_rent %>%
  mutate(Region = case_when(
    State %in% northeast ~ "Northeast",
    State %in% midwest ~ "Midwest",
    State %in% south ~ "South",
    State %in% west ~ "West",
    TRUE ~ "Other"
  ))

# Add full state names to your dataset
house_own <- house_own %>%
  mutate(State = state.name[match(State, state.abb)])

# Add full state names to your dataset
house_rent <- house_rent %>%
  mutate(State = state.name[match(State, state.abb)])

print(house_own %>% filter(if_any(everything(), is.na)))

# fill in NA cells (Washington dc)
house_own$State[is.na(house_own$State)] <- "District of Columbia"
house_rent$State[is.na(house_rent$State)] <- "District of Columbia"

# fill in missing 2015 prices with 2016 prices
house_rent[house_rent$State == "North Dakota" & house_rent$Year == 2015, "Price"] <- 10390.744
house_rent[house_rent$State == "South Dakota" & house_rent$Year == 2015, "Price"] <- 9554.138
house_rent[house_rent$State == "Vermont" & house_rent$Year == 2015, "Price"] <- 15424.493
house_rent[house_rent$State == "West Virginia" & house_rent$Year == 2015, "Price"] <- 10361.419





##### Print the csv files
write.csv(house_own, "house_own.csv", row.names=FALSE)
write.csv(house_rent, "house_rent.csv", row.names=FALSE)


########################################################## Line charts with Confidence Intervals ####################################################

# calculate state averages with confidence intervals
house_ci <- house %>%
  mutate(year = year(date)) %>%
  group_by(State, year) %>%
  summarize(
    avg_price = mean(price, na.rm = TRUE),
    sd_cost = sd(price, na.rm = TRUE),
    count = n(),
    se = sd_cost / sqrt(count),
    ci_lower = avg_price - qt(0.975, count-1) * se,
    ci_upper = avg_price + qt(0.975, count-1) * se,
    .groups = "drop"
    )

# function for line chart w/ CI
plot_ci <- function(data, states_to_plot = NULL) {
  # filter for specific state if requested
  if (!is.null(states_to_plot)) {
    data <- data %>% filter(State %in% states_to_plot)
  }
  
  # create plot
  ggplot(data, aes(x = year, y = avg_price, color = State, group = State)) +
    geom_line(linewidth = 1) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = State), alpha = 0.2, color = NA) +
    labs(
      title = "Average Housing cost by State",
      subtitle = "With 95% Confidence Intervals",
      x = "year",
      y = "Average Housing cost ($)",
      color = "State",
      fill = "State"
    ) +
    theme_minimal() +
    theme(
      legend.position = "right",
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 0, hjust = 0.5)
    )
}

# Plot all states
states_plot <- plot_ci(house_ci)
print(states_plot)

######### Regional CI plots

# add region to data
house_ci <- house_ci %>%
  mutate(Region = case_when(
    State %in% northeast ~ "Northeast",
    State %in% midwest ~ "Midwest",
    State %in% south ~ "South",
    State %in% west ~ "West",
    TRUE ~ "Other"
  ))

region_plot <- ggplot( house_ci,
                       aes(x = year, y = avg_price, color = State, group = State)) +
  geom_line(alpha = 0.7) +
  facet_wrap(~Region, scales = "free_y") +
  labs(
    title = "Average Housing cost by Region",
    x = "Year",
    y = "Average Housing cost ($)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(region_plot)
