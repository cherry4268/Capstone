# Required libraries
library(tidyverse)
library(lubridate)
library(forecast)
library(tseries)
library(ggplot2)
library(scales)

####################################################### Data Prep ###########################################################

# Upload the data
food <- read.csv("Capstone/data/Grocery/food_2019-2022.csv")

# Split the column into County and State using separate()
food <- food %>%
  separate(County..State, 
           into = c("County", "State"), 
           sep = ", ", 
           remove = FALSE)  # keep original column

# Dataframe that combines observations by state
state_food <- food %>%
  mutate(
    Cost.Per.Meal = as.numeric(gsub("[$,]", "", Cost.Per.Meal))  # Removes $ and commas
  ) %>%
  group_by(State, Year) %>%
  summarize(
    avg_cost_per_meal = mean(Cost.Per.Meal, na.rm = TRUE),
    counties = n(),
    .groups = "drop"
    )

print(state_food)


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

# add region to data
state_food <- state_food %>%
  mutate(Region = case_when(
    State %in% northeast ~ "Northeast",
    State %in% midwest ~ "Midwest",
    State %in% south ~ "South",
    State %in% west ~ "West",
    TRUE ~ "Other"
  ))

########################################################## Line charts with Confidence Intervals ####################################################

# calculate state averages with confidence intervals
state_food_ci <- food %>%
  mutate(Cost.Per.Meal = as.numeric(gsub("[$,]", "", Cost.Per.Meal))
  ) %>%
  group_by(State, Year) %>%
  summarize(
    avg_cost_per_meal = mean(Cost.Per.Meal, na.rm = TRUE),
    sd_cost = sd(Cost.Per.Meal, na.rm = TRUE),
    count = n(),
    se = sd_cost / sqrt(count),
    ci_lower = avg_cost_per_meal - qt(0.975, count-1) * se,
    ci_upper = avg_cost_per_meal + qt(0.975, count-1) * se,
    .groups = "drop"
  )

# function for line chart w/ CI
plot_ci <- function(data, states_to_plot = NULL) {
  # filter for specific state if requested
  if (!is.null(states_to_plot)) {
    data <- data %>% filter(State %in% states_to_plot)
  }
  
  # create plot
  ggplot(data, aes(x = Year, y = avg_cost_per_meal, color = State, group = State)) +
    geom_line(linewideth = 1) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = State), alpha = 0.2, color = NA) +
    labs(
      title = "Average Food Cost Per Meal by State",
      subtitle = "With 95% Confidence Intervals",
      x = "Year",
      y = "Average Cost Per Meal ($)",
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
states_plot <- plot_ci(state_food_ci)
print(states_plot)

######### Regional CI plots

# add region to data
state_food_ci <- state_food_ci %>%
  mutate(Region = case_when(
    State %in% northeast ~ "Northeast",
    State %in% midwest ~ "Midwest",
    State %in% south ~ "South",
    State %in% west ~ "West",
    TRUE ~ "Other"
  ))

region_plot <- ggplot( state_food_ci,
                       aes(x = Year, y = avg_cost_per_meal, color = State, group = State)) +
  geom_line(alpha = 0.7) +
  facet_wrap(~Region, scales = "free_y") +
  labs(
    title = "Average Food Cost Per Meal by Region",
    x = "Year",
    y = "Average Cost Per Meal ($)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(region_plot)


############################################################ Heat Map ################################################

ggplot(state_food, aes(x = Year, y = State, fill = avg_cost_per_meal)) +
  geom_tile(color = "white") +
  scale_fill_gradientn(
    colors = c("blue", "white", "red"),
    name = "Average Cost per Meal"
  ) +
  facet_wrap(~ Region, scales = "free_y") +
  labs (
    title = "Average Cost per Meal by State and Year",
    x = "Year",
    y = "State"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust =1),
    panel.grid = element_blank()
  )

