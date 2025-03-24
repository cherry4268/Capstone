# Required libraries
library(tidyverse)
library(lubridate)
library(forecast)
library(tseries)
library(ggplot2)
library(scales)

####################################################### Data Prep ###########################################################

# Upload the data
house <- read.csv("Capstone/data/Housing/housing.csv")


# Pivot the data
house <- house %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "date",
    values_to = "price"
  ) %>%
  select(State, date, price) %>%
  mutate(
    date = gsub("X", "", date),
    date = gsub("\\.", "-", date),
    date = mdy(date)
  )

# Group data by State and Year
house_year <- house %>%
  mutate(year = year(date)) %>%
  group_by(State, year) %>%
  summarize(avg_price = mean(price, na.rm = TRUE))


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
house <- house %>%
  mutate(Region = case_when(
    State %in% northeast ~ "Northeast",
    State %in% midwest ~ "Midwest",
    State %in% south ~ "South",
    State %in% west ~ "West",
    TRUE ~ "Other"
  ))

house_year <- house_year %>%
  mutate(Region = case_when(
    State %in% northeast ~ "Northeast",
    State %in% midwest ~ "Midwest",
    State %in% south ~ "South",
    State %in% west ~ "West",
    TRUE ~ "Other"
  ))



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
