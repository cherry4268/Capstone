# Libraries
library(shiny)
library(maps)
library(mapproj)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(plotly)

# Get US states map data
us_states <- map_data("state")

# random data (temporary)
state_data <- data.frame(
  state = unique(us_states$region),
  value = runif(length(unique(us_states$region)), 5, 15)
)

# Transportation: auto insurance, gas
# Food
food <- read.csv("Capstone/data/Grocery/food_2019-2022.csv")
food$Cost.Per.Meal <- as.numeric(gsub("\\$|,", "", food$Cost.Per.Meal))

food <- food %>%
  group_by(State, Year) %>%
  summarize(
    Cost.Per.Meal = mean(Cost.Per.Meal, na.rm = TRUE),
    county_count = n(),
    .groups = 'drop')

write.csv(food, "food.csv", row.names = FALSE)

food <- food %>%
  mutate(
    state = tolower(State)
  ) %>%
  select()

# Housing

# Define UI
ui <- fluidPage(
  titlePanel("U.S. States Map"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("colorScheme", "Color Scheme:",
                  choices = c("Blues", "Reds", "Greens", "Purples", "Oranges"),
                  selected = "Blues"),
      
      selectInput("variable", "Variable to Display:",
                  choices = c("Cost Per Meal" = "value"), # Fixed: "value" was capitalized
                  selected = "value"),
      
      sliderInput("year", "Year:",
                  min = 2010, max = 2024, value = 2024, step = 1)
    ),
    
    mainPanel(
      plotlyOutput("stateMap", height = "600px"),
      br(),
      dataTableOutput("stateData")
    )
  )
)

# Define Server
server <- function(input, output) {
  
  # reactive data filtering
  filtered_data <- reactive({
    # fake data
    state_data
  })
  
  # generate the map
  output$stateMap <- renderPlotly({ 
    # join map data with metrics
    map_data <- us_states %>%
      left_join(filtered_data(), by = c("region" = "state"))
    
    # variable label for display
    var_label <- ifelse(input$variable == "value", "Cost Per Meal ($)", input$variable)
    
    # create the map
    g <- ggplot(map_data, aes(x = long, y = lat, group = group, fill = value,
                              text = paste0(
                                "<b>", tools::toTitleCase(region), "</b><br>",
                                var_label, ": $", round(value, 2)
                              ))) +
      geom_polygon(color = "white", size = 0.2) +
      coord_map("albers", lat0 = 45, lat1 = 45) +
      scale_fill_distiller(
        palette = input$colorScheme,
        direction = 1,
        name = var_label,
        breaks = seq(5, 15, by = 5), 
        labels = scales::dollar_format()
      ) +
      labs(
        title = paste(" Data by State -", input$year),
        caption = "Sources: data"
      ) +
      theme_minimal() +
      theme(
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16)
      )
    
    # Convert to plotly with hover information
    ggplotly(g, tooltip = "text") %>%
      layout(hoverlabel = list(bgcolor = "white", font = list(family = "Arial", size = 12))) %>%
      config(displayModeBar = FALSE)
  })
  
  # Display data table
  output$stateData <- renderDataTable({
    filtered_data() %>%
      arrange(desc(value)) %>%
      mutate(
        state = tools::toTitleCase(state),
        value = scales::dollar(value)
      ) %>%
      rename(
        State = state,
        `Cost Per Meal` = value
      )
  })
}

# run app
shinyApp(ui = ui, server = server)