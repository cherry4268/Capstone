# ========================= libraries =========================
library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(maps)
library(RColorBrewer)
library(sf)
library(DT)
library(data.table) 
library(stringr)
library(shinydashboard)
library(billboarder)
library(plotly)

# ========================= Upload the Data =========================
predictor_forecasts <- readRDS("predictor_forecasts.rds")
print("Loaded predictor_forecasts columns:")
print(names(predictor_forecasts))

predictor_forecasts2 <- readRDS("predictor_forecasts2.rds")
print("Loaded predictor_forecasts2 columns:")
print(names(predictor_forecasts2))

# Convert data.table to data.frame if needed
if (is.data.table(predictor_forecasts2)) {
  predictor_forecasts2 <- as.data.frame(predictor_forecasts2)
  print("Converted data.table to data.frame")
}

# Check columns
print("Columns in predictor_forecasts:")
print(names(predictor_forecasts))


print("Columns in predictor_forecasts2:")
print(names(predictor_forecasts2))

# ========================= Prep data for mapping =========================
prep_map_data <- function(predictor_forecasts) {
  states_map <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
  states_map$ID <- gsub(":", "", states_map$ID) 
  states_map$ID <- stringr::str_to_title(states_map$ID)
  
  map_data <- left_join(states_map, predictor_forecasts, by = c("ID" = "State"))
  return(map_data)
}

# ========================= Shiny App UI =========================
header <- dashboardHeader(title = "State-level PCE Forecasts")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Map", tabName = "map_tab", icon = icon("globe")),
    menuItem("Data Table", tabName = "table_tab", icon = icon("table")),
    menuItem("Documentation", tabName = "doc_tab", icon = icon("book")),
    menuItem("Methodology", tabName = "method_tab", icon = icon("cogs"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "map_tab",
            fluidRow(
              box(width = 3, status = "primary", solidHeader = TRUE, title = "Controls",
                  selectInput("Year", "Select Year:",
                              choices = sort(unique(predictor_forecasts$Year[predictor_forecasts$Year > 2022])),
                              selected = min(predictor_forecasts$Year[predictor_forecasts$Year > 2022])),
                  radioButtons("ownership", "Select Ownership Type:",
                               choices = c("Owner" = "own", "Renter" = "rent"),
                               selected = "own"),
                  radioButtons("model", "Select Model:",
                               choices = c("ARDL Model" = "rPCE_dollars",
                                           "XGBoost Model" = "rPCE_xgb",
                                           "Blended Forecast" = "rPCE_final"),
                               selected = "rPCE_final"),
                  br(),
                  downloadButton("downloadData", "Download Data")
              ),
              box(width = 9, solidHeader = TRUE, title = "Map of Forecasted PCE",
                  leafletOutput("map", height = 500)
              )
            ),
            fluidRow(
              box(width = 12, solidHeader = TRUE, title = "Forecasted Expenses Breakdown",
                  billboarderOutput("forecast_pie", height = "350px")
              )
            )
    ),
    tabItem(tabName = "table_tab",
            fluidRow(
              box(width = 12, solidHeader = TRUE, title = "PCE Forecasts Data Table",
                  DT::dataTableOutput("table")
              )
            )
    ),
    tabItem(tabName = "doc_tab",
            fluidRow(
              box(width = 12, solidHeader = TRUE, title = "Documentation",
                  h3("About the Project"),
                  p("This application forecasts state-level Personal Consumption Expenditure (PCE) using multiple modeling approaches. Users can explore forecasts by year, ownership type, and modeling method."),
                  br(),
                  h3("Data Sources"),
                  p("- Zillow Home Value Index (ZHVI): https://www.zillow.com/research/data/"),
                  p("- Bureau of Economic Analysis Real Personal Consumption Expenditures by State (per Capita): https://www.bea.gov/news/2024/real-personal-consumption-expenditures-state-and-real-personal-income-state-and"),
                  p("- U.S. Energy Information Administration (EIA) for utility Consumption and Expenditures: https://www.eia.gov/state/data.cfm#ConsumptionExpenditures"),
                  p("- National Association of Insurance Commissioners (NAIC) for auto insurance costs: https://content.naic.org/article/naic-releases-latest-auto-insurance-database-average-premium-supplement, https://content.naic.org/article/news-release-naic-releases-2017-2018-auto-insurance-database-report"),
                  p("- Bureau of Labor Statisitics Gasoline by Census Region: https://data.bls.gov/dataQuery/find?st=0&r=20&q=gasoline&more=0&fq=cg:[Geography]"),
                  p("- Map the Meal Gap food cost data: https://map.feedingamerica.org/"),
                  br(),
                  h3("Models Used"),
                  p("- ARDL (Auto-Regressive Distributed Lag) Model"),
                  p("- XGBoost Machine Learning Model"),
                  p("- Blended Forecast combining ARDL and XGBoost outputs")
              )
            )
    ),
    
    tabItem(tabName = "method_tab",
            fluidRow(
              box(width = 12, solidHeader = TRUE, title = "Methodology",
                  h3("Forecasting Process"),
                  p("1. Data from multiple sources were aggregated and cleaned."),
                  p("2. Predictors such as utilities, food, housing, transportation costs were forecasted individually using ARIMA models."),
                  p("3. Recursive forecasting was performed for the response variable (PCE) using Panel ARDL models and XGBoost models separately."),
                  p("4. Blended forecasts were created by taking a weighted average of ARDL and XGBoost predictions."),
                  br(),
                  h3("Technical Notes"),
                  p("- All financial variables were log-transformed during model estimation and back-transformed for interpretability."),
                  p("- Fixed Effects were incorporated at the state level to control for unobserved heterogeneity."),
                  p("- Forecast accuracy metrics include RMSE, MAE, MAPE, and Median Absolute Error.")
              )
            )
    )
    
  )
)

ui <- dashboardPage(
  header,
  sidebar,
  body
)

# ========================= Shiny Server =========================
server <- function(input, output, session) {
  
  # Reactive filtered data
  filtered_data <- reactive({
    req(input$Year)
    dataset <- if (input$ownership == "own") predictor_forecasts else predictor_forecasts2
    dataset %>% filter(Year == input$Year)
  })
  
  # Reactive map data
  map_data <- reactive({
    prep_map_data(filtered_data())
  })
  
  # Render Leaflet Map
  output$map <- renderLeaflet({
    data <- map_data()
    selected_col <- input$model
    req(selected_col %in% names(data))
    
    if (all(is.na(data[[selected_col]]))) {
      showNotification("No data available for selected model", type = "error")
      return(NULL)
    }
    
    valid_values <- data[[selected_col]][!is.na(data[[selected_col]])]
    
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = valid_values,
      na.color = "#CCCCCC"
    )
    
    popup_content <- sapply(1:nrow(data), function(i) {
      val <- data[[selected_col]][i]
      id <- data$ID[i]
      if (is.na(val)) {
        return(paste0("<strong>", toupper(id), "</strong><br/>PCE Value: No data"))
      } else {
        return(paste0("<strong>", toupper(id), "</strong><br/>PCE Value $", formatC(val, format = "f", big.mark = ",", digits = 0)))
      }
    })
    
    leaflet(data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~ifelse(is.na(get(selected_col)), "#CCCCCC", pal(get(selected_col))),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 2,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        popup = popup_content,
        label = ~toupper(ID),
        layerId = ~ID
      ) %>%
      addLegend(
        pal = pal,
        values = valid_values,
        opacity = 0.7,
        title = "PCE Value ($)",
        position = "bottomright"
      )
  })
  
  # Render Data Table
  output$table <- DT::renderDataTable({
    table_data <- filtered_data() %>%
      select(State, Year, rPCE_dollars, rPCE_xgb, rPCE_final) %>%
      arrange(State) %>%
      rename(
        "State" = State,
        "Year" = Year,
        "ARDL Forecast ($)" = rPCE_dollars,
        "XGBoost Forecast ($)" = rPCE_xgb,
        "Blended Forecast ($)" = rPCE_final
      )
    
    DT::datatable(table_data, options = list(pageLength = 10, searching = TRUE, ordering = TRUE)) %>%
      formatCurrency(columns = c("ARDL Forecast ($)", "XGBoost Forecast ($)", "Blended Forecast ($)"),
                     currency = "$", digits = 0)
  })
  
  # Download handler
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("PCE_Forecasts_", input$Year, ".csv", sep = "")
    },
    content = function(file) {
      download_data <- filtered_data() %>%
        select(State, Year, rPCE_dollars, rPCE_xgb, rPCE_final)
      write.csv(download_data, file, row.names = FALSE)
    }
  )
  
  # Selected State for Pie Chart
  selected_state <- reactiveVal(NULL)
  
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    if (!is.null(click$id)) {
      selected_state(click$id)
    }
  })
  
  output$forecast_pie <- renderBillboarder({
    state_id <- selected_state()
    req(state_id)
    
    data <- filtered_data() %>%
      filter(State == state_id)
    req(nrow(data) > 0)
    
    housing_value <- if (input$ownership == "own") data$house_own_forecast[1] else data$house_rent_forecast[1]
    
    # Create data frame for the chart
    chart_data <- data.frame(
      group = c("Utilities", "Food", "Auto", "Housing"),
      value = c(
        data$utilities_forecast[1],
        data$food_forecast[1],
        data$auto_forecast[1] + data$gas_forecast[1],
        housing_value
      )
    )
    
    # Create a donut chart
    billboarder() %>%
      bb_piechart(chart_data) %>%
      bb_donut(
        title = paste("Forecast for", state_id, input$Year),
        width = 3  # Controls the thickness of the donut ring
      ) %>%
      bb_legend(position = "right") %>%
      bb_tooltip(
        format = list(
          value = htmlwidgets::JS(
            "function(value, ratio, id) { return Math.round(ratio * 100) + '%'; }"
          )
        )
      )
  })
}

# ========================= Run the App =========================
shinyApp(ui, server)
