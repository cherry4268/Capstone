# Required libraries
library(tidyverse)  # For data manipulation
library(stats)      # For STL decomposition
library(seasonal)   # For X-13ARIMA-SEATS
library(lubridate)  # For date handling
library(ggplot2)    # For plotting

# List of file paths
file_paths <- c(
  "Capstone/data/Grocery/food_bev/food_bev_midwestA.csv",
  "Capstone/data/Grocery/food_bev/food_bev_midwestBC.csv",
  "Capstone/data/Grocery/food_bev/food_bev_westA.csv",
  "Capstone/data/Grocery/food_bev/food_bev_westBC.csv",
  "Capstone/data/Grocery/food_bev/food_bev_southA.csv",
  "Capstone/data/Grocery/food_bev/food_bev_southBC.csv",
  "Capstone/data/Grocery/food_bev/food_bev_neA.csv",
  "Capstone/data/Grocery/food_bev/food_bev_neBC.csv"
)

# Prepare time series from dataset (Function)
prep_time_series <- function(df) {
  # Extract month number from Period column
  df <- df%>%
    mutate(
      Month = as.numeric(str_extract(Period, "\\d+")),
      date = make_date(Year, Month, 1) # Create data: Year, Month, Day=1
    ) %>%
    arrange(date) # Sort by date
  
  # Create time series object
  ts_data <- ts(df$Value,
                start = c(min(df$Year), min(df$Month[df$Year == min(df$Year)])),
                frequency = 12)
  return(list(df = df, ts_data = ts_data))
}

# Apply STL Decomposition (Function)
apply_stl <- function(ts_data) {
  # apply STL decomposition
  stl_result <- stl(ts_data, s.window = "periodic")
  
  # Extract components
  trend <- stl_result$time.series[, "trend"]
  seasonal <- stl_result$time.series[, "seasonal"]
  remainder <- stl_result$time.series[, "remainder"]
  
  # Calculate seasonally adjusted series
  seasonally_adjusted <- trend + remainder
  
  # Return as data frame
  result_df <- data.frame(
    observed = as.numeric(ts_data),
    trend = as.numeric(trend),
    seasonal = as.numeric(seasonal),
    remainder = as.numeric(remainder),
    seasonally_adjusted = as.numeric(seasonally_adjusted)
  )
  
  return(result_df)
}

# Plot decomposition results (Function)
plot_decomposition <- function(result_df, title) {
  # Convert to long format for ggplot
  result_long <- result_df %>%
    mutate(index = 1:n()) %>%
    pivot_longer(cols = -index, names_to = "component", values_to = "value")
  
  # Create plot
  p <- ggplot(result_long, aes(x = index, y = value)) +
    geom_line() +
    facet_wrap(~ component, scales = "free_y", ncol = 1) +
    labs(title = title, x = "Time", y = "Value") +
    theme_minimal()
  return(p)
}

# Create output directory
dir.create("adjusted_data", showWarnings = FALSE)

# Process each file
results_list <- list()

for (file_path in file_paths) {
  # Extract name from file path
  file_name <- tools::file_path_sans_ext(basename(file_path))
  
  message("Processing ", file_name, "...")
  
  # Read CSV file
  df <- read.csv(file_path)
  
  # Prepare time series
  ts_prepared <- prep_time_series(df)
  original_df <- ts_prepared$df
  ts_data <- ts_prepared$ts_data
  
  # Apply STL decomposition
  stl_results <- apply_stl(ts_data)
  
  # Store results for each dataset
  results_list[[file_name]] <- list(
    original_df = original_df,
    stl_results = stl_results
  )
  
  # Create merged dataset w/ seasonal adjustments
  dates <- original_df$date
  
  # Add results back to original dataframe
  adjusted_df <- original_df %>%
    mutate(
      Value_STL_Adjusted = stl_results$seasonally_adjusted
    )
  
  # Save to CSV
  write.csv(adjusted_df, file = paste0("adjusted_data/", file_name, "_seasonally_adjusted.csv"), row.names = FALSE)
  
  # Create comparison plot (X-13ARIMA-SEATS needs to B added)
  p <- ggplot(adjusted_df, aes(x = date)) +
    geom_line(aes(y = Value, color = "Original"), alpha = 0.7) +
    geom_line(aes( y = Value_STL_Adjusted, color = "STL Adjusted"), alpha = 0.7) +
    labs(
      title = paste("COmparison of Original and Seasonally Adjusted Values -", file_name),
      x = "Date",
      y = "Price Index",
      color = "Series"
    ) +
    theme_minimal() +
    scale_color_manual(values = c(" Original" = "blue", "STL Adjusted" = "red"))
  
  # Save plot
  ggsave(paste0("adjusted_data/", file_name, "comparison.png"), p, width = 12, height = 6)
  
  #Generate & Save decomposition plots
  stl_plot <- plot_decomposition(stl_results, paste("STL Decomposition -", file_name))
  ggsave(paste0("adjusted_data/", file_name, "_stl_decomposition.png"), stl_plot, width = 10, height =10)
  
  message("Successfully processed", file_name)
}


message("All adjusted data saved to adjusted_data/ directory")
