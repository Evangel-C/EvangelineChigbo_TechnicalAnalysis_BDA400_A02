library(quantmod)

load_stock_data <- function(portfolio_file) {
  
  # Read stock symbols from portfolio.txt
  symbols <- readLines(portfolio_file)
  
  # Create a list to store each stock's data
  stock_data <- list()
  
  # Download data for each stock
  for (symbol in symbols) {
    stock_data[[symbol]] <- getSymbols(
      symbol,
      src = "yahoo",
      auto.assign = FALSE
    )
  }
  
  return(stock_data)
}
# Load stock data from portfolio.txt
portfolio_data <- load_stock_data("portfolio.txt")
# Convert each stock's data to separate data frames
AAPL <- data.frame(portfolio_data$AAPL)
MSFT <- data.frame(portfolio_data$MSFT)
AMZN <- data.frame(portfolio_data$AMZN)
GOOGL <- data.frame(portfolio_data$GOOGL)
TSLA <- data.frame(portfolio_data$TSLA)
# Function to calculate basic statistics
calculate_statistics <- function(stock_data) {
  
  # Use the Adjusted Close price
  prices <- stock_data[, 6]
  
  # Calculate statistics
  mean_price <- mean(prices, na.rm = TRUE)
  median_price <- median(prices, na.rm = TRUE)
  standard_deviation <- sd(prices, na.rm = TRUE)
  
  # Calculate mode
  mode_value <- as.numeric(names(sort(table(prices), decreasing = TRUE)[1]))
  
  # Calculate 20-day moving average
  moving_average <- mean(tail(prices, 20), na.rm = TRUE)
  
  # Store results in a list
  statistics <- list(
    Moving_Average = moving_average,
    Mean = mean_price,
    Mode = mode_value,
    Median = median_price,
    Standard_Deviation = standard_deviation
  )
  
  return(statistics)
  # Calculate statistics for each stock
  AAPL_stats <- calculate_statistics(AAPL)
  MSFT_stats <- calculate_statistics(MSFT)
  AMZN_stats <- calculate_statistics(AMZN)
  GOOGL_stats <- calculate_statistics(GOOGL)
  TSLA_stats <- calculate_statistics(TSLA)
  
  # Store all statistics in a structured list
  all_statistics <- list(
    AAPL = AAPL_stats,
    MSFT = MSFT_stats,
    AMZN = AMZN_stats,
    GOOGL = GOOGL_stats,
    TSLA = TSLA_stats
  )
  # Function to display stock data
  display_stock_data <- function(stock_data, stock_symbol) {
    
    cat("\n==============================\n")
    cat("Stock:", stock_symbol, "\n")
    cat("==============================\n")
    
    cat("\nFirst 6 rows:\n")
    print(head(stock_data))
    
    cat("\nLast 6 rows:\n")
    print(tail(stock_data))
    
    cat("\nData dimensions:\n")
    print(dim(stock_data))
    
    cat("\nColumn names:\n")
    print(names(stock_data))
  }
  # Display information for each stock
  display_stock_data(AAPL, "AAPL")
  display_stock_data(MSFT, "MSFT")
  display_stock_data(AMZN, "AMZN")
  display_stock_data(GOOGL, "GOOGL")
  display_stock_data(TSLA, "TSLA")
  
}