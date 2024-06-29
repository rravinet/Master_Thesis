library(Metrics) # Assuming the mse and rmse functions are from the Metrics package

stock_symbols <- c("AAPL", "BA", "IBM", "JPM", "MMM", "SPY")
horizons <- c(1, 5, 10, 20)

aaa <- read.csv("/Users/raphaelravinet/Code/BSE/Thesis/Results/Forecast_results_final/AAPL.csv/realised_garch_horizon_10.csv")
testing_mse <- aaa$forecast_data
length(aaa$forecast_data)
length(all_results$AAPL.csv$test_data$log_x_adj)
mse(all_results$AAPL.csv$test_data$log_x_adj[10:length(all_results$AAPL.csv$test_data$log_x_adj)], testing_mse)

# Base directory
base_dir <- "/Users/raphaelravinet/Code/BSE/Thesis/Results/Forecast_results_final/"

# Function to compute QLIKE for a single stock and horizon
compute_QLIKE <- function(stock_symbol, horizon) {
  # Paths to the GARCH and HAR GARCH prediction files
  garch_file <- paste0(base_dir, stock_symbol, ".csv/realised_garch_horizon_", horizon, ".csv")
  har_garch_file <- paste0(base_dir, stock_symbol, ".csv/realised_har_garch_horizon_", horizon, ".csv")
  
  # Read the prediction files
  garch_data <- read.csv(garch_file)
  har_garch_data <- read.csv(har_garch_file)
  
  # Assuming the data frame has a column named 'forecast_data'
  sigma_pred_garch <- garch_data$forecast_data
  exp_sigma_pred_garch <- exp(sigma_pred_garch)
  sigma_pred_har_garch <- har_garch_data$forecast_data
  exp_sigma_pred_har_garch <- exp(sigma_pred_har_garch)
  
  # Retrieve log_x_adj from the all_results object and adjust for the horizon
  log_x_adj <- all_results[[paste0(stock_symbol, ".csv")]]$test_data$log_x_adj[horizon:length(all_results[[paste0(stock_symbol, ".csv")]]$test_data$log_x_adj)]
  sigma_proxy <- exp(log_x_adj)
  
  # Ensure lengths match
  sigma_proxy <- head(sigma_proxy, length(sigma_pred_garch))
  
  # Compute QLIKE for GARCH
  a_garch <- sigma_proxy / exp_sigma_pred_garch
  b_garch <- log(a_garch)
  c_garch <- a_garch - b_garch - 1
  d_garch <- sum(c_garch) / length(exp_sigma_pred_garch)
  mse_rg <- mse(log_x_adj, sigma_pred_garch)
  rmse_rg <- rmse(log_x_adj, sigma_pred_garch)
  
  # Compute QLIKE for HAR GARCH
  a_har_garch <- sigma_proxy / exp_sigma_pred_har_garch
  b_har_garch <- log(a_har_garch)
  c_har_garch <- a_har_garch - b_har_garch - 1
  d_har_garch <- sum(c_har_garch) / length(exp_sigma_pred_har_garch)
  mse_rhg <- mse(log_x_adj, sigma_pred_har_garch)
  rmse_rhg <- rmse(log_x_adj, sigma_pred_har_garch)
  
  return(list(garch = d_garch, har_garch = d_har_garch, mse_garch = mse_rg, rmse_garch = rmse_rg, mse_har_garch = mse_rhg, rmse_har_garch = rmse_rhg))
}

# Create a data frame to store the results
results <- data.frame(
  Stock = character(),
  Horizon = integer(),
  QLIKE_GARCH = numeric(),
  QLIKE_HAR_GARCH = numeric(),
  MSE_GARCH = numeric(),
  MSE_HAR_GARCH = numeric(),
  RMSE_GARCH = numeric(),
  RMSE_HAR_GARCH = numeric(),
  stringsAsFactors = FALSE
)

# Loop over each stock symbol and horizon to compute QLIKE
for (symbol in stock_symbols) {
  for (horizon in horizons) {
    qlike_values <- compute_QLIKE(symbol, horizon)
    results <- rbind(results, data.frame(
      Stock = symbol,
      Horizon = horizon,
      QLIKE_GARCH = qlike_values$garch,
      QLIKE_HAR_GARCH = qlike_values$har_garch,
      MSE_GARCH = qlike_values$mse_garch,
      MSE_HAR_GARCH = qlike_values$mse_har_garch,
      RMSE_GARCH = qlike_values$rmse_garch,
      RMSE_HAR_GARCH = qlike_values$rmse_har_garch
    ))
  }
}

print(results)
