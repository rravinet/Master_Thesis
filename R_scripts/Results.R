##This script calculate the results for each dataset and put it into a table###
library(dplyr)
library(readr)
library(MCS)


testing <- read.csv('/Users/raphaelravinet/Code/BSE/Thesis/KAI_folder/Forecast_results/AAPL.csv/realised_garch_horizon_5.csv')

# Path to your forecast results directory
forecast_dir <- "/Users/raphaelravinet/Code/BSE/Thesis/KAI_folder/Forecast_results_final/"

QLIKE <- function(actual, forecast) {
  term_1 <- actual / forecast
  term_2 <- - log((actual / forecast))
  QLIKE <- sum(term_1 + term_2 - 1)
  return(QLIKE)
}

#MSE, QLIKE, and RMSE
calculate_metrics <- function(actual, forecast) {
  mse <- mse(actual, forecast)
  qlike <- QLIKE(actual,forecast)
  rmse <- rmse(actual, forecast)
  return(data.frame(MSE = mse, QLIKE = qlike, RMSE = rmse))
}

write.csv(results_wide, "/Users/raphaelravinet/Code/BSE/Thesis/Results/results.csv", row.names = FALSE)

results_df <- data.frame()

for (stock in names(all_results)) {
  actual <- all_results[[stock]]$test_data$log_x_adj
  for (model in c("realised_garch_horizon_", "realised_har_garch_horizon_")) {
    for (horizon in c(1, 5, 10, 20)) {
      model_name <- paste0(model, horizon)
      forecast_file <- file.path(forecast_dir, stock, paste0(model_name, ".csv"))
      if (file.exists(forecast_file)) {
        forecast_data <- read_csv(forecast_file)
        forecast <- forecast_data$forecast_data
        actual_adjusted <- actual[1:(length(actual) - (horizon - 1))]
        forecast <- forecast[1:length(actual_adjusted)]
        metrics <- calculate_metrics(actual_adjusted, forecast)
        metrics$Stock <- stock
        metrics$Model <- model_name
        results_df <- rbind(results_df, metrics)
      }
    }
  }
}

results_df <- results_df %>%
  separate(Model, into = c("Model", "Horizon"), sep = "_horizon_") %>%
  unite("Metric_Model_Horizon", Model, Horizon, sep = "_horizon_") %>%
  pivot_wider(names_from = Metric_Model_Horizon, values_from = c(MSE, QLIKE, RMSE))

# Ensure all columns are atomic vectors
results_wide <- data.frame(lapply(results_df, as.vector))

write.csv(results_wide, "/Users/raphaelravinet/Code/BSE/Thesis/Results/results.csv", row.names = FALSE)
