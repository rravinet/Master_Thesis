library(esback)
t <-all_results$AAPL.csv$test_data
##### EITHER THIS ONE
##### FINAL WORKING
compute_fz_loss <- function(returns, VaR, ES, alpha) {
  tryCatch({
    FZ_loss_result <- FZLoss(data = returns, VaR = -VaR, ES = -ES, alpha = alpha)
    return(mean(FZ_loss_result))
  }, error = function(e) {
    print(paste("Error in compute_fz_loss:", e$message))
    return(NA)
  })
}
apple_results <- all_results$AAPL.csv$test_data$returns
apple_results_h10 <- apple_results[10:length(apple_results)]
apple_rg_var_001_h1 <- all_forecasts$AAPL.csv$realised_garch_horizon_1$VaR_0.01
apple_rg_var_001_h10 <- all_forecasts$AAPL.csv$realised_garch_horizon_10$VaR_0.01[10:length(all_forecasts$AAPL.csv$realised_garch_horizon_10$VaR_0.01)]
apple_var_rhg_001_h1 <- all_forecasts$AAPL.csv$realised_har_garch_horizon_1$VaR_0.01
apple_var_rhg_001_h10 <- all_forecasts$AAPL.csv$realised_har_garch_horizon_10$VaR_0.01[10:length(all_forecasts$AAPL.csv$realised_har_garch_horizon_10$VaR_0.01)]

apple_rg_es_001_h1 <- all_forecasts$AAPL.csv$realised_garch_horizon_1$ES_0.01
apple_rg_es_001_h10 <- all_forecasts$AAPL.csv$realised_garch_horizon_10$ES_0.01[10:length(all_forecasts$AAPL.csv$realised_garch_horizon_10$ES_0.01)]
apple_var_es_001_h1 <- all_forecasts$AAPL.csv$realised_har_garch_horizon_1$ES_0.01
apple_var_es_001_h10 <- all_forecasts$AAPL.csv$realised_har_garch_horizon_10$ES_0.01[10:length(all_forecasts$AAPL.csv$realised_har_garch_horizon_10$ES_0.01)]

backtest_rg_es_h1 <- esr_backtest(r = apple_results, q = apple_rg_var_001_h1, e = apple_rg_es_001_h1, alpha = 0.01, version = 1)
backtest_rg_es_h10 <- esr_backtest(r = apple_results_h10, q = apple_rg_var_001_h10, e = apple_rg_es_001_h10, alpha = 0.01, version = 1)
backtest_rhg_es_h1 <- esr_backtest(r = apple_results, q = apple_var_rhg_001_h1, e = apple_var_es_001_h1, alpha = 0.01, version = 1)
backtest_rhg_es_h10 <- esr_backtest(r = apple_results_h10, q = apple_var_rhg_001_h10, e = apple_var_es_001_h10, alpha = 0.01, version = 1)

ESR_p_value_rg_h1 <- backtest_rg_es_h1$pvalue_twosided_asymptotic
ESR_p_value_rg_h10 <- backtest_rg_es_h10$pvalue_twosided_asymptotic
ESR_p_value_rhg_h1 <- backtest_rhg_es_h1$pvalue_twosided_asymptotic
ESR_p_value_rhg_h10 <- backtest_rhg_es_h10$pvalue_twosided_asymptotic
print(ESR_p_value_rg_h1)
print(ESR_p_value_rg_h10)
print(ESR_p_value_rhg_h1)
print(ESR_p_value_rhg_h10)


####ESR TEST####

# Initialize a data frame to store ESR p-values
esr_p_values_df <- data.frame(
  Stock = character(),
  GARCH_Horizon_1 = numeric(),
  GARCH_Horizon_10 = numeric(),
  HAR_GARCH_Horizon_1 = numeric(),
  HAR_GARCH_Horizon_10 = numeric(),
  stringsAsFactors = FALSE
)

###Fitting SPY##
returns_spy_10 <- returns_spy[10:length(returns_spy)]
rg_spy_h1 <- read.csv("/Users/raphaelravinet/Code/BSE/Thesis/Results/Forecast_results_final/Forecast_results_VAR_ES/SPY.csv/realised_garch_horizon_1.csv")
rhg_spy_h1 <- read.csv("/Users/raphaelravinet/Code/BSE/Thesis/Results/Forecast_results_final/Forecast_results_VAR_ES/SPY.csv/realised_har_garch_horizon_1.csv")
rg_spy_h10 <- read.csv("/Users/raphaelravinet/Code/BSE/Thesis/Results/Forecast_results_final/Forecast_results_VAR_ES/SPY.csv/realised_garch_horizon_10.csv")
rhg_spy_h10 <- read.csv("/Users/raphaelravinet/Code/BSE/Thesis/Results/Forecast_results_final/Forecast_results_VAR_ES/SPY.csv/realised_har_garch_horizon_10.csv")
returns_spy <- all_results$SPY.csv$test_data$returns

rg_spy_esrtest <- esr_backtest(r = all_results, q = rg_spy_h1$VaR_0.01, e = rg_spy_h1$ES_0.01, alpha = 0.01, version = 1)
rhg_spy_esrtest <- esr_backtest(r = returns_spy, q = rhg_spy_h1$VaR_0.01, e = rhg_spy_h1$ES_0.01, alpha = 0.01, version = 1)
rg_spy_esrtest10 <- esr_backtest(r = returns_spy_10, q = rg_spy_h10$VaR_0.01[10:length(rg_spy_h10$VaR_0.01)], e = rg_spy_h10$ES_0.01[10:length(rg_spy_h10$ES_0.01)], alpha = 0.01, version = 1)
rhg_spy_esrtest10 <- esr_backtest(r = returns_spy_10, q = rhg_spy_h10$VaR_0.01[10:length(rhg_spy_h10$VaR_0.01)], e = rhg_spy_h10$ES_0.01[10:length(rhg_spy_h10$ES_0.01)], alpha = 0.01, version = 1)

rg_spy_esrtest$pvalue_twosided_asymptotic
rhg_spy_esrtest$pvalue_twosided_asymptotic
rg_spy_esrtest10$pvalue_twosided_asymptotic
rhg_spy_esrtest10$pvalue_twosided_asymptotic
# Loop through each stock and perform the ESR backtest
for (stock_name in filtered_stock_names) {
  actual_returns <- all_results[[stock_name]]$test_data$returns[10:length(actual_returns)]
  actual_returns_h10 <- actual_returns[10:length(actual_returns)]
  
  # GARCH Model
  garch_var_001_h1 <- all_forecasts[[stock_name]]$realised_garch_horizon_1$VaR_0.01
  garch_var_001_h10 <- all_forecasts[[stock_name]]$realised_garch_horizon_10$VaR_0.01[10:length(all_forecasts[[stock_name]]$realised_garch_horizon_10$VaR_0.01)]
  
  
  garch_es_001_h1 <- all_forecasts[[stock_name]]$realised_garch_horizon_1$ES_0.01
  garch_es_001_h10 <- all_forecasts[[stock_name]]$realised_garch_horizon_10$ES_0.01[10:length(all_forecasts[[stock_name]]$realised_garch_horizon_10$ES_0.01)]
  
  # HAR-GARCH Model
  har_garch_var_001_h1 <- all_forecasts[[stock_name]]$realised_har_garch_horizon_1$VaR_0.01
  har_garch_var_001_h10 <- all_forecasts[[stock_name]]$realised_har_garch_horizon_10$VaR_0.01[10:length(all_forecasts[[stock_name]]$realised_har_garch_horizon_10$VaR_0.01)]

  
  har_garch_es_001_h1 <- all_forecasts[[stock_name]]$realised_har_garch_horizon_1$ES_0.01
  har_garch_es_001_h10 <- all_forecasts[[stock_name]]$realised_har_garch_horizon_10$ES_0.01[10:length(all_forecasts[[stock_name]]$realised_har_garch_horizon_10$ES_0.01)]
  
  # Perform ESR backtests
  backtest_rg_es_h1 <- esr_backtest(r = actual_returns, q = garch_var_001_h1, e = garch_es_001_h1, alpha = 0.01, version = 1)
  backtest_rg_es_h10 <- esr_backtest(r = actual_returns_h10, q = garch_var_001_h10, e = garch_es_001_h10, alpha = 0.01, version = 1)
  backtest_rhg_es_h1 <- esr_backtest(r = actual_returns, q = har_garch_var_001_h1, e = har_garch_es_001_h1, alpha = 0.01, version = 1)
  backtest_rhg_es_h10 <- esr_backtest(r = actual_returns_h10, q = har_garch_var_001_h10, e = har_garch_es_001_h10, alpha = 0.01, version = 1)
  
  # Extract p-values
  ESR_p_value_rg_h1 <- backtest_rg_es_h1$pvalue_twosided_asymptotic
  ESR_p_value_rg_h10 <- backtest_rg_es_h10$pvalue_twosided_asymptotic
  ESR_p_value_rhg_h1 <- backtest_rhg_es_h1$pvalue_twosided_asymptotic
  ESR_p_value_rhg_h10 <- backtest_rhg_es_h10$pvalue_twosided_asymptotic
  
  # Store results in the data frame
  esr_p_values_df <- rbind(esr_p_values_df, data.frame(
    Stock = stock_name,
    GARCH_Horizon_1 = ESR_p_value_rg_h1,
    GARCH_Horizon_10 = ESR_p_value_rg_h10,
    HAR_GARCH_Horizon_1 = ESR_p_value_rhg_h1,
    HAR_GARCH_Horizon_10 = ESR_p_value_rhg_h10
  ))
  
  # Print results for each stock and horizon
  print(paste("ESR p-values for:", stock_name))
  print(paste("GARCH Horizon 1:", ESR_p_value_rg_h1))
  print(paste("GARCH Horizon 10:", ESR_p_value_rg_h10))
  print(paste("HAR-GARCH Horizon 1:", ESR_p_value_rhg_h1))
  print(paste("HAR-GARCH Horizon 10:", ESR_p_value_rhg_h10))
}

# Save the ESR p-values to a CSV file
write.csv(esr_p_values_df, "esr_p_values_results.csv", row.names = FALSE)





results_df










# Compute FZ Loss
FZ_apple_rg_h1 <- FZLoss(data = apple_results , VaR = apple_rg_var_001_h1, ES = apple_rg_es_001_h1, alpha = 0.01)
FZ_apple_rg_h10 <- FZLoss(data = apple_results_h10 , VaR = apple_rg_var_001_h10, ES = apple_rg_es_001_h10, alpha = 0.01)
FZ_apple_rhg_h1 <- FZLoss(data = apple_results , VaR = apple_var_rhg_001_h1, ES = apple_var_es_001_h1, alpha = 0.01)
FZ_apple_rhg_h10 <- FZLoss(data = apple_results_h10 , VaR = apple_var_rhg_001_h10, ES = apple_var_es_001_h10, alpha = 0.01)

print(FZ_apple_rg_h1)
print(FZ_apple_rg_h10)
print(FZ_apple_rhg_h1)
print(FZ_apple_rhg_h10)



print(FZ_apple_rg_h1)compute_all_es_metrics <- function(returns, VaR, ES, alpha, horizon = 1) {
  tryCatch({
    # Adjust returns for the forecast horizon
    if (horizon > 1) {
      adjusted_returns <- calculate_rolling_sum(returns, horizon)
      adjusted_returns <- head(adjusted_returns, length(VaR)) # Ensure same length
    } else {
      adjusted_returns <- returns
    }
    
    # Compute V_ES_1
    delta_t <- adjusted_returns - -ES
    K_alpha <- which(adjusted_returns < -VaR)
    T1 <- length(K_alpha)
    if (T1 > 0) {
      V_ES_1 <- sum(delta_t[K_alpha]) / T1
    } else {
      V_ES_1 <- 0
    }
    
    # Compute V_ES_2
    D_t <- adjusted_returns - -ES
    D_p <- quantile(D_t, alpha)
    tau_alpha <- which(D_t < D_p)
    T2 <- length(tau_alpha)
    if (T2 > 0) {
      V_ES_2 <- sum(D_t[tau_alpha]) / T2
    } else {
      V_ES_2 <- 0
    }
    
    # Combine V_ES_1 and V_ES_2
    V_alpha <- (abs(V_ES_1) + abs(V_ES_2)) / 2
    
    # Compute ESR p-value
    esr_backtest_result <- esr_backtest(r = adjusted_returns, q = -VaR, e = -ES, alpha = alpha / 2, version = 1)
    ESR_p_value <- esr_backtest_result$pvalue_twosided_asymptotic
    
    # Compute FZ Loss
    FZ_loss <- compute_fz_loss(adjusted_returns, VaR, ES, alpha)
    
    # Return all metrics as a list
    return(list(
      V_alpha = V_alpha,
      ESR_p_value = ESR_p_value,
      FZ_loss = FZ_loss
    ))
  }, error = function(e) {
    print(paste("Error in compute_all_es_metrics:", e$message))
    return(list(
      V_alpha = NA,
      ESR_p_value = NA,
      FZ_loss = NA
    ))
  })
}


# Initialize an empty data frame to store results
es_results_df <- data.frame()

# Loop through each stock and each model to calculate metrics
for (stock in list.files(forecast_dir, full.names = TRUE)) {
  stock_name <- basename(stock)
  actual <- all_results[[stock_name]]$test_data$returns
  for (model in c("realised_garch_horizon_", "realised_har_garch_horizon_")) {
    for (horizon in c(1, 10)) {  # Adjust horizons as needed
      model_name <- paste0(model, horizon)
      
      for (alpha_level in c(0.01, 0.05)) {
        es_file <- file.path(stock, paste0(model_name, "_ES_", alpha_level, ".csv"))
        var_file <- file.path(stock, paste0(model_name, "_VaR_", alpha_level, ".csv"))
        if (file.exists(es_file) && file.exists(var_file)) {
          es_data <- read.csv(es_file)
          var_data <- read.csv(var_file)
          es_forecast <- es_data$Value
          var_forecast <- var_data$Value
          
          # Adjust the length of returns and calculate rolling sums for the 10-day horizon
          if (horizon == 10) {
            actual_adjusted <- calculate_rolling_sum(actual, horizon)
            actual_adjusted <- head(actual_adjusted, length(actual_adjusted) - horizon + 1) # Trim the last few days
          } else {
            actual_adjusted <- actual
          }
          
          # Debugging: Print the lengths of the inputs
          print(paste("Stock:", stock_name))
          print(paste("Model:", model_name))
          print(paste("Horizon:", horizon))
          print(paste("Alpha Level:", alpha_level))
          print(paste("Length of actual_adjusted:", length(actual_adjusted)))
          print(paste("Length of es_forecast:", length(es_forecast)))
          print(paste("Length of var_forecast:", length(var_forecast)))
          
          # Check if lengths match before calling the function
          if (length(actual_adjusted) == length(es_forecast) && length(actual_adjusted) == length(var_forecast)) {
            es_metrics <- compute_all_es_metrics(actual_adjusted, var_forecast, es_forecast, alpha_level, horizon)
            es_metrics$Stock <- stock_name
            es_metrics$Model <- ifelse(grepl("realised_garch", model_name), "Realised GARCH", "HAR-GARCH")
            es_metrics$Horizon <- horizon
            es_metrics$Alpha <- alpha_level
            es_results_df <- bind_rows(es_results_df, es_metrics)
          } else {
            print("Lengths do not match. Skipping this combination.")
          }
        }
      }
    }
  }
}


##### OR THIS ONE


# Initialize an empty data frame to store results
es_results_df <- data.frame()

# Loop through each stock and each model to calculate metrics
for (stock in list.files(forecast_dir, full.names = TRUE)) {
  stock_name <- basename(stock)
  actual <- all_results[[stock_name]]$test_data$returns
  for (model in c("realised_garch_horizon_", "realised_har_garch_horizon_")) {
    for (horizon in c(1, 10)) {  # Adjust horizons as needed
      model_name <- paste0(model, horizon)
      
      for (alpha_level in c(0.01, 0.05)) {
        es_file <- file.path(stock, paste0(model_name, "_ES_", alpha_level, ".csv"))
        var_file <- file.path(stock, paste0(model_name, "_VaR_", alpha_level, ".csv"))
        if (file.exists(es_file) && file.exists(var_file)) {
          es_data <- read.csv(es_file)
          var_data <- read.csv(var_file)
          es_forecast <- es_data$Value
          var_forecast <- var_data$Value
          
          # Adjust the length of returns and calculate rolling sums for the 10-day horizon
          if (horizon == 10) {
            actual_adjusted <- calculate_rolling_sum(actual, horizon)
            actual_adjusted <- head(actual_adjusted, length(actual_adjusted))
          } else {
            actual_adjusted <- actual
          }
          
          # Debugging: Print the lengths of the inputs
          print(paste("Stock:", stock_name))
          print(paste("Model:", model_name))
          print(paste("Horizon:", horizon))
          print(paste("Alpha Level:", alpha_level))
          print(paste("Length of actual_adjusted:", length(actual_adjusted)))
          print(paste("Length of es_forecast:", length(es_forecast)))
          print(paste("Length of var_forecast:", length(var_forecast)))
          
          # Check if lengths match before calling the function
          if (length(actual_adjusted) == length(es_forecast) && length(actual_adjusted) == length(var_forecast)) {
            es_metrics <- compute_all_es_metrics(actual_adjusted, var_forecast, es_forecast, alpha_level)
            es_metrics$Stock <- stock_name
            es_metrics$Model <- ifelse(grepl("realised_garch", model_name), "Realised GARCH", "HAR-GARCH")
            es_metrics$Horizon <- horizon
            es_metrics$Alpha <- alpha_level
            es_results_df <- bind_rows(es_results_df, es_metrics)
          } else {
            print("Lengths do not match. Skipping this combination.")
          }
        }
      }
    }
  }
}

# View and save the final ES results table
es_results_df <- es_results_df %>%
  select(Stock, Model, Horizon, Alpha, V_alpha, ESR_p_value, FZ_loss) %>%
  rename(
    `V_alpha` = V_alpha,
    `ESR p-value` = ESR_p_value,
    `FZ Loss` = FZ_loss
  )

