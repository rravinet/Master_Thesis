###Diebold-Mariano Test###

library(readr)
library(dplyr)
library(forecast)
library(tidyverse)


##  varestimator = "bartlett" is what they used in the paper. Here I played around with the alternative being greater or two sided
##For alternative="greater", the alternative hypothesis is that method 2 is more accurate than method 1. For alternative="two.sided", 
##the alternative hypothesis is that method 1 and method 2 have different levels of accuracy.

all_results$AAPL.csv$test_data$log_x_adj
rg_pred_h1_aapl$rolling_forecasts
sigma_pred <- exp(rg_pred_h1_aapl$rolling_forecasts)
sigma_proxy <- exp(all_results$AAPL.csv$test_data$log_x_adj)
a <- (sigma_proxy / sigma_pred)
b <- log(a)
c <- a - b - 1
d <- c/length(sigma_pred)
d
sum((a - b - 1) / length(sigma_pred) )

perform_dm_test <- function(errors1, errors2, h = 1) {
  # Perform the DM test
  dm_test_result <- dm.test(errors1, errors2, alternative = "greater", h = h, varestimator = "bartlett" ) 
  return(dm_test_result)
}

###Manual implementation###
calculate_loss_differential <- function(actual, forecast1, forecast2, crit = "MSE", lambda = 2) {
  if (crit == "MSE") {
    loss1 <- (actual - forecast1) ^ 2
    loss2 <- (actual - forecast2) ^ 2
  } else if (crit == "MAD") {
    loss1 <- abs(actual - forecast1)
    loss2 <- abs(actual - forecast2)
  } else if (crit == "ASYM") {
    loss1 <- asymmetric_loss(actual - forecast1, lambda)
    loss2 <- asymmetric_loss(actual - forecast2, lambda)
  } else {
    stop("Criterion must be either 'MSE', 'MAD', or 'ASYM'")
  }
  d <- loss1 - loss2
  return(d)
}


bartlett_kernel <- function(j, H) {
  return(1 - (j / H))
}


asymmetric_loss <- function(e, lambda) {
  return(exp(lambda * e) - 1 - lambda * e)
}

long_run_variance <- function(d, H) {
  T <- length(d)
  gamma_0 <- var(d)
  gamma_sum <- 0
  for (j in 1:H) {
    weight <- bartlett_kernel(j,H)
    cov <- cov(d[-((T-j+1):T)], d[-(1:j)])
    gamma_sum <- gamma_sum + weight * cov
  }
  LRV <- gamma_0 + 2 * gamma_sum
  return(LRV)
}


dm_statistic <- function(mean_d, LRV, T) {
  S <- mean_d / sqrt(LRV / T)
  return(S)
}

dm_test_manual <- function(actual, forecast1, forecast2, h = 1, crit = "MSE") {
  d <- calculate_loss_differential(actual, forecast1, forecast2, crit)
  mean_d <- mean(d)
  T <- length(d)
  H <- floor(4 * (T / 100) ^ (2/9))
  LRV <- long_run_variance(d, H)
  S <- dm_statistic(mean_d, LRV, T)
  p_value <- 2 * (1 - pnorm(abs(S)))
  return(list(DM = S, p_value = p_value))
}


FZ_rg_aapl_h1 <- FZLoss(data = all_results$AAPL.csv$test_data$log_ret, VaR = rg_pred_h1_aapl$VaR_0.01, ES = rg_pred_h1_aapl$ES_0.01, alpha = 0.01)
FZ_rg_aapl_h10 <- (FZLoss(data = all_results$AAPL.csv$test_data$returns[10:length(all_results$AAPL.csv$test_data$returns)], VaR = rg_pred_h10_aapl$VaR_0.01[10:length(rg_pred_h10_aapl$VaR_0.01)], ES = rg_pred_h10_aapl$ES_0.01[10:length(rg_pred_h10_aapl$ES_0.01)], alpha = 0.01))^2
FZ_rhg_aapl_h1 <- (FZLoss(data = all_results$AAPL.csv$test_data$log_ret, VaR = rhg_pred_h1_aapl$VaR_0.01, ES = rhg_pred_h1_aapl$ES_0.01, alpha = 0.01))^2
FZ_rhg_aapl_h10 <- (FZLoss(data = all_results$AAPL.csv$test_data$returns[10:length(all_results$AAPL.csv$test_data$returns)], VaR = rhg_pred_h10_aapl$VaR_0.01[10:length(rhg_pred_h10_aapl$VaR_0.01)], ES = rhg_pred_h10_aapl$ES_0.01[10:length(rhg_pred_h10_aapl$ES_0.01)], alpha = 0.01))^2

dm_fz_h1 <- dm.test(FZ_rg_aapl_h1, FZ_rhg_aapl_h1, alternative = "g", h = 1, varestimator = "bartlett")
dm_fz_h10 <- dm.test(FZ_rg_aapl_h10, FZ_rhg_aapl_h10, alternative = "t", h = 10, varestimator = "bartlett")
dm_fz_h1


dm_test_manual(all_results$AAPL.csv$test_data$returns,rg_pred_h1_aapl$VaR_0.01,rhg_pred_h1_aapl$VaR_0.01)
dm.test(FZ_rg_aapl_h1, FZ_rhg_aapl_h1, h = 1, alternative = "l", varestimator = "bartlett")

FZ_rg_aapl_h1



dm_test_results <- list()

for (stock_name in filtered_stock_names) {
  actual_returns <- all_results[[stock_name]]$test_data$returns
  actual_returns_h10 <- actual_returns[10:length(actual_returns)]
  
  # GARCH Model
  garch_var_001_h1 <- all_forecasts[[stock_name]]$realised_garch_horizon_1$VaR_0.01
  garch_var_001_h10 <- all_forecasts[[stock_name]]$realised_garch_horizon_10$VaR_0.01[10:length(all_forecasts[[stock_name]]$realised_garch_horizon_10$VaR_0.01)]

  
  # HAR-GARCH Model
  har_garch_var_001_h1 <- all_forecasts[[stock_name]]$realised_har_garch_horizon_1$VaR_0.01
  har_garch_var_001_h10 <- all_forecasts[[stock_name]]$realised_har_garch_horizon_10$VaR_0.01[10:length(all_forecasts[[stock_name]]$realised_har_garch_horizon_10$VaR_0.01)]
  
  # Diebold-Mariano Test
  e1_garch <- (actual_returns - garch_var_001_h1) ^ 2
  e10_garch <- (actual_returns_h10 - garch_var_001_h10) ^ 2
  e1_har_garch <- (actual_returns - har_garch_var_001_h1) ^ 2
  e10_har_garch <- (actual_returns_h10 - har_garch_var_001_h10) ^ 2
  
  dm_h1 <- dm.test(e1_garch, e1_har_garch, h = 1, alternative = "two.sided", varestimator = "bartlett")
  dm_h10 <- dm.test(e10_garch, e10_har_garch, h = 10, alternative = "two.sided", varestimator = "bartlett")
  
  dm_test_results[[stock_name]] <- list(
    Horizon_1 = list(
      DM_Test = dm_h1
    ),
    Horizon_10 = list(
      DM_Test = dm_h10
    )
  )
  
  # Print DM Test results for each stock and horizon
  print(paste("DM Test for:", stock_name, "Horizon 1"))
  print(dm_h1)
  
  print(paste("DM Test for:", stock_name, "Horizon 10"))
  print(dm_h10)
}


dm_results_list <- list()

# Convert the results into a data frame
for (stock_name in names(dm_test_results)) {
  for (horizon in names(dm_test_results[[stock_name]])) {
    dm_result <- dm_test_results[[stock_name]][[horizon]]$DM_Test
    dm_results_list[[paste(stock_name, horizon, sep = "_")]] <- data.frame(
      Stock = stock_name,
      Horizon = horizon,
      Statistic = dm_result$statistic,
      P_Value = dm_result$p.value
    )
  }
}

# Combine all data frames into one
dm_results_df <- do.call(rbind, dm_results_list)

# Save to CSV
write.csv(dm_results_df, "dm_test_results_var_es_2s.csv", row.names = FALSE)
dm_results_df

###DM TEST WITH FZ LOSS###

apple_dm_fz_h1 <- dm.test(FZ_apple_rg_h1, FZ_apple_rhg_h1, h = 1, alternative = "less",varestimator = "bartlett" ) 
apple_dm__fz_h10 <- dm.test(FZ_apple_rg_h10, FZ_apple_rhg_h10, h = 10, alternative = "greater",varestimator = "bartlett" ) 



# Define the directories and parameters
forecast_dir <- "/Users/raphaelravinet/Code/BSE/Thesis/Results/Forecast_results_final"
stocks <- c("AAPL", "SPY", "IBM", "BA", "JPM", "MMM")
models <- c("realised_garch_horizon_", "realised_har_garch_horizon_")
horizons <- c(1, 5, 10, 20)
results_dm <- data.frame()

# Function to perform DM test
perform_dm_test <- function(errors1, errors2, h = 1) {
  dm.test(errors1, errors2, h = h,varestimator =  "bartlett", alternative = "g")
}

# Loop through each stock and each model to calculate metrics
for (stock in stocks) {
  actual <- all_results[[paste0(stock, ".csv")]]$test_data$log_x_adj
  
  for (horizon in horizons) {
    model1_name <- paste0(models[1], horizon)
    model2_name <- paste0(models[2], horizon)
    
    forecast_file1 <- file.path(forecast_dir, paste0(stock, ".csv"), paste0(model1_name, ".csv"))
    forecast_file2 <- file.path(forecast_dir, paste0(stock, ".csv"), paste0(model2_name, ".csv"))
    
    if (file.exists(forecast_file1) && file.exists(forecast_file2)) {
      forecast_data1 <- read_csv(forecast_file1, show_col_types = FALSE)
      forecast_data2 <- read_csv(forecast_file2, show_col_types = FALSE)
      
      forecast1 <- forecast_data1$forecast_data
      forecast2 <- forecast_data2$forecast_data
      
      actual_adjusted <- actual[horizon:(length(actual))]
      
      # Calculate the forecast errors
      errors_rg_mse <- (actual_adjusted - forecast1)^2
      errors_rhg_mse <- (actual_adjusted - forecast2)^2
      errors_rg_rmse <- sqrt(errors_rg_mse)
      errors_rhg_rmse <- sqrt(errors_rhg_mse)
      
      # QLIKE calculation
      qlike <- function(proxy, forecast) {
        proxy / forecast - log(proxy / forecast) - 1
      }
      errors_rg_qlike <- qlike(actual_adjusted, forecast1)
      errors_rhg_qlike <- qlike(actual_adjusted, forecast2)
      
      # Perform DM test for MSE
      dm_test_mse <- perform_dm_test(errors_rg_mse, errors_rhg_mse, h = horizon)
      # Perform DM test for RMSE
      dm_test_rmse <- perform_dm_test(errors_rg_rmse, errors_rhg_rmse, h = horizon)
      # Perform DM test for QLIKE
      dm_test_qlike <- perform_dm_test(errors_rg_qlike, errors_rhg_qlike, h = horizon)
      
      # Store the results for MSE
      results_dm <- rbind(results_dm, data.frame(
        Stock = stock,
        Horizon = horizon,
        Model1 = model1_name,
        Model2 = model2_name,
        Metric = "MSE",
        DM_test_statistic = round(dm_test_mse$statistic,3),
        DM_p_value = round(dm_test_mse$p.value,3)
      ))
      
      # Store the results for RMSE
      results_dm <- rbind(results_dm, data.frame(
        Stock = stock,
        Horizon = horizon,
        Model1 = model1_name,
        Model2 = model2_name,
        Metric = "RMSE",
        DM_test_statistic = round(dm_test_rmse$statistic,3),
        DM_p_value = round(dm_test_rmse$p.value,3)
      ))
      
      # Store the results for QLIKE
      results_dm <- rbind(results_dm, data.frame(
        Stock = stock,
        Horizon = horizon,
        Model1 = model1_name,
        Model2 = model2_name,
        Metric = "QLIKE",
        DM_test_statistic = round(dm_test_qlike$statistic,3),
        DM_p_value = round(dm_test_qlike$p.value,3)
      ))
    }
  }
}

print(results_dm)
write.csv(results_dm, "dm_test_final.csv")
