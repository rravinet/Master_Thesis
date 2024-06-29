####VaR Tests#####
library(stargazer)
library(GAS)
library(readr)
library(dplyr)
library(forecast)

load("/Users/raphaelravinet/Code/BSE/Thesis/KAI_folder/Final_restuls 08.06/results_data.RData")


# save(all_results, compiled_df, results_var, esr_backtest_results, dm_test_results, file = "results_data.RData")
# Save compiled_df to a CSV file
# write.csv(compiled_df, file = "compiled_df.csv", row.names = FALSE)
# write.csv(results_var, file = "results_var.csv", row.names = FALSE)
# write.csv(esr_backtest_results, file = "esr_backtest_results.csv", row.names = FALSE)
load("/Users/raphaelravinet/Code/BSE/Thesis/KAI_folder/Final_restuls 08.06/results_data.RData")

###Loading VAR AND ES FORECASTS####
base_dir <- "/Users/raphaelravinet/Code/BSE/Thesis/Results/Forecast_results_final/Forecast_results_VAR_ES/"
stock_symbols <- c("AAPL", "BA", "IBM", "JPM", "MMM", "SPY")
file_patterns <- c("realised_garch_horizon_1.csv",
                   "realised_garch_horizon_10.csv",
                   "realised_har_garch_horizon_1.csv",
                   "realised_har_garch_horizon_10.csv")
var_names <- c("rg_pred_h1", "rg_pred_h10", "rhg_pred_h1", "rhg_pred_h10")

for (symbol in stock_symbols) {
  for (i in seq_along(file_patterns)) {
    file_path <- paste0(base_dir, symbol, ".csv/", file_patterns[i])
    data <- read.csv(file_path)
  
    var_name <- paste0(var_names[i], "_", tolower(symbol))
    
    assign(var_name, data)
  }
}

##UC TEST###
unconditional_coverage_test <- function(actual, VaR, alpha) {
  # Calculate exceedances
  exceedances <- actual < VaR
  n1 <- sum(exceedances)
  n0 <- length(actual) - n1
  
  vr <- n1 / (n1+n0)
  # failure rate
  phat <- n1 / (n0 + n1)
  
  #test stat
  LR_uc <- -2 * log((alpha^n1 * (1 - alpha)^n0) / (phat^n1 * (1 - phat)^n0))
  
  # p-value from chi-squared distribution
  p_value <- 1 - pchisq(LR_uc, df = 1)
  
  return(list(LR_uc = LR_uc, p_value = p_value, n0 = n0, n1 = n1, phat = phat, vr = vr))
}


# ##REALISED GARCH###
# dq_garch_h1 <- BacktestVaR(all_results$AAPL.csv$test_data$returns, VaR = rg_pred_h1_aapl$VaR_0.01, alpha = 0.01, Lags = 4)
# dq_garch_h10 <- BacktestVaR(all_results$AAPL.csv$test_data$returns[10:length(all_results$AAPL.csv$test_data$returns)], VaR = rg_pred_h10_aapl$VaR_0.01[10:length(rg_pred_h10_aapl$VaR_0.01)], alpha = 0.01, Lags = 4)
# garch_UC_h1 <- unconditional_coverage_test(all_results$AAPL.csv$test_data$returns, rg_pred_h1_aapl$VaR_0.01, alpha = 0.01)
# garch_UC_h10 <- unconditional_coverage_test(all_results$AAPL.csv$test_data$returns[10:length(all_results$AAPL.csv$test_data$returns)], rg_pred_h10_aapl$VaR_0.01[10:length(rg_pred_h10_aapl$VaR_0.01)], alpha = 0.01)
# ##REALISED HAR GARCH###
# har_garch_UC_h1 <- unconditional_coverage_test(all_results$AAPL.csv$test_data$returns, rhg_pred_h1_aapl$VaR_0.01, alpha = 0.01)
# har_garch_UC_h10 <- unconditional_coverage_test(all_results$AAPL.csv$test_data$returns[10:length(all_results$AAPL.csv$test_data$returns)], rhg_pred_h10_aapl$VaR_0.01[10:length(rhg_pred_h10_aapl$VaR_0.01)], alpha = 0.01)
# dq_har_garch_h1 <- BacktestVaR(all_results$AAPL.csv$test_data$returns, VaR = rhg_pred_h1_aapl$VaR_0.01, alpha = 0.01, Lags = 4)
# dq_har_garch_h10 <- BacktestVaR(all_results$AAPL.csv$test_data$returns[10:length(all_results$AAPL.csv$test_data$returns)], VaR = rhg_pred_h10_aapl$VaR_0.01[10:length(rhg_pred_h10_aapl$VaR_0.01)], alpha = 0.01, Lags = 4)
# 
# rg_aapl_esrtest <- esr_backtest(r = all_results$AAPL.csv$test_data$returns, q = rg_pred_h1_aapl$VaR_0.01, e = rg_pred_h1_aapl$ES_0.01, alpha = 0.01, version = 1)
# rg_aapl_esrtest10 <- esr_backtest(r = all_results$AAPL.csv$test_data$returns[10:length(all_results$AAPL.csv$test_data$returns)], q = rg_pred_h10_aapl$VaR_0.01[10:length(rg_pred_h10_aapl$VaR_0.01)], e = rg_pred_h10_aapl$ES_0.01[10:length(rg_pred_h10_aapl$ES_0.01)], alpha = 0.01, version = 1)
# rhg_aapl_esrtest <- esr_backtest(r = all_results$AAPL.csv$test_data$returns, q = rhg_pred_h1_aapl$VaR_0.01, e = rg_pred_h1_aapl$ES_0.01, alpha = 0.01, version = 1)
# rhg_aapl_esrtest10 <- esr_backtest(r = all_results$AAPL.csv$test_data$returns[10:length(all_results$AAPL.csv$test_data$returns)], q = rhg_pred_h10_aapl$VaR_0.01[10:length(rhg_pred_h10_aapl$VaR_0.01)], e = rhg_pred_h10_aapl$ES_0.01[10:length(rhg_pred_h10_aapl$ES_0.01)], alpha = 0.01, version = 1)

results_var <- list()

# Loop through each stock and perform the desired operations
for (stock_name in stock_symbols) {
  cat("\nProcessing stock:", stock_name, "\n")

  rg_pred_h1_var <- get(paste0("rg_pred_h1_", tolower(stock_name)))
  rg_pred_h10_var <- get(paste0("rg_pred_h10_", tolower(stock_name)))
  rhg_pred_h1_var <- get(paste0("rhg_pred_h1_", tolower(stock_name)))
  rhg_pred_h10_var <- get(paste0("rhg_pred_h10_", tolower(stock_name)))
  
  actual_returns <- all_results[[paste0(stock_name, ".csv")]]$test_data$returns
  actual_returns_h10 <- actual_returns[10:length(actual_returns)]
  
  garch_var_001_h1 <- rg_pred_h1_var$VaR_0.01
  garch_var_001_h10 <- rg_pred_h10_var$VaR_0.01[10:length(rg_pred_h10_var$VaR_0.01)]
  
  garch_UC_h1 <- unconditional_coverage_test(actual_returns, garch_var_001_h1, alpha = 0.01)
  garch_UC_h10 <- unconditional_coverage_test(actual_returns_h10, garch_var_001_h10, alpha = 0.01)
  
  
  
  dq_garch_h1 <- BacktestVaR(actual_returns, VaR = garch_var_001_h1, alpha = 0.01, Lags = 4)
  dq_garch_h10 <- BacktestVaR(actual_returns_h10, VaR = garch_var_001_h10, alpha = 0.01, Lags = 4)
  
  # HAR-GARCH Model
  har_garch_var_001_h1 <- rhg_pred_h1_var$VaR_0.01
  har_garch_var_001_h10 <- rhg_pred_h10_var$VaR_0.01[10:length(rhg_pred_h10_var$VaR_0.01)]
  
  har_garch_UC_h1 <- unconditional_coverage_test(actual_returns, har_garch_var_001_h1, alpha = 0.01)
  har_garch_UC_h10 <- unconditional_coverage_test(actual_returns_h10, har_garch_var_001_h10, alpha = 0.01)
  
  
  dq_har_garch_h1 <- BacktestVaR(actual_returns, VaR = har_garch_var_001_h1, alpha = 0.01, Lags = 4)
  dq_har_garch_h10 <- BacktestVaR(actual_returns_h10, VaR = har_garch_var_001_h10, alpha = 0.01, Lags = 4)

  results_var[[stock_name]] <- list(
    GARCH_Horizon_1 = list(
      UC_Test = garch_UC_h1,
      CC_Test = list(p_value = dq_garch_h1$LRcc['Pvalue']),
      DQ_Test = list(p_value = dq_garch_h1$DQ$pvalue)
    ),
    GARCH_Horizon_10 = list(
      UC_Test = garch_UC_h10,
      CC_Test = list(p_value = dq_garch_h10$LRcc['Pvalue']),
      DQ_Test = list(p_value = dq_garch_h10$DQ$pvalue)
    ),
    HAR_GARCH_Horizon_1 = list(
      UC_Test = har_garch_UC_h1,
      CC_Test = list(p_value = dq_har_garch_h1$LRcc['Pvalue']),
      DQ_Test = list(p_value = dq_har_garch_h1$DQ$pvalue)
    ),
    HAR_GARCH_Horizon_10 = list(
      UC_Test = har_garch_UC_h10,
      CC_Test = list(p_value = dq_har_garch_h10$LRcc['Pvalue']),
      DQ_Test = list(p_value = dq_har_garch_h10$DQ$pvalue)
    )
  )
}


# Combine the results into a data frame
results_var_list2 <- list()
for (stock_name in names(results_var)) {
  for (horizon in names(results_var[[stock_name]])) {
    horizon_data <- results_var[[stock_name]][[horizon]]
    results_var_list2[[paste(stock_name, horizon, sep = "_")]] <- data.frame(
      Model = stock_name,
      Horizon = horizon,
      `No. of violations` = horizon_data$UC_Test$n1,
      `Violation rate` = horizon_data$UC_Test$vr *100,
      `UC p-value` = horizon_data$UC_Test$p_value,
      `CC p-value` = horizon_data$CC_Test$p_value,
      `DQ p-value` = horizon_data$DQ_Test$p_value
    )
  }
}
results_var2 <- do.call(rbind, results_var_list2)


###ES TESTS###


esr_backtest_results <- list()
# Loop through each stock and perform the ESR backtest
for (stock_name in stock_symbols) {
  cat("\nProcessing ESR backtest for stock:", stock_name, "\n")
  
  # Create the variable names dynamically
  rg_pred_h1_var <- get(paste0("rg_pred_h1_", tolower(stock_name)))
  rg_pred_h10_var <- get(paste0("rg_pred_h10_", tolower(stock_name)))
  rhg_pred_h1_var <- get(paste0("rhg_pred_h1_", tolower(stock_name)))
  rhg_pred_h10_var <- get(paste0("rhg_pred_h10_", tolower(stock_name)))
  
  # Extract actual returns and forecasted VaR and ES
  actual_returns <- all_results[[paste0(stock_name, ".csv")]]$test_data$returns
  actual_returns_h10 <- actual_returns[10:length(actual_returns)]
  
  # Perform the ESR backtest for GARCH model
  rg_esrtest_h1 <- esr_backtest(r = actual_returns, q = rg_pred_h1_var$VaR_0.01, e = rg_pred_h1_var$ES_0.01, alpha = 0.01, version = 2, B = 5000)
  rg_esrtest_h10 <- esr_backtest(r = actual_returns_h10, q = rg_pred_h10_var$VaR_0.01[10:length(rg_pred_h10_var$VaR_0.01)], e = rg_pred_h10_var$ES_0.01[10:length(rg_pred_h10_var$ES_0.01)], alpha = 0.01, version = 2,B = 5000)
  
  # Perform the ESR backtest for HAR-GARCH model
  rhg_esrtest_h1 <- esr_backtest(r = actual_returns, q = rhg_pred_h1_var$VaR_0.01, e = rhg_pred_h1_var$ES_0.01, alpha = 0.01, version = 2,B = 5000)
  rhg_esrtest_h10 <- esr_backtest(r = actual_returns_h10, q = rhg_pred_h10_var$VaR_0.01[10:length(rhg_pred_h10_var$VaR_0.01)], e = rhg_pred_h10_var$ES_0.01[10:length(rhg_pred_h10_var$ES_0.01)], alpha = 0.01, version = 2,B = 5000)
  
  # Store results in a nested list structure
  esr_backtest_results[[stock_name]] <- list(
    GARCH_Horizon_1 = rg_esrtest_h1,
    GARCH_Horizon_10 = rg_esrtest_h10,
    HAR_GARCH_Horizon_1 = rhg_esrtest_h1,
    HAR_GARCH_Horizon_10 = rhg_esrtest_h10
  )
}

esr_results_df <- do.call(rbind, lapply(stock_symbols, function(stock_name) {
  data.frame(
    Stock = stock_name,
    Horizon = c(1, 1, 10, 10),
    Model = c("RG", "RHG", "RG", "RHG"),
    ESR_Result = c(
      esr_backtest_results[[stock_name]]$RG_H1,
      esr_backtest_results[[stock_name]]$RHG_H1,
      esr_backtest_results[[stock_name]]$RG_H10,
      esr_backtest_results[[stock_name]]$RHG_H10
    )
  )
}))


# Initialize a list to store the dataframe rows
esr_backtest_rows <- list()

# Loop through each stock and extract the results
for (stock_name in names(esr_backtest_results)) {
  results <- esr_backtest_results[[stock_name]]
  
  # Extract results for GARCH Horizon 1
  esr_backtest_rows <- append(esr_backtest_rows, list(
    data.frame(
      Stock = stock_name,
      Horizon = 1,
      Model = "GARCH",
      ESR_Result = results$GARCH_Horizon_1
    )
  ))
  
  # Extract results for GARCH Horizon 10
  esr_backtest_rows <- append(esr_backtest_rows, list(
    data.frame(
      Stock = stock_name,
      Horizon = 10,
      Model = "GARCH",
      ESR_Result = results$GARCH_Horizon_10
    )
  ))
  
  # Extract results for HAR-GARCH Horizon 1
  esr_backtest_rows <- append(esr_backtest_rows, list(
    data.frame(
      Stock = stock_name,
      Horizon = 1,
      Model = "HAR-GARCH",
      ESR_Result = results$HAR_GARCH_Horizon_1
    )
  ))
  
  # Extract results for HAR-GARCH Horizon 10
  esr_backtest_rows <- append(esr_backtest_rows, list(
    data.frame(
      Stock = stock_name,
      Horizon = 10,
      Model = "HAR-GARCH",
      ESR_Result = results$HAR_GARCH_Horizon_10
    )
  ))
}


(1277 + 1278 + 1285) / 3


num_days_rhg_higher <- sum(rhg_pred_h1_ibm$ES_0.01 > rg_pred_h1_ibm$ES_0.01)

1280 / 30 / 12
1277 - num_days_rhg_higher
length(rg_pred_h1_jpm$ES_0.01)

rg_pred_h1_ibm$ES_0.01
rhg_pred_h1_ibm$ES_0.01

# Combine the list of dataframes into a single dataframe
esr_backtest_df <- do.call(rbind, esr_backtest_rows)
stock_symbols <- c("BA", "MMM", "SPY")

library(esback)
esr_backtest_results_5 <- list()
# Loop through each stock and perform the ESR backtest
for (stock_name in stock_symbols) {
  cat("\nProcessing ESR backtest for stock:", stock_name, "\n")
  
  # Create the variable names dynamically
  rg_pred_h1_var <- get(paste0("rg_pred_h1_", tolower(stock_name)))
  rg_pred_h10_var <- get(paste0("rg_pred_h10_", tolower(stock_name)))
  rhg_pred_h1_var <- get(paste0("rhg_pred_h1_", tolower(stock_name)))
  rhg_pred_h10_var <- get(paste0("rhg_pred_h10_", tolower(stock_name)))
  
  # Extract actual returns and forecasted VaR and ES
  actual_returns <- all_results[[paste0(stock_name, ".csv")]]$test_data$returns
  actual_returns_h10 <- actual_returns[10:length(actual_returns)]
  
  # Perform the ESR backtest for GARCH model
  rg_esrtest_h1 <- esr_backtest(r = actual_returns, q = rg_pred_h1_var$VaR_0.05, e = rg_pred_h1_var$ES_0.05, alpha = 0.05, version = 2, B=5000)
  rg_esrtest_h10 <- esr_backtest(r = actual_returns_h10, q = rg_pred_h10_var$VaR_0.05[10:length(rg_pred_h10_var$VaR_0.05)], e = rg_pred_h10_var$ES_0.05[10:length(rg_pred_h10_var$ES_0.05)], alpha = 0.05, version = 2, B=5000)
  
  # Perform the ESR backtest for HAR-GARCH model
  rhg_esrtest_h1 <- esr_backtest(r = actual_returns, q = rhg_pred_h1_var$VaR_0.05, e = rhg_pred_h1_var$ES_0.05, alpha = 0.05, version = 2, B=5000)
  rhg_esrtest_h10 <- esr_backtest(r = actual_returns_h10, q = rhg_pred_h10_var$VaR_0.05[10:length(rhg_pred_h10_var$VaR_0.05)], e = rhg_pred_h10_var$ES_0.05[10:length(rhg_pred_h10_var$ES_0.05)], alpha = 0.05, version = 2, B=5000)
  
  # Store results in a nested list structure
  esr_backtest_results_5[[stock_name]] <- list(
    GARCH_Horizon_1 = rg_esrtest_h1,
    GARCH_Horizon_10 = rg_esrtest_h10,
    HAR_GARCH_Horizon_1 = rhg_esrtest_h1,
    HAR_GARCH_Horizon_10 = rhg_esrtest_h10
  )
}

# Initialize a list to store the dataframe rows
esr_backtest_rows_5 <- list()

# Loop through each stock and extract the results
for (stock_name in names(esr_backtest_results_5)) {
  results <- esr_backtest_results_5[[stock_name]]
  
  # Extract results for GARCH Horizon 1
  esr_backtest_rows_5 <- append(esr_backtest_rows_5, list(
    data.frame(
      Stock = stock_name,
      Horizon = 1,
      Model = "GARCH",
      ESR_Result = results$GARCH_Horizon_1
    )
  ))
  
  # Extract results for GARCH Horizon 10
  esr_backtest_rows_5 <- append(esr_backtest_rows_5, list(
    data.frame(
      Stock = stock_name,
      Horizon = 10,
      Model = "GARCH",
      ESR_Result = results$GARCH_Horizon_10
    )
  ))
  
  # Extract results for HAR-GARCH Horizon 1
  esr_backtest_rows_5 <- append(esr_backtest_rows_5, list(
    data.frame(
      Stock = stock_name,
      Horizon = 1,
      Model = "HAR-GARCH",
      ESR_Result = results$HAR_GARCH_Horizon_1
    )
  ))
  
  # Extract results for HAR-GARCH Horizon 10
  esr_backtest_rows_5 <- append(esr_backtest_rows_5, list(
    data.frame(
      Stock = stock_name,
      Horizon = 10,
      Model = "HAR-GARCH",
      ESR_Result = results$HAR_GARCH_Horizon_10
    )
  ))
}

# Combine the list of dataframes into a single dataframe
esr_backtest_df_5 <- do.call(rbind, esr_backtest_rows_5)

# Print the resulting dataframe
print(esr_backtest_df_5)

esr_backtest_results_5

####DM TESTS FOR VAR AND ES###

dm_test_results <- list()

# Loop through each stock and perform the DM test
for (stock_name in stock_symbols) {
  cat("\nProcessing DM test for stock:", stock_name, "\n")
  
  # Create the variable names dynamically
  rg_pred_h1_var <- get(paste0("rg_pred_h1_", tolower(stock_name)))
  rg_pred_h10_var <- get(paste0("rg_pred_h10_", tolower(stock_name)))
  rhg_pred_h1_var <- get(paste0("rhg_pred_h1_", tolower(stock_name)))
  rhg_pred_h10_var <- get(paste0("rhg_pred_h10_", tolower(stock_name)))
  
  # Extract actual returns and forecasted VaR
  actual_returns <- all_results[[paste0(stock_name, ".csv")]]$test_data$returns
  actual_returns_h10 <- actual_returns[10:length(actual_returns)]
  
  e1_rg_h1 <- (actual_returns - rg_pred_h1_var$VaR_0.01) ^ 2
  e1_rg_h10 <- (actual_returns_h10 - rg_pred_h10_var$VaR_0.01[10:length(rg_pred_h10_var$VaR_0.01)]) ^ 2
  e1_rhg_h1 <- (actual_returns - rhg_pred_h1_var$VaR_0.01) ^ 2
  e1_rhg_h10 <- (actual_returns_h10 - rhg_pred_h10_var$VaR_0.01[10:length(rhg_pred_h10_var$VaR_0.01)]) ^ 2
  
  # Perform the DM tests
  dm_h1 <- dm.test(e1_rg_h1, e1_rhg_h1, alternative = "greater", h = 1, varestimator = "bartlett")
  dm_h10 <- dm.test(e1_rg_h10, e1_rhg_h10, alternative = "greater", h = 10, varestimator = "bartlett")
  
  # Store results in a nested list structure
  dm_test_results[[stock_name]] <- list(
    DM_Horizon_1 = dm_h1,
    DM_Horizon_10 = dm_h10
  )
}
stock_symbols
round(dm_test_results$AAPL$DM_Horizon_1$statistic,3)
round(dm_test_results$AAPL$DM_Horizon_1$p.value,3)
round(dm_test_results$AAPL$DM_Horizon_10$statistic,3)
round(dm_test_results$AAPL$DM_Horizon_10$p.value,3)

round(dm_test_results$BA$DM_Horizon_1$statistic,3)
round(dm_test_results$BA$DM_Horizon_1$p.value,3)
round(dm_test_results$BA$DM_Horizon_10$statistic,3)
round(dm_test_results$BA$DM_Horizon_10$p.value,3)

round(dm_test_results$IBM$DM_Horizon_1$statistic,3)
round(dm_test_results$IBM$DM_Horizon_1$p.value,3)
round(dm_test_results$IBM$DM_Horizon_10$statistic,3)
round(dm_test_results$IBM$DM_Horizon_10$p.value,3)

round(dm_test_results$JPM$DM_Horizon_1$statistic,3)
round(dm_test_results$JPM$DM_Horizon_1$p.value,3)
round(dm_test_results$JPM$DM_Horizon_10$statistic,3)
round(dm_test_results$JPM$DM_Horizon_10$p.value,3)

round(dm_test_results$MMM$DM_Horizon_1$statistic,3)
round(dm_test_results$MMM$DM_Horizon_1$p.value,3)
round(dm_test_results$MMM$DM_Horizon_10$statistic,3)
round(dm_test_results$MMM$DM_Horizon_10$p.value,3)

round(dm_test_results$SPY$DM_Horizon_1$statistic,3)
round(dm_test_results$SPY$DM_Horizon_1$p.value,3)
round(dm_test_results$SPY$DM_Horizon_10$statistic,3)
round(dm_test_results$SPY$DM_Horizon_10$p.value,3)




####JOINT TESTSSS FZ LOSSS## 
library(GAS)
FZ_rg_aapl_h1 <- mean((FZLoss(data = all_results$AAPL.csv$test_data$returns, VaR = rg_pred_h1_aapl$VaR_0.01, ES = rg_pred_h1_aapl$ES_0.01, alpha = 0.01)))
FZ_rg_aapl_h10 <- (FZLoss(data = all_results$AAPL.csv$test_data$returns[10:length(all_results$AAPL.csv$test_data$returns)], VaR = rg_pred_h10_aapl$VaR_0.01[10:length(rg_pred_h10_aapl$VaR_0.01)], ES = rg_pred_h10_aapl$ES_0.01[10:length(rg_pred_h10_aapl$ES_0.01)], alpha = 0.01))^2
FZ_rhg_aapl_h1 <- (FZLoss(data = all_results$AAPL.csv$test_data$returns, VaR = rhg_pred_h1_aapl$VaR_0.01, ES = rhg_pred_h1_aapl$ES_0.01, alpha = 0.01))
FZ_rhg_aapl_h10 <- (FZLoss(data = all_results$AAPL.csv$test_data$returns[10:length(all_results$AAPL.csv$test_data$returns)], VaR = rhg_pred_h10_aapl$VaR_0.01[10:length(rhg_pred_h10_aapl$VaR_0.01)], ES = rhg_pred_h10_aapl$ES_0.01[10:length(rhg_pred_h10_aapl$ES_0.01)], alpha = 0.01))^2



dm_fz_h1 <- dm.test(FZ_rg_aapl_h1, FZ_rhg_aapl_h1, alternative = "g", h = 1,power = 2, varestimator = "bartlett")
dm_fz_h10 <- dm.test(FZ_rg_aapl_h10, FZ_rhg_aapl_h10, alternative = "g", h = 10,power = 2, varestimator = "bartlett")

dm_fz_results <- list()
fz_loss_values <- list()
# Loop through each stock and perform the calculations
for (stock_name in stock_symbols) {
  # Create the variable names dynamically
  rg_pred_h1_var <- get(paste0("rg_pred_h1_", tolower(stock_name)))
  rg_pred_h10_var <- get(paste0("rg_pred_h10_", tolower(stock_name)))
  rhg_pred_h1_var <- get(paste0("rhg_pred_h1_", tolower(stock_name)))
  rhg_pred_h10_var <- get(paste0("rhg_pred_h10_", tolower(stock_name)))
  
  # Extract actual returns
  actual_returns <- all_results[[paste0(stock_name, ".csv")]]$test_data$returns
  actual_returns_h10 <- actual_returns[10:length(actual_returns)]
  
  # Calculate FZ Loss
  FZ_rg_h1 <- FZLoss(data = actual_returns, VaR = rg_pred_h1_var$VaR_0.01, ES = rg_pred_h1_var$ES_0.01, alpha = 0.01)
  FZ_rg_h10 <- FZLoss(data = actual_returns_h10, VaR = rg_pred_h10_var$VaR_0.01[10:length(rg_pred_h10_var$VaR_0.01)], ES = rg_pred_h10_var$ES_0.01[10:length(rg_pred_h10_var$ES_0.01)], alpha = 0.01)
  FZ_rhg_h1 <- FZLoss(data = actual_returns, VaR = rhg_pred_h1_var$VaR_0.01, ES = rhg_pred_h1_var$ES_0.01, alpha = 0.01)
  FZ_rhg_h10 <- FZLoss(data = actual_returns_h10, VaR = rhg_pred_h10_var$VaR_0.01[10:length(rhg_pred_h10_var$VaR_0.01)], ES = rhg_pred_h10_var$ES_0.01[10:length(rhg_pred_h10_var$ES_0.01)], alpha = 0.01)
  

  # Perform DM test
  dm_fz_h1 <- dm.test(FZ_rg_h1, FZ_rhg_h1, alternative = "t", h = 1,power = 2, varestimator = "bartlett")
  dm_fz_h10 <- dm.test(FZ_rg_h10, FZ_rhg_h10, alternative = "t", h = 10,power = 2, varestimator = "bartlett")
  
  # Store results in a list
  dm_fz_results[[stock_name]] <- list(
    Horizon_1 = list(statistic = round(dm_fz_h1$statistic, 3), p.value = round(dm_fz_h1$p.value, 3)),
    Horizon_10 = list(statistic = round(dm_fz_h10$statistic, 3), p.value = round(dm_fz_h10$p.value, 3))
  )
  fz_loss_values[[stock_name]] <- list(
    FZ_rg_h1 = FZ_rg_h1,
    FZ_rg_h10 = FZ_rg_h10,
    FZ_rhg_h1 = FZ_rhg_h1,
    FZ_rhg_h10 = FZ_rhg_h10
  )
}

# Create a data frame for LaTeX table
dm_fz_df <- data.frame(
  Stock = stock_symbols,
  Horizon_1_Statistic = sapply(stock_symbols, function(x) dm_fz_results[[x]]$Horizon_1$statistic),
  Horizon_1_p_value = sapply(stock_symbols, function(x) dm_fz_results[[x]]$Horizon_1$p.value),
  Horizon_10_Statistic = sapply(stock_symbols, function(x) dm_fz_results[[x]]$Horizon_10$statistic),
  Horizon_10_p_value = sapply(stock_symbols, function(x) dm_fz_results[[x]]$Horizon_10$p.value)
)
dm_fz_df
mean(fz_loss_values$AAPL$FZ_rg_h1)
fz_loss_values$AAPL$FZ_rhg_h1
fz_loss_values$AAPL$FZ_rg_h1
fz_loss_values$AAPL$FZ_rg_h1

# Initialize lists to store the results
dm_fz_results2 <- list()
fz_loss_values2 <- list()
fz_loss_means <- list()

# Loop through each stock and perform the calculations
for (stock_name in stock_symbols) {
  # Create the variable names dynamically
  rg_pred_h1_var <- get(paste0("rg_pred_h1_", tolower(stock_name)))
  rg_pred_h10_var <- get(paste0("rg_pred_h10_", tolower(stock_name)))
  rhg_pred_h1_var <- get(paste0("rhg_pred_h1_", tolower(stock_name)))
  rhg_pred_h10_var <- get(paste0("rhg_pred_h10_", tolower(stock_name)))
  
  # Extract actual returns
  actual_returns <- all_results[[paste0(stock_name, ".csv")]]$test_data$returns
  actual_returns_h10 <- actual_returns[10:length(actual_returns)]
  
  # Calculate FZ Loss
  FZ_rg_h1 <- FZLoss(data = actual_returns, VaR = rg_pred_h1_var$VaR_0.01, ES = rg_pred_h1_var$ES_0.01, alpha = 0.01)
  FZ_rg_h10 <- FZLoss(data = actual_returns_h10, VaR = rg_pred_h10_var$VaR_0.01[10:length(rg_pred_h10_var$VaR_0.01)], ES = rg_pred_h10_var$ES_0.01[10:length(rg_pred_h10_var$ES_0.01)], alpha = 0.01)
  FZ_rhg_h1 <- FZLoss(data = actual_returns, VaR = rhg_pred_h1_var$VaR_0.01, ES = rhg_pred_h1_var$ES_0.01, alpha = 0.01)
  FZ_rhg_h10 <- FZLoss(data = actual_returns_h10, VaR = rhg_pred_h10_var$VaR_0.01[10:length(rhg_pred_h10_var$VaR_0.01)], ES = rhg_pred_h10_var$ES_0.01[10:length(rhg_pred_h10_var$ES_0.01)], alpha = 0.01)
  
  # Calculate means of FZ Losses
  mean_FZ_rg_h1 <- mean(FZ_rg_h1)
  mean_FZ_rg_h10 <- mean(FZ_rg_h10)
  mean_FZ_rhg_h1 <- mean(FZ_rhg_h1)
  mean_FZ_rhg_h10 <- mean(FZ_rhg_h10)
  
  # Store mean values in a list
  fz_loss_means[[stock_name]] <- list(
    mean_FZ_rg_h1 = mean_FZ_rg_h1,
    mean_FZ_rg_h10 = mean_FZ_rg_h10,
    mean_FZ_rhg_h1 = mean_FZ_rhg_h1,
    mean_FZ_rhg_h10 = mean_FZ_rhg_h10
  )
  
  # Perform DM test
  dm_fz_h1 <- dm.test(FZ_rg_h1, FZ_rhg_h1, alternative = "t", h = 1, power = 2, varestimator = "bartlett")
  dm_fz_h10 <- dm.test(FZ_rg_h10, FZ_rhg_h10, alternative = "t", h = 10, power = 2, varestimator = "bartlett")
  
  # Store results in a list
  dm_fz_results2[[stock_name]] <- list(
    Horizon_1 = list(statistic = round(dm_fz_h1$statistic, 3), p.value = round(dm_fz_h1$p.value, 3)),
    Horizon_10 = list(statistic = round(dm_fz_h10$statistic, 3), p.value = round(dm_fz_h10$p.value, 3))
  )
  fz_loss_values2[[stock_name]] <- list(
    FZ_rg_h1 = FZ_rg_h1,
    FZ_rg_h10 = FZ_rg_h10,
    FZ_rhg_h1 = FZ_rhg_h1,
    FZ_rhg_h10 = FZ_rhg_h10
  )
}


# Create a data frame for the means of FZ losses
fz_loss_means_df <- data.frame(
  Stock = stock_symbols,
  Mean_FZ_rg_h1 = sapply(stock_symbols, function(x) fz_loss_means[[x]]$mean_FZ_rg_h1),
  Mean_FZ_rg_h10 = sapply(stock_symbols, function(x) fz_loss_means[[x]]$mean_FZ_rg_h10),
  Mean_FZ_rhg_h1 = sapply(stock_symbols, function(x) fz_loss_means[[x]]$mean_FZ_rhg_h1),
  Mean_FZ_rhg_h10 = sapply(stock_symbols, function(x) fz_loss_means[[x]]$mean_FZ_rhg_h10)
)

# Display the data frames
dm_fz_df
fz_loss_means_df
