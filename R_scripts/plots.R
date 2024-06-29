###PLOTTING VAR RESULTS####

plot_var_forecasts <- function(combined_data, model, horizon) {
  if (horizon == 1) {
    returns_col <- "returns"
    var_0_05_col <- paste0(model, "_VaR_0.05_1d")
    var_0_01_col <- paste0(model, "_VaR_0.01_1d")
    title <- "VaR: 1-Day Forecasts at 5% and 1% Level with Daily Returns"
  } else if (horizon == 10) {
    returns_col <- "sum_returns"
    var_0_05_col <- paste0(model, "_VaR_0.05_10d")
    var_0_01_col <- paste0(model, "_VaR_0.01_10d")
    title <- "VaR: 10-Day Forecasts at 5% and 1% Level with Sum of Returns"
  } else {
    stop("Invalid horizon. Please use 1 or 10.")
  }
  
  # Check for missing values
  if (all(is.na(combined_data[[returns_col]]))) {
    stop(paste("All values in", returns_col, "are missing."))
  }
  if (all(is.na(combined_data[[var_0_05_col]]))) {
    stop(paste("All values in", var_0_05_col, "are missing."))
  }
  if (all(is.na(combined_data[[var_0_01_col]]))) {
    stop(paste("All values in", var_0_01_col, "are missing."))
  }
  
  train_data_len <- nrow(combined_data %>% filter(TrainTest == 'Train'))
  
  p_var <- ggplot(combined_data, aes(x = Day)) +
    geom_line(aes_string(y = returns_col, color = "'Daily Returns'"), linetype = "solid", na.rm = TRUE, group = 1) +
    geom_line(aes_string(y = var_0_05_col, color = "'VaR 5%'"), na.rm = TRUE, linetype = "dotted", group = 1) +
    geom_line(aes_string(y = var_0_01_col, color = "'VaR 1%'"), na.rm = TRUE, linetype = "dotted", group = 1) +
    geom_vline(xintercept = train_data_len, linetype = "dashed", color = "black") +
    labs(title = title, x = "Day", y = "Value") +
    scale_color_manual(values = c("Daily Returns" = "black", "VaR 5%" = "purple", "VaR 1%" = "orange")) +
    scale_y_continuous(
      limits = c(-1, 1) 
    ) +
    theme_minimal()
  
  print(p_var)
}

plot_var_forecasts(combined_results_all[["AAPL.csv"]], "realised_garch", 1)
plot_var_forecasts(combined_results_all[["AAPL.csv"]], "realised_garch", 10)
