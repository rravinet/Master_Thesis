har_iv_garch_forecast <- function(params, model, forecast_horizon, results, train_data, test_data) {
  # Function to calculate quantiles
  # calculate_quantiles <- function(alpha, p) {
  #   alpha_values <- rev(seq(0, alpha, length.out = p + 1)[-1])
  #   return(alpha_values)
  # }
  # Extracting fitted values
  z <- results$fitted_models[[model]]$fitted_values$z
  ut <- results$fitted_models[[model]]$fitted_values$ut
  log_sigma <- results$fitted_models[[model]]$fitted_values$log_sigma2
  
  # Fitting skewed t-distribution
  # fit <- fit.ghypuv(data = z, symmetric = FALSE, lambda = -0.5)
  # df <- fit@chi
  # skew <- fit@gamma[1]
  
  # Model parameters
  omega <- params[1]
  beta <- params[2]
  gamma_d <- params[3]
  gamma_w <- params[4]
  gamma_m <- params[5]
  gamma_iv_d <- params[6]
  xi <- params[7]
  phi <- params[8]
  tau1 <- params[9]
  tau2 <- params[10]
  sigma_ut <- params[11]
  
  # Calculating mu_x
  mu_x <- phi * omega + xi * (1 - beta)
  
  # Initialize a list to store forecasts
  rolling_forecasts <- numeric(nrow(test_data) - forecast_horizon + 1)
  # rolling_VaR_0.05 <- numeric(nrow(test_data) - forecast_horizon + 1)
  # rolling_VaR_0.01 <- numeric(nrow(test_data) - forecast_horizon + 1)
  # rolling_ES_0.05 <- numeric(nrow(test_data) - forecast_horizon + 1)
  # rolling_ES_0.01 <- numeric(nrow(test_data) - forecast_horizon + 1)
  
  # Buffers to store past RM_day and IV_day values
  rm_history <- as.list(train_data$RM_day)
  iv_history <- as.list(train_data$IV_day)
  
  # Calculate quantiles
  # quantiles_0.05 <- calculate_quantiles(alpha_0.05, p)
  # quantiles_0.01 <- calculate_quantiles(alpha_0.01, p)
  
  # Perform rolling forecast
  for (start in 1:(nrow(test_data) - forecast_horizon + 1)) {
    end <- start + forecast_horizon - 1
    
    # Use the training data up to the current point
    current_train_data <- rbind(train_data, test_data[1:(start-1), ])
    log_sigma_t <- log_sigma[length(log_sigma)]
    RM_day <- current_train_data$RM_day[nrow(current_train_data)]
    IV_day <- current_train_data$IV_day[nrow(current_train_data)]
    z_t_prev <- z[length(z)]
    u_t_prev <- ut[length(ut)]
    epsilon_t_prev <- tau1 * z_t_prev + tau2 * (z_t_prev^2 - 1) + u_t_prev
    
    # Update RM_week, RM_month, IV_week, and IV_month
    rm_history <- append(rm_history, list(RM_day))
    iv_history <- append(iv_history, list(IV_day))
    
    RM_week <- mean(unlist(rm_history[(length(rm_history)-5):(length(rm_history)-2)]))
    RM_month <- mean(unlist(rm_history[(length(rm_history)-22):(length(rm_history)-6)]))
    
    # Forecast IV using ARMA(1,1) model
    iv_forecast <- forecast_iv_arma(unlist(iv_history), forecast_horizon)
    
    # Combining z and ut to simulate values
    Wt <- cbind(z, ut)
    
    for (m in 1:forecast_horizon) {
      sim_pairs <- Wt[sample(nrow(Wt), 5000, replace = TRUE), ]
      simulated_log_x <- numeric(5000)
      simulated_log_sigma <- numeric(5000)
      
      for (i in 1:5000) {
        z_i <- sim_pairs[i, 1]
        u_i <- sim_pairs[i, 2]
        epsilon_t <- tau1 * z_i + tau2 * (z_i^2 - 1) + u_i
        
        # Update log_x using the measurement equation
        log_x_i <- mu_x + (beta + phi * gamma_d) * RM_day + (phi * gamma_w * RM_week) + (phi * gamma_m * RM_month) +
          (beta + phi * gamma_iv_d) * IV_day + epsilon_t - beta * epsilon_t_prev
        simulated_log_x[i] <- log_x_i
        
        # Get forecasted IV
        iv_i <- iv_forecast[m]
        
        # Calculate log_sigma
        log_sigma_i <- omega + beta * log_sigma_t + gamma_d * RM_day + gamma_w * RM_week + gamma_m * RM_month + 
          gamma_iv_d * IV_day
        simulated_log_sigma[i] <- log_sigma_i
      }
      
      log_sigma_t <- mean(simulated_log_sigma)
      RM_day <- mean(simulated_log_x)
      IV_day <- iv_forecast[m]
      
      rm_history <- append(rm_history, list(RM_day))
      iv_history <- append(iv_history, list(IV_day))
      
      RM_week <- mean(unlist(rm_history[(length(rm_history)-5):(length(rm_history)-2)]))
      RM_month <- mean(unlist(rm_history[(length(rm_history)-22):(length(rm_history)-6)]))
  
      epsilon_t_prev <- mean(tau1 * sim_pairs[, 1] + tau2 * (sim_pairs[, 1]^2 - 1) + sim_pairs[, 2])
    }
    
    rolling_forecasts[start] <- log_sigma_t
    
    # Calculate VaR using standardised skewed student's t-distribution
    sigma_t <- sqrt(exp(log_sigma_t))
    # VaR_dist <- standardised_student_t(df, skew)
    
    # VaR_quantiles_0.05 <- sapply(quantiles_0.05, function(alpha) qghyp(1 - alpha, VaR_dist) * sigma_t * sqrt(forecast_horizon))
    # VaR_quantiles_0.01 <- sapply(quantiles_0.01, function(alpha) qghyp(1 - alpha, VaR_dist) * sigma_t * sqrt(forecast_horizon))
    
    # rolling_VaR_0.05[start] <- VaR_quantiles_0.05[1]
    # rolling_VaR_0.01[start] <- VaR_quantiles_0.01[1]
    # rolling_ES_0.05[start] <- mean(VaR_quantiles_0.05[1:p])
    # rolling_ES_0.01[start] <- mean(VaR_quantiles_0.01[1:p])
  }
  
  return(rolling_forecasts = rolling_forecasts)
}













forecast_iv_arma <- function(iv_history, forecast_horizon) {
  # Fit ARMA(1,1) model
  arma_model <- Arima(iv_history, order = c(1, 0, 1))
  # Forecast future IV values
  iv_forecast <- forecast(arma_model, h = forecast_horizon)
  return(as.numeric(iv_forecast$mean))
}

test_params <- c(0.252, 0.47, 0.325, 0.29, -0.1, 0.3, -0.417, 0.953, -0.085, 0.087, 0.88)
har_iv_for
har_iv_for <- har_iv_garch_forecast(test_params, "realised_har_iv_garch", 1, all_results$AAPL.csv, all_results$AAPL.csv$train_data, all_results$AAPL.csv$test_data)
mse(all_results$AAPL.csv$test_data$log_x_adj,har_iv_for)  
har_iv_for

all_results$AAPL.csv$test_data$IV_day
combined_wmt_1day <- data.frame(
  Day = as.Date(all_results$AAPL.csv$test_data$Day[1:(length(all_results$AAPL.csv$test_data$Day))], format = "%Y-%m-%d"),
  Adjusted_RM = 100 * all_results$AAPL.csv$test_data$RM_adj[1:(length(all_results$AAPL.csv$test_data$Day))],
  # HAR_Garch_Volatilty_Forecast_10day = aapl_vol_forecast_1day_har$forecast_data,
  HAR_Garch_IV_Volatility_Forecast_10day = 100 * exp(har_iv_for)
)
head(har_iv_for,10)

wmt_date_range <- range(combined_wmt_1day$Day, na.rm = TRUE)


# Plot graph
wmt_1day_plot <- 
  ggplot(combined_wmt_1day, aes(x = Day)) +
  geom_line(aes(y = Adjusted_RM, color = "Adjusted RM"), size = 0.5) +
  # geom_line(aes(y = HAR_Garch_Volatilty_Forecast_10day, color = "Realised HAR-GARCH"), size = 0.3, na.rm = TRUE) +
  geom_line(aes(y = HAR_Garch_IV_Volatility_Forecast_10day, color = "Realised HAR-IV-GARCH"), size = 0.4, na.rm = TRUE) +
  labs(x = "AAPL", y = "", color = NULL) +
  scale_color_manual(values = c("Adjusted RM" = "black", "Realised HAR-GARCH" = "blue", "Realised HAR-IV-GARCH" = "red")) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 3, by = 0.25)) +
  scale_x_date(limits = wmt_date_range, date_labels = "%m/%Y", date_breaks = "6 months") +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.ticks.length = unit(0.2, "cm"),  # Add tick marks
    axis.ticks = element_line(color = "black"),  # Color of tick marks
    axis.line = element_blank(),  # Remove axis lines
    panel.border = element_rect(color = "black", fill = NA, size = 0.6)  # Add border with uniform thickness
  )
print(wmt_1day_plot)

















mse(all_results$AAPL.csv$test_data$log_x_adj,har_iv_for)  


# Define the initial parameters
test_params <- c(0.252, 0.47, 0.325, 0.29, -0.1, 0.3, -0.417, 0.953, -0.085, 0.087, 0.88)

# Store the MSE results
mse_results <- numeric(15)

# Loop for 15 iterations
for (i in 1:15) {
  # Adjust beta and gamma_iv_d by 0.05
  test_params[2] <- test_params[2] + 0.05
  test_params[4] <- test_params[4] + 0.05
  
  # Fit the model
  har_iv_for <- har_iv_garch_forecast(test_params, "realised_har_iv_garch", 1, all_results$AAPL.csv, all_results$AAPL.csv$train_data, all_results$AAPL.csv$test_data)
  
  # Calculate the MSE
  mse_results[i] <- mse(all_results$AAPL.csv$test_data$log_x_adj, har_iv_for)
}

# Print the MSE results
print(mse_results)










#####TESTING ###

har_iv_garch_forecast <- function(params, model, forecast_horizon, results, train_data, test_data, alpha_0.05 = 0.05, alpha_0.01 = 0.01, p = 8) {
  # Function to calculate quantiles
  calculate_quantiles <- function(alpha, p) {
    alpha_values <- rev(seq(0, alpha, length.out = p + 1)[-1])
    return(alpha_values)
  }
  # Extracting fitted values
  z <- results$fitted_models[[model]]$fitted_values$z
  ut <- results$fitted_models[[model]]$fitted_values$ut
  log_sigma <- results$fitted_models[[model]]$fitted_values$log_sigma2
  
  # Fitting skewed t-distribution
  # fit <- fit.ghypuv(data = z, symmetric = FALSE, lambda = -0.5)
  # df <- fit@chi
  # skew <- fit@gamma[1]
  # 
  # Model parameters
  omega <- params[1]
  beta <- params[2]
  gamma_d <- params[3]
  gamma_w <- params[4]
  gamma_m <- params[5]
  gamma_iv_d <- params[6]
  xi <- params[7]
  phi <- params[8]
  tau1 <- params[9]
  tau2 <- params[10]
  sigma_ut <- params[11]
  
  # Calculating mu_x
  mu_x <- phi * omega + xi * (1 - beta)
  
  # Initialize a list to store forecasts
  rolling_forecasts <- numeric(nrow(test_data) - forecast_horizon + 1)
  rolling_VaR_0.05 <- numeric(nrow(test_data) - forecast_horizon + 1)
  rolling_VaR_0.01 <- numeric(nrow(test_data) - forecast_horizon + 1)
  rolling_ES_0.05 <- numeric(nrow(test_data) - forecast_horizon + 1)
  rolling_ES_0.01 <- numeric(nrow(test_data) - forecast_horizon + 1)
  
  # Buffers to store past RM_day and IV_day values
  rm_history <- as.list(train_data$RM_day)
  iv_history <- as.list(train_data$IV_day)
  
  # Calculate quantiles
  quantiles_0.05 <- calculate_quantiles(alpha_0.05, p)
  quantiles_0.01 <- calculate_quantiles(alpha_0.01, p)
  
  # Perform rolling forecast
  for (start in 1:(nrow(test_data) - forecast_horizon + 1)) {
    end <- start + forecast_horizon - 1
    
    # Use the training data up to the current point
    current_train_data <- rbind(train_data, test_data[1:(start-1), ])
    log_sigma_t <- log_sigma[length(log_sigma)]
    RM_day <- current_train_data$RM_day[nrow(current_train_data)]
    IV_day <- current_train_data$IV_day[nrow(current_train_data)]
    z_t_prev <- z[length(z)]
    u_t_prev <- ut[length(ut)]
    epsilon_t_prev <- tau1 * z_t_prev + tau2 * (z_t_prev^2 - 1) + u_t_prev
    
    # Update RM_week, RM_month, IV_week, and IV_month
    rm_history <- append(rm_history, list(RM_day))
    iv_history <- append(iv_history, list(IV_day))
    
    RM_week <- mean(unlist(rm_history[(length(rm_history)-5):(length(rm_history)-2)]))
    RM_month <- mean(unlist(rm_history[(length(rm_history)-22):(length(rm_history)-6)]))
    
    # Forecast IV using ARMA(1,1) model
    iv_forecast <- forecast_iv_arma(unlist(iv_history), forecast_horizon)
    
    # Combining z and ut to simulate values
    Wt <- cbind(z, ut)
    
    for (m in 1:forecast_horizon) {
      sim_pairs <- Wt[sample(nrow(Wt), 5000, replace = TRUE), ]
      simulated_log_x <- numeric(5000)
      simulated_log_sigma <- numeric(5000)
      
      for (i in 1:5000) {
        z_i <- sim_pairs[i, 1]
        u_i <- sim_pairs[i, 2]
        epsilon_t <- tau1 * z_i + tau2 * (z_i^2 - 1) + u_i
        
        # Update log_x using the measurement equation
        log_x_i <- mu_x + (beta + phi * gamma_d) * RM_day + (phi * gamma_w * RM_week) + (phi * gamma_m * RM_month) +
          (beta + phi * gamma_iv_d) * IV_day + epsilon_t - beta * epsilon_t_prev
        simulated_log_x[i] <- log_x_i
        
        # Get forecasted IV
        iv_i <- iv_forecast[m]
        
        # Calculate log_sigma
        log_sigma_i <- omega + beta * log_sigma_t + gamma_d * RM_day + gamma_w * RM_week + gamma_m * RM_month + 
          gamma_iv_d * IV_day
        simulated_log_sigma[i] <- log_sigma_i
      }
      
      log_sigma_t <- mean(simulated_log_sigma)
      RM_day <- mean(simulated_log_x)
      IV_day <- iv_forecast[m]
      
      rm_history <- append(rm_history, list(RM_day))
      iv_history <- append(iv_history, list(IV_day))
      
      RM_week <- mean(unlist(rm_history[(length(rm_history)-5):(length(rm_history)-2)]))
      RM_month <- mean(unlist(rm_history[(length(rm_history)-22):(length(rm_history)-6)]))
      
      epsilon_t_prev <- mean(tau1 * sim_pairs[, 1] + tau2 * (sim_pairs[, 1]^2 - 1) + sim_pairs[, 2])
    }
    
    rolling_forecasts[start + forecast_horizon - 1] <- log_sigma_t
    
    # Calculate VaR using standardised skewed student's t-distribution
    sigma_t <- sqrt(exp(log_sigma_t))
    # VaR_dist <- standardised_student_t(df, skew)
    
    fit <- selm(z ~ 1, family = "SN")
    params <- coef(fit)
    dp <- fit@param$dp
    scale <- dp["omega"]
    shape <- dp["alpha"]
    location <- dp[1]
    
    VaR_quantiles_0.05 <- qsn(quantiles_0.05, xi = location, omega = scale, alpha = shape) * sigma_t
    VaR_quantiles_0.01 <- qsn(quantiles_0.01, xi = location, omega = scale, alpha = shape) * sigma_t
    
    rolling_VaR_0.05[start + forecast_horizon - 1] <- VaR_quantiles_0.05[1]
    rolling_VaR_0.01[start + forecast_horizon - 1] <- VaR_quantiles_0.01[1]
    rolling_ES_0.05[start + forecast_horizon - 1] <- mean(VaR_quantiles_0.05[1:p])
    rolling_ES_0.01[start + forecast_horizon - 1] <- mean(VaR_quantiles_0.01[1:p])
  }
  return(list(rolling_forecasts = rolling_forecasts, VaR_0.05 = rolling_VaR_0.05, VaR_0.01 = rolling_VaR_0.01, ES_0.05 = rolling_ES_0.05, ES_0.01 = rolling_ES_0.01))
}
library(sn)



har_iv_garch_var_es_forecast <- function(params, model, forecast_horizon, results, train_data, test_data, alpha_0.05 = 0.05, alpha_0.01 = 0.01, p = 8) {
  # Function to calculate quantiles
  calculate_quantiles <- function(alpha, p) {
    alpha_values <- rev(seq(0, alpha, length.out = p + 1)[-1])
    return(alpha_values)
  }
  # Extracting fitted values
  z <- results$fitted_models[[model]]$fitted_values$z
  ut <- results$fitted_models[[model]]$fitted_values$ut
  log_sigma <- results$fitted_models[[model]]$fitted_values$log_sigma2
  
  # Fitting skewed t-distribution
  # fit <- fit.ghypuv(data = z, symmetric = FALSE, lambda = -0.5)
  # df <- fit@chi
  # skew <- fit@gamma[1]
  # 
  # Model parameters
  omega <- params[1]
  beta <- params[2]
  gamma_d <- params[3]
  gamma_w <- params[4]
  gamma_m <- params[5]
  gamma_iv_d <- params[6]
  xi <- params[7]
  phi <- params[8]
  tau1 <- params[9]
  tau2 <- params[10]
  sigma_ut <- params[11]
  
  # Calculating mu_x
  mu_x <- phi * omega + xi * (1 - beta)
  
  # Initialize a list to store forecasts
  rolling_forecasts <- numeric(nrow(test_data) - forecast_horizon + 1)
  rolling_VaR_0.05 <- numeric(nrow(test_data) - forecast_horizon + 1)
  rolling_VaR_0.01 <- numeric(nrow(test_data) - forecast_horizon + 1)
  rolling_ES_0.05 <- numeric(nrow(test_data) - forecast_horizon + 1)
  rolling_ES_0.01 <- numeric(nrow(test_data) - forecast_horizon + 1)
  
  # Buffers to store past RM_day and IV_day values
  rm_history <- as.list(train_data$RM_day)
  iv_history <- as.list(train_data$IV_day)
  
  # Calculate quantiles
  quantiles_0.05 <- calculate_quantiles(alpha_0.05, p)
  quantiles_0.01 <- calculate_quantiles(alpha_0.01, p)
  
  # Perform rolling forecast
  for (start in 1:(nrow(test_data) - forecast_horizon + 1)) {
    end <- start + forecast_horizon - 1
    
    # Use the training data up to the current point
    current_train_data <- rbind(train_data, test_data[1:(start-1), ])
    log_sigma_t <- log_sigma[length(log_sigma)]
    RM_day <- current_train_data$RM_day[nrow(current_train_data)]
    IV_day <- current_train_data$IV_day[nrow(current_train_data)]
    z_t_prev <- z[length(z)]
    u_t_prev <- ut[length(ut)]
    epsilon_t_prev <- tau1 * z_t_prev + tau2 * (z_t_prev^2 - 1) + u_t_prev
    
    # Update RM_week, RM_month, IV_week, and IV_month
    rm_history <- append(rm_history, list(RM_day))
    iv_history <- append(iv_history, list(IV_day))
    
    RM_week <- mean(unlist(rm_history[(length(rm_history)-5):(length(rm_history)-2)]))
    RM_month <- mean(unlist(rm_history[(length(rm_history)-22):(length(rm_history)-6)]))
    
    # Forecast IV using ARMA(1,1) model
    iv_forecast <- forecast_iv_arma(unlist(iv_history), forecast_horizon)
    
    # Combining z and ut to simulate values
    Wt <- cbind(z, ut)
    
    for (m in 1:forecast_horizon) {
      sim_pairs <- Wt[sample(nrow(Wt), 5000, replace = TRUE), ]
      simulated_log_x <- numeric(5000)
      simulated_log_sigma <- numeric(5000)
      
      for (i in 1:5000) {
        z_i <- sim_pairs[i, 1]
        u_i <- sim_pairs[i, 2]
        epsilon_t <- tau1 * z_i + tau2 * (z_i^2 - 1) + u_i
        
        # Update log_x using the measurement equation
        log_x_i <- mu_x + (beta + phi * gamma_d) * RM_day + (phi * gamma_w * RM_week) + (phi * gamma_m * RM_month) +
          (beta + phi * gamma_iv_d) * IV_day + epsilon_t - beta * epsilon_t_prev
        simulated_log_x[i] <- log_x_i
        
        # Get forecasted IV
        iv_i <- iv_forecast[m]
        
        # Calculate log_sigma
        log_sigma_i <- omega + beta * log_sigma_t + gamma_d * RM_day + gamma_w * RM_week + gamma_m * RM_month + 
          gamma_iv_d * IV_day
        simulated_log_sigma[i] <- log_sigma_i
      }
      
      log_sigma_t <- mean(simulated_log_sigma)
      RM_day <- mean(simulated_log_x)
      IV_day <- iv_forecast[m]
      
      rm_history <- append(rm_history, list(RM_day))
      iv_history <- append(iv_history, list(IV_day))
      
      RM_week <- mean(unlist(rm_history[(length(rm_history)-5):(length(rm_history)-2)]))
      RM_month <- mean(unlist(rm_history[(length(rm_history)-22):(length(rm_history)-6)]))
      
      epsilon_t_prev <- mean(tau1 * sim_pairs[, 1] + tau2 * (sim_pairs[, 1]^2 - 1) + sim_pairs[, 2])
    }
    
    rolling_forecasts[start + forecast_horizon - 1] <- log_sigma_t
    
    # Calculate VaR using standardised skewed student's t-distribution
    sigma_t <- sqrt(exp(log_sigma_t))
    # VaR_dist <- standardised_student_t(df, skew)
    
    fit <- selm(z ~ 1, family = "SN")
    params <- coef(fit)
    dp <- fit@param$dp
    scale <- dp["omega"]
    shape <- dp["alpha"]
    location <- dp[1]
    
    VaR_quantiles_0.05 <- qsn(quantiles_0.05, xi = location, omega = scale, alpha = shape) * sigma_t
    VaR_quantiles_0.01 <- qsn(quantiles_0.01, xi = location, omega = scale, alpha = shape) * sigma_t
    
    rolling_VaR_0.05[start + forecast_horizon - 1] <- VaR_quantiles_0.05[1]
    rolling_VaR_0.01[start + forecast_horizon - 1] <- VaR_quantiles_0.01[1]
    rolling_ES_0.05[start + forecast_horizon - 1] <- mean(VaR_quantiles_0.05[1:p])
    rolling_ES_0.01[start + forecast_horizon - 1] <- mean(VaR_quantiles_0.01[1:p])
  }
  return(list(rolling_forecasts = rolling_forecasts, VaR_0.05 = rolling_VaR_0.05, VaR_0.01 = rolling_VaR_0.01, ES_0.05 = rolling_ES_0.05, ES_0.01 = rolling_ES_0.01))
}

mse(all_results$AAPL.csv$test_data$log_x_adj,har_iv_for)
