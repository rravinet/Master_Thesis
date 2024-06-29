##This script performs forecasts###
#Standardised Student's t-distribution function
standardised_student_t <- function(nu, gamma) {
  student.t(nu = nu, gamma = gamma, mu = 0, sigma = 1)
}

######################################################################################################
### IN-SAMPLE: 1-day horizon VaR for 5% and 1% Levels ###
######################################################################################################
# Define in-sample VaR calculation function
in_sample_all_models_VaR <- function(model, forecast_horizon, results, alpha_0.05 = 0.05, alpha_0.01 = 0.01, p = 8) {
  # Function to calculate quantiles
  calculate_quantiles <- function(alpha, p) {
    # Generate p quantiles evenly spaced between 0 and alpha, including alpha
    alpha_values <- rev(seq(0, alpha, length.out = p + 1)[-1])
    return(alpha_values)
  }
  
  sigma2 <- results$fitted_models[[model]]$fitted_values$sigma2
  z <- results$fitted_models[[model]]$fitted_values$z
  fit <- fit.ghypuv(data = z, symmetric = FALSE, lambda = -0.5)
  df <- fit@chi
  skew <- fit@gamma[1]
  
  in_sample_var_0.05 <- numeric(length(sigma2))
  in_sample_var_0.01 <- numeric(length(sigma2))
  in_sample_es_0.05 <- numeric(length(sigma2))
  in_sample_es_0.01 <- numeric(length(sigma2))
  all_var_quantiles_0.05 <- matrix(0, nrow = length(sigma2), ncol = p)
  all_var_quantiles_0.01 <- matrix(0, nrow = length(sigma2), ncol = p)
  
  quantiles_0.05 <- calculate_quantiles(alpha_0.05, p)
  quantiles_0.01 <- calculate_quantiles(alpha_0.01, p)
  
  for (t in 1:length(sigma2)) {
    sigma_t <- sqrt(sigma2[t])
    VaR_dist <- standardised_student_t(df, skew)
    
    VaR_quantiles_0.05 <- sapply(quantiles_0.05, function(alpha) qghyp(alpha, VaR_dist) * sigma_t * sqrt(forecast_horizon))
    VaR_quantiles_0.01 <- sapply(quantiles_0.01, function(alpha) qghyp(alpha, VaR_dist) * sigma_t * sqrt(forecast_horizon))
    
    in_sample_var_0.05[t] <- VaR_quantiles_0.05[1]
    in_sample_var_0.01[t] <- VaR_quantiles_0.01[1]
    in_sample_es_0.05[t] <- mean(VaR_quantiles_0.05[1:p])
    in_sample_es_0.01[t] <- mean(VaR_quantiles_0.01[1:p])
    
    all_var_quantiles_0.05[t, ] <- VaR_quantiles_0.05
    all_var_quantiles_0.01[t, ] <- VaR_quantiles_0.01
  }
  return(list(
    VaR_0.05 = in_sample_var_0.05, 
    VaR_0.01 = in_sample_var_0.01, 
    ES_0.05 = in_sample_es_0.05, 
    ES_0.01 = in_sample_es_0.01,
    quantiles_0.05 = quantiles_0.05,
    quantiles_0.01 = quantiles_0.01,
    all_var_quantiles_0.05 = all_var_quantiles_0.05,
    all_var_quantiles_0.01 = all_var_quantiles_0.01
  ))
}

######################################################################################################
### Out of Sample VaR Forecasts Function ###
######################################################################################################
## Realised GARCH
realised_garch_var_forecast <- function(params, model, forecast_horizon, results, train_data, test_data,alpha_0.05 = 0.05, alpha_0.01 = 0.01, p = 8) {
  # Function to calculate quantiles
  calculate_quantiles <- function(alpha, p) {
    # Generate p quantiles evenly spaced between 0 and alpha, including alpha
    alpha_values <- rev(seq(0, alpha, length.out = p + 1)[-1])
    return(alpha_values)
  }
  z <- results$fitted_models[[model]]$fitted_values$z
  ut <- results$fitted_models[[model]]$fitted_values$ut
  log_sigma <- results$fitted_models[[model]]$fitted_values$log_sigma2
  fit <- fit.ghypuv(data = z, symmetric = FALSE, lambda = -0.5)
  df <- fit@chi
  skew <- fit@gamma[1]
  
  omega <- params[1]
  beta <- params[2]
  gamma <- params[3]
  xi <- params[4]
  phi <- params[5]
  tau1 <- params[6]
  tau2 <- params[7]
  sigma_ut <- params[8]
  
  mu_x <- phi * omega + xi * (1 - beta)
  rolling_forecasts <- numeric(nrow(test_data) - forecast_horizon + 1)
  rolling_VaR_0.05 <- numeric(nrow(test_data) - forecast_horizon + 1)
  rolling_VaR_0.01 <- numeric(nrow(test_data) - forecast_horizon + 1)
  rolling_ES_0.05 <- numeric(nrow(test_data) - forecast_horizon + 1)
  rolling_ES_0.01 <- numeric(nrow(test_data) - forecast_horizon + 1)

  quantiles_0.05 <- calculate_quantiles(alpha_0.05, p)
  quantiles_0.01 <- calculate_quantiles(alpha_0.01, p)
  
  for (start in 1:(nrow(test_data) - forecast_horizon + 1)) { 
    print(paste("Starting forecast window:", start, "of", nrow(test_data) - forecast_horizon + 1))
    end <- start + forecast_horizon - 1
    current_train_data <- rbind(train_data, test_data[1:(start-1), ])
    log_sigma_t <- log_sigma[length(log_sigma)]
    log_x_t <- current_train_data$log_x[nrow(current_train_data)]
    z_t_prev <- z[length(z)]
    u_t_prev <- ut[length(ut)]
    epsilon_t_prev <- tau1 * z_t_prev + tau2 * (z_t_prev^2 - 1) + u_t_prev
    
    Wt <- cbind(z, ut)
    
    for (m in 1:forecast_horizon) {
      sim_pairs <- Wt[sample(nrow(Wt), 5000, replace = TRUE), ]
      
      simulated_log_x <- numeric(5000)
      simulated_log_sigma <- numeric(5000)
      
      for (i in 1:5000) {
        z_i <- sim_pairs[i, 1]
        u_i <- sim_pairs[i, 2]
        
        epsilon_t <- tau1 * z_i + tau2 * (z_i^2 - 1) + u_i
        log_x_i <- mu_x + (beta + phi * gamma) * log_x_t + epsilon_t - beta * epsilon_t_prev
        simulated_log_x[i] <- log_x_i
        log_sigma_i <- omega + beta * log_sigma_t + gamma * log_x_i
        simulated_log_sigma[i] <- log_sigma_i
      }
      
      log_sigma_t <- mean(simulated_log_sigma)
      log_x_t <- mean(simulated_log_x)
      epsilon_t_prev <- mean(tau1 * sim_pairs[, 1] + tau2 * (sim_pairs[, 1]^2 - 1) + sim_pairs[, 2])
    }
    
    rolling_forecasts[start + forecast_horizon - 1] <- log_sigma_t
    print(paste("Stored rolling forecast for step:", start + forecast_horizon - 1))
    sigma_t <- sqrt(exp(log_sigma_t))
    VaR_dist <- standardised_student_t(df, skew)
    
    VaR_quantiles_0.05 <- sapply(quantiles_0.05, function(alpha) qghyp(alpha, VaR_dist) * sigma_t)
    VaR_quantiles_0.01 <- sapply(quantiles_0.01, function(alpha) qghyp(alpha, VaR_dist) * sigma_t)
    
    rolling_VaR_0.05[start + forecast_horizon - 1] <- VaR_quantiles_0.05[1]
    rolling_VaR_0.01[start + forecast_horizon - 1] <- VaR_quantiles_0.01[1]
    rolling_ES_0.05[start + forecast_horizon - 1] <- mean(VaR_quantiles_0.05[1:p])
    rolling_ES_0.01[start + forecast_horizon - 1] <- mean(VaR_quantiles_0.01[1:p])
  }
  
  return(list(rolling_forecasts = rolling_forecasts, VaR_0.05 = rolling_VaR_0.05, VaR_0.01 = rolling_VaR_0.01, ES_0.05 = rolling_ES_0.05, ES_0.01 = rolling_ES_0.01))
}

## Realised HAR-GARCH
har_garch_var_forecast <- function(params, model, forecast_horizon, results, train_data, test_data,alpha_0.05 = 0.05, alpha_0.01 = 0.01, p = 8) {
  # Function to calculate quantiles
  calculate_quantiles <- function(alpha, p) {
    alpha_values <- rev(seq(0, alpha, length.out = p + 1)[-1])
    return(alpha_values)
  }
  
  # Extracting fitted values
  z <- results$fitted_models[[model]]$fitted_values$z
  ut <- results$fitted_models[[model]]$fitted_values$ut
  log_sigma <- results$fitted_models[[model]]$fitted_values$log_sigma2
  
  # VaR Setup
  fit <- fit.ghypuv(data = z, symmetric = FALSE, lambda = -0.5)
  df <- fit@chi
  skew <- fit@gamma[1]
  
  # Getting our parameters
  omega <- params[1]
  beta <- params[2]
  gamma_d <- params[3]
  gamma_w <- params[4]
  gamma_m <- params[5]
  xi <- params[6]
  phi <- params[7]
  tau1 <- params[8]
  tau2 <- params[9]
  sigma_ut <- params[10]
  
  # Calculating mu_x
  mu_x <- phi * omega + xi * (1 - beta)
  
  # Initialise a list to store forecasts
  rolling_forecasts <- numeric(nrow(test_data) - forecast_horizon + 1)
  rolling_VaR_0.05 <- numeric(nrow(test_data) - forecast_horizon + 1)
  rolling_VaR_0.01 <- numeric(nrow(test_data) - forecast_horizon + 1)
  rolling_ES_0.05 <- numeric(nrow(test_data) - forecast_horizon + 1)
  rolling_ES_0.01 <- numeric(nrow(test_data) - forecast_horizon + 1)
  # Buffers to store past RM_day values
  rm_history <- as.list(train_data$RM_day)
  
  # Calculate quantiles
  quantiles_0.05 <- calculate_quantiles(alpha_0.05, p)
  quantiles_0.01 <- calculate_quantiles(alpha_0.01, p)
  
  # Perform rolling forecast
  for (start in 1:(nrow(test_data) - forecast_horizon + 1)) { 
    print(paste("Starting forecast window:", start, "of", nrow(test_data) - forecast_horizon + 1))
    end <- start + forecast_horizon - 1
    
    # Use the training data up to the current point
    current_train_data <- rbind(train_data, test_data[1:(start-1), ])
    log_sigma_t <- log_sigma[length(log_sigma)]
    RM_day <- current_train_data$RM_day[nrow(current_train_data)]
    z_t_prev <- z[length(z)]
    u_t_prev <- ut[length(ut)]
    epsilon_t_prev <- tau1 * z_t_prev + tau2 * (z_t_prev^2 - 1) + u_t_prev
    
    # Update RM_week and RM_month
    rm_history <- append(rm_history, list(RM_day))
    # Calculate RM_week
    RM_week <- mean(unlist(rm_history[(length(rm_history)-5):(length(rm_history)-2)]))
    # Calculate RM_month
    RM_month <- mean(unlist(rm_history[(length(rm_history)-22):(length(rm_history)-6)]))
    
    # Combining z and ut so we can simulate the values
    Wt <- cbind(z, ut)
    
    for (m in 1:forecast_horizon) {
      # Drawing 5000 random pairs
      sim_pairs <- Wt[sample(nrow(Wt), 5000, replace = TRUE), ]
      
      simulated_log_x <- numeric(5000)
      simulated_log_sigma <- numeric(5000)
      
      # Getting epsilon and log_x for each simulation pair
      for (i in 1:5000) {
        z_i <- sim_pairs[i, 1]
        u_i <- sim_pairs[i, 2]
        
        # Compute epsilon_t
        epsilon_t <- tau1 * z_i + tau2 * (z_i^2 - 1) + u_i
        
        # Update log_x using the measurement equation
        log_x_i <- mu_x + (beta + phi * gamma_d) * RM_day + (phi * gamma_w * RM_week) + (phi * gamma_m * RM_month) + epsilon_t - beta * epsilon_t_prev
        simulated_log_x[i] <- log_x_i
        
        # Calculate log_sigma
        log_sigma_i <- omega + beta * log_sigma_t + gamma_d * RM_day + gamma_w * RM_week + gamma_m * RM_month
        simulated_log_sigma[i] <- log_sigma_i
      }
      
      # Calculate the mean of the simulated log_sigma values
      log_sigma_t <- mean(simulated_log_sigma)
      RM_day <- mean(simulated_log_x)
      
      # Update RM_week and RM_month dynamically for the next step
      rm_history <- append(rm_history, list(RM_day))
      
      RM_week <- mean(unlist(rm_history[(length(rm_history)-5):(length(rm_history)-2)]))
      RM_month <- mean(unlist(rm_history[(length(rm_history)-22):(length(rm_history)-6)]))
      
      # Update epsilon_t_prev for the next iteration using the mean of the current iteration's values
      epsilon_t_prev <- mean(tau1 * sim_pairs[, 1] + tau2 * (sim_pairs[, 1]^2 - 1) + sim_pairs[, 2])
    }
    
    # Store the forecast
    rolling_forecasts[start + forecast_horizon - 1] <- log_sigma_t
    print(paste("Stored rolling forecast for step:", start + forecast_horizon - 1))
    
    # Calculate VaR using standardised skewed student's t-distribution
    sigma_t <- sqrt(exp(log_sigma_t))
    VaR_dist <- standardised_student_t(df, skew)
    # VaR_dist <- fit
    VaR_quantiles_0.05 <- sapply(quantiles_0.05, function(alpha) qghyp(alpha, VaR_dist) * sigma_t)
    VaR_quantiles_0.01 <- sapply(quantiles_0.01, function(alpha) qghyp(alpha, VaR_dist) * sigma_t)
    # 
    rolling_VaR_0.05[start + forecast_horizon - 1] <- VaR_quantiles_0.05[1]
    rolling_VaR_0.01[start + forecast_horizon - 1] <- VaR_quantiles_0.01[1]
    rolling_ES_0.05[start + forecast_horizon - 1] <- mean(VaR_quantiles_0.05[1:p])
    rolling_ES_0.01[start + forecast_horizon - 1] <- mean(VaR_quantiles_0.01[1:p])
  }
  
  return(list(rolling_forecasts = rolling_forecasts, VaR_0.05 = rolling_VaR_0.05, VaR_0.01 = rolling_VaR_0.01, ES_0.05 = rolling_ES_0.05, ES_0.01 = rolling_ES_0.01))
}


