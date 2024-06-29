#### This script performs the Maximum Likelihood estimation for our models###

### Import all the libraries we will need###
library(ggplot2)
library(ghyp)
library(stargazer)
library(segMGarch)
library(dplyr)
library(zoo)
library(xts)
library(rugarch)
library(lubridate)
library(stats)
library(data.table)
library(Metrics)
library(Rsolnp)
library(knitr)
library(maxLik)
library(DescTools)


##FILTER Functions##
##STANDARD GARCH##
standard_garch <- function (params, data) {
  returns <- data$returns
  omega <- params[1]
  alpha <- params[2]
  beta <- params[3]
  T <- length(returns)
  z <- length(returns)
  sigma_2 <- numeric(T)
  
  if ((1-alpha-beta) > 0) {
    sigma_2[1] <- omega / (1 - alpha - beta)
  } else {
    sigma_2[1] <- var(returns)
  }
  z[1] <- returns[1] / sigma_2[1]
  for (t in 2:T){
    sigma_2[t] <- omega + alpha * returns[t-1]^2 + beta * sigma_2[t-1]
    z[t] <- returns[t] / sqrt(sigma_2[t])
  }
  return(list(sigma2 = sigma_2,z = z))
}

###REALISED GARCH###
realised_garch <- function (params, data) {
  returns <- data$returns
  RM <- data$RM
  log_x <- data$log_x
  omega <- params[1]
  beta <- params[2]
  gamma <- params[3]
  xi <- params[4]
  phi <- params[5]
  tau1 <- params[6]
  tau2 <- params[7]
  T <- length(returns)
  
  log_sigma2 <- numeric(T)
  sigma2 <- numeric(T)
  ut <- numeric(T)
  z <- numeric(T)
  tau_z <- numeric(T)
  
  numerator = omega + gamma * xi
  denominator = 1 - (beta + gamma * phi)
  
  
  if (numerator > 0 && denominator > 0) {
    log_sigma2[1] <- log(numerator / denominator)
  } else {
    log_sigma2[1] <- log(var(returns))  
  }
  
  sigma2[1] <- exp(log_sigma2[1])
  z[1] <- returns[1] / sqrt(exp(log_sigma2[1]))
  
  for (t in 2:T){
    #we are using log here because that's how the model is specified in most of the papers
    log_sigma2[t] <- omega + beta * log_sigma2[t-1] + gamma * log_x[t-1]
    sigma2[t] <- exp(log_sigma2[t])
    z[t] <- returns[t] / sqrt(exp(log_sigma2[t]))
    
    tau_z[t] <- tau1 *z[t] + tau2 * (z[t] ^ 2 - 1) #our leverage function
    ut[t]<- log_x[t] - xi - phi * log_sigma2[t] - tau_z[t] # rearranging measurement equation to calculate the residuals
  }
  
  return(list(log_sigma2 = log_sigma2, sigma2 = sigma2, ut = ut, z = z))
}

##REALISED_HAR_GARCH##
realised_har_garch <- function (params, data) {
  #Starting our parameters
  returns <- data$returns
  RM_day <- data$RM_day
  RM_week <- data$RM_week
  RM_month <- data$RM_month
  log_x <- data$log_x
  
  omega <- params[1]
  beta <- params[2]
  gamma_d <- params[3]
  gamma_w <- params[4]
  gamma_m <- params[5]
  xi <- params[6]
  phi <- params[7]
  tau1 <- params[8]
  tau2 <- params[9]
  
  T <- length(returns)
  
  log_sigma2 <- numeric(T)
  sigma2 <- numeric(T)
  ut <- numeric(T)
  z <- numeric(T)
  tau_z <- numeric(T)
  
  numerator = omega + (gamma_d + gamma_w + gamma_m) * xi
  denominator = 1 - (beta + (gamma_d + gamma_w + gamma_m) * phi)
  
  # We were getting NaN if we started with log_sigma2 unconditional variance, so we're trying to control for that
  if (numerator > 0 && denominator > 0) {
    log_sigma2[1] <- log(numerator / denominator)
  } else {
    log_sigma2[1] <- log(var(returns))  # Fallback to variance of returns
  }
  
  sigma2[1] <- exp(log_sigma2[1])
  z[1] <- returns[1] / sqrt(exp(log_sigma2[1]))
  #mu_x <- phi * omega + xi * (1 - beta)
  
  for (t in 2:T){
    #we are using log here because that's how the model is specified in most of the papers
    log_sigma2[t] <- omega + beta * log_sigma2[t-1] + gamma_d * RM_day[t] + gamma_w * RM_week[t] + gamma_m * RM_month[t]
    sigma2[t] <- exp(log_sigma2[t])
    z[t] <- returns[t] / sqrt(exp(log_sigma2[t]))
    
    tau_z[t] <- tau1 * z[t] + tau2 * (z[t] ^ 2 - 1) #leverage function
    ut[t]<- log_x[t] - xi - phi * log_sigma2[t] - tau_z[t]
    
  }
  
  return(list(log_sigma2 = log_sigma2, sigma2 = sigma2, ut = ut, z = z))
}
##REALISED_HAR_IV_GARCH##
realised_har_iv_garch <- function (params, data) {
  returns <- data$returns
  IV_day <- data$IV_day
  IV_week <- data$IV_week
  IV_month <- data$IV_month
  RM_day <- data$RM_day
  RM_week <- data$RM_week
  RM_month <- data$RM_month
  log_x <- data$log_x

  omega <- params[1]
  beta <- params[2]
  gamma_d <- params[3]
  gamma_w <- params[4]
  gamma_m <- params[5]
  gamma_iv_d <- params[6]
  gamma_iv_w <- params[7]
  gamma_iv_m <- params[8]
  xi <- params[9]
  phi <- params[10]
  tau1 <- params[11]
  tau2 <- params[12]
  
  T <- length(returns)
  
  log_sigma2 <- numeric(T)
  sigma2 <- numeric(T)
  ut <- numeric(T)
  z <- numeric(T)
  tau_z <- numeric(T)
  
  numerator = omega + (gamma_d + gamma_w + gamma_m + gamma_iv_d + gamma_iv_w + gamma_iv_m) * xi
  denominator = 1 - (beta + (gamma_d + gamma_w + gamma_m + gamma_iv_d + gamma_iv_w + gamma_iv_m) * phi)

  # We were getting NaN if we started with log_sigma2 unconditional variance, so we're trying to control for that
  if (numerator > 0 && denominator > 0) {
    log_sigma2[1] <- log(numerator / denominator)
  } else {
    log_sigma2[1] <- log(var(returns))  # Fallback to variance of returns
  }
  
  sigma2[1] <- exp(log_sigma2[1])
  z[1] <- returns[1] / sqrt(exp(log_sigma2[1]))
  
  for (t in 2:T){
    #we are using log here because that's how the model is specified in most of the papers
    log_sigma2[t] <- omega + beta * log_sigma2[t-1] + gamma_d * RM_day[t] + gamma_w * RM_week[t] + gamma_m * RM_month[t]  + gamma_iv_d * IV_day[t] + gamma_iv_w * IV_week[t] + gamma_iv_m * IV_month[t]
    sigma2[t] <- exp(log_sigma2[t])
    z[t] <- returns[t] / sqrt(exp(log_sigma2[t]))
    
    tau_z[t] <- tau1 *z[t] + tau2 * (z[t] ^ 2 - 1) #our leverage function
    ut[t]<- exp(log_x[t]) - xi - phi * log_sigma2[t] - tau_z[t]
  }
  
  return(list(log_sigma2 = log_sigma2, sigma2 = sigma2, ut = ut, z = z))
}

###OBJECTIVE FUNCTIONS###
##STANDARD GARCH##
standard_garch_obj <- function(params, data){
  returns <- data$returns
  omega <- params[1]
  alpha <- params[2]
  beta <- params[3]
  T <- length(returns)
  sigma_2 <- standard_garch(params,data)$sigma2
  

  if (all(is.finite(params)) && omega > 0 && alpha > 0 && beta > 0 & alpha + beta < 1) {
    log_lik <- - T/2* log(2*pi) - 1/2 * sum(log(sigma_2)) - 1/2 * sum(returns^2 / sigma_2)
    log_lik <- log_lik
  } else {
    log_lik <- -1e15
    }
}

##REALISED GARCH##
realised_garch_obj <- function(params,data){
  returns <- data$returns
  omega <- params[1]
  beta <- params[2]
  gamma <- params[3]
  xi <- params[4]
  phi <- params[5]
  tau1 <- params[6]
  tau2 <- params[7]
  sigma_ut <- params[8] #variance of our ut in the measurement equation
  
  T <- length(returns)
  
  model_results <- realised_garch(params,data)
  sigma2 <- model_results$sigma2
  ut <- model_results$ut
  
  #required conditions for realized garch -  https://core.ac.uk/download/pdf/41239655.pdf - page 6 of this paper
  # 0 < β + φγ < 1 and  ω + γξ > 0
  if (all(is.finite(params)) && omega > 0 && beta > 0 && gamma > 0 && 0 < (beta + phi * gamma) && (beta + phi * gamma) < 1 && (omega + gamma * xi) > 0) {
    #quasi log-likelihood
    log_lik_1 <- - T/2* log(2*pi) - 1/2 * sum(log(sigma2)) - 1/2 * sum(returns^2 / sigma2) #this is the first part l(r)
    log_lik_2 <- - T/2 * log(2 * pi) - 1/2 * sum(log(sigma_ut)) -1/2 * sum((ut ^ 2 / sigma_ut)) #this is the second part l(x|r)
    log_lik <- log_lik_1 + log_lik_2
  } else {
    log_lik <- -1e15
  }
  return(log_lik)
}


##REALISED_HAR_GARCH
realised_har_garch_obj <- function(params, data){
  returns <- data$returns
  omega <- params[1]
  beta <- params[2]
  gamma_d <- params[3]
  gamma_w <- params[4]
  gamma_m <- params[5]
  xi <- params[6]
  phi <- params[7]
  tau1 <- params[8]
  tau2 <- params[9]
  sigma_ut <- params[10] #variance of our ut in the measurement equation
  
  T <- length(returns)
  
  model_results <- realised_har_garch(params, data)
  sigma2 <- model_results$sigma2
  ut <- model_results$ut
  
  #Constraints
  if (all(is.finite(params)) && 
      omega > 0 && 
      beta > 0 && 
      gamma_d > 0 && 
      gamma_w > 0 && 
      gamma_m > 0 && 
      sigma_ut > 0 && 
      beta + (gamma_d + gamma_w + gamma_m) * phi < 1 && 
      (omega + (gamma_d + gamma_w + gamma_m) * xi) > 0 ) {
    
    # quasi log-likelihood
    log_lik_1 <- - T/2 * log(2*pi) - 1/2 * sum(log(sigma2)) - 1/2 * sum(returns^2 / sigma2) # this is the first part l(r)
    log_lik_2 <- - T/2 * log(2 * pi) - 1/2 * sum(log(sigma_ut)) - 1/2 * sum((ut^2 / sigma_ut)) # this is the second part l(x|r)
    log_lik <- (log_lik_1 + log_lik_2)
  } else {
    log_lik <- -1e15
  }
  return(log_lik)
}
##REALISED_HAR_IV_GARCH
realised_har_iv_garch_obj <- function(params, data){
  returns <- data$returns
  omega <- params[1]
  beta <- params[2]
  gamma_d <- params[3]
  gamma_w <- params[4]
  gamma_m <- params[5]
  gamma_iv_d <- params[6]
  gamma_iv_w <- params[7]
  gamma_iv_m <- params[8]
  xi <- params[9]
  phi <- params[10]
  tau1 <- params[11]
  tau2 <- params[12]
  sigma_ut <- params[13] #variance of our ut in the measurement equation
  
  T <- length(returns)
  
  model_results <- realised_har_iv_garch(params, data)
  sigma2 <- model_results$sigma2
  ut <- model_results$ut
  
  #Constraints
  if (all(is.finite(params)) && 
      omega > 0 && 
      beta > 0 && 
      gamma_d > 0 && 
      gamma_w > 0 && 
      gamma_m > 0 && 
      gamma_iv_d > 0 &&
      gamma_iv_w > 0 &&
      gamma_iv_m > 0 &&
      sigma_ut > 0 && 
      beta + (gamma_d + gamma_w + gamma_m + gamma_iv_d + gamma_iv_w +gamma_iv_m) * phi < 1 && 
      (omega + (gamma_d + gamma_w + gamma_m + gamma_iv_d + gamma_iv_w + gamma_iv_m) * xi) > 0 ) {
    
    # quasi log-likelihood
    log_lik_1 <- - T/2 * log(2*pi) - 1/2 * sum(log(sigma2)) - 1/2 * sum(returns^2 / sigma2) 
    log_lik_2 <- - T/2 * log(2 * pi) - 1/2 * sum(log(sigma_ut)) - 1/2 * sum((ut^2 / sigma_ut)) 
    log_lik <- (log_lik_1 + log_lik_2)
  } else {
    log_lik <- -1e15
  }
  return(log_lik)
}

##CONSTRAINTS FOR MAXLIK##
# https://github.com/cran/maxLik/blob/master/tests/constraints.R

##STANDARD GARCH##
A_garch <- matrix(c(
  1,  0,  0,   # omega > 0
  0, 1,  0,   # alpha > 0
  0,  0, 1,   # beta > 0
  0,  -1,  -1    # alpha + beta < 1
), 4, 3, byrow=TRUE)
B_garch <- c(0, 0, 0, 1)
ineqCon_sgarch <- list(ineqA=A_garch, ineqB=B_garch)

##REALISED GARCH##
A_rgarch <- matrix(c(
  1,  0,  0,  0,  0,  0,  0,  0,  # omega >= 0
  0,  1,  0,  0,  0,  0,  0,  0,  # beta >= 0
  0,  0,  1,  0,  0,  0,  0,  0,  # gamma >= 0
  0,  -1,  -1,  0,  0,  0,  0,  0,  # beta + gamma < 1
  0,  0,  0,  0,  0,  0,  0,  1  # sigma_ut >= 0
), 5, 8, byrow=TRUE)
B_rgarch <- c(0, 0, 0, 1, 0)
ineqCon_rgarch <- list(ineqA = A_rgarch, ineqB = B_rgarch)

##REALISED HAR GARCH##
A_rhar_garch <- matrix(c(
  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,   # omega >= 0
  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,     # beta >= 0
  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,     # gamma_d >= 0
  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,     # gamma_w >= 0
  0,  0,  0,  0,  1,  0,  0,  0,  0,  0,     # gamma_m >= 0
  0,  0,  0,  0,  0,  0,  1,  0,  0,  0,     # phi >= 0
  0,  0,  -1,  -1,  -1,  0,  0,  0,  0,  0,     # sum of gammas  <1 0
  0,  0,  0,  0,  0,  0,  0,  0,  0,  1     # sigma_ut >= 0
), 8, 10, byrow=TRUE)
B_rhar_garch <- c(0, 0, 0, 0, 0,-0.8, 1, 0)
ineqCon_rhar_garch <- list(ineqA=A_rhar_garch, ineqB=B_rhar_garch)

##REALISED HAR IV GARCH##
A_rhar_iv_garch <- matrix(c(
  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,0,0,0,   # omega >= 0
  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,0,0,0,     # beta >= 0
  0,  -1,  0,  0,  0,  0,  0,  0,  0,  0,0,0,0, # beta < 1
  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,0,0,0,   # gamma_d >= 0
  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,0,0,0,    # gamma_w >= 0
  0,  0,  0,  0,  1,  0,  0,  0,  0,  0,0, 0,0,    # gamma_m >= 0
  0,  0,  -1,  -1,-1,  0,  0, 0,  0,  0,0,0,0,   # gd+gw+gm+giv < 1
  0,  0,  0,  0,  0,  0,  0,  0,0,  0,1,0,0    # sigma_ut >= 0
), 8, 13, byrow=TRUE)
B_rhar_iv_garch <- c(0, 0,1, 0, 0, 0,1, 0)
ineqCon_rhar_iv_garch <- list(ineqA=A_rhar_iv_garch, ineqB=B_rhar_iv_garch)

####RUNNING MODELS####
run_garch_models <- function(data, model_type) {
  
  start_params_sgarch <- c(0.01, 0.5, 0.3)
  start_params_rgarch <- c(0.01, 0.5, 0.4, 0.1, 0.9, 0.01, 0.01, 0.2)
  #pretend_these_dont_exist <- c(0.16, 0.6, 0.394, -0.41, 0.95, -0.085, 0.117, 0.240)
  #start_params_r_har_garch <- c(0.25, 0.4, 0.3, 0.1, 0.02, 0.1, 0.85, 0.01, 0.01, 0.1)
  start_params_r_har_garch <- c(0.252, 0.35, 0.4, 0.15, 0.075, -0.417, 0.9, -0.01, 0.01, 0.342)
  #paper's_params <-          c(0.252, 0.388, 0.425, 0.114, 0.075, -0.417, 0.953, -0.085, 0.087, 0.24)
  start_params_r_har_iv_garch <- c(0.01, 0.3, 0.3,0.1, 0.01,0.15,0.08,0.01, 0.3, 0.7, 0.01, 0.01, 0.1)
  
  if (model_type == "standard_garch") {
    objective_fn <- standard_garch_obj
    start_params <- start_params_sgarch
    ineqCon <- ineqCon_sgarch
  } else if (model_type == "realised_garch") {
    objective_fn <- realised_garch_obj
    start_params <- start_params_rgarch
    ineqCon <- ineqCon_rgarch
  } else if (model_type == "realised_har_garch") {
    objective_fn <- realised_har_garch_obj
    start_params <- start_params_r_har_garch
    ineqCon <- ineqCon_rhar_garch
  } else if (model_type == "realised_har_iv_garch") {
    objective_fn <- realised_har_iv_garch_obj
    start_params <- start_params_r_har_iv_garch
    ineqCon <- ineqCon_rhar_iv_garch
  }else {
    stop("Invalid model type specified.")
  }
  
  result <- maxLik(logLik = objective_fn, start = start_params, constraints = ineqCon, data = data)
  return(result)
}


param_names <- list(
  standard_garch = c("omega", "alpha", "beta"),
  realised_garch = c("omega", "beta", "gamma", "xi", "phi", "tau1", "tau2", "sigma_ut"),
  realised_har_garch = c("omega", "beta", "gamma_d", "gamma_w", "gamma_m", "xi", "phi", "tau1", "tau2", "sigma_ut"),
  realised_har_iv_garch = c("omega", "beta", "gamma_d", "gamma_w", "gamma_m", "gamma_iv_d","gamma_iv_w","gamma_iv_m", "xi", "phi", "tau1", "tau2", "sigma_ut"),
  standard_garch_rugarch = c("omega", "alpha", "beta"),
  realised_garch_rugarch = c("omega", "gamma", "beta","tau1", "tau2", "phi", "sigma_ut", "xi")
)

model_types <- c("standard_garch", "realised_garch", "realised_har_garch", "realised_har_iv_garch")

#USING THIS FUNCTION NOW SO WE DON'T NEED TO GET THE MSE###
fit_models_and_get_parameters <- function(data, model_types, param_names) {
  fitted_models_list <- list()
  results <- list()
  
  for (model_type in model_types[1:4]) {
    model_result <- run_garch_models(data, model_type)
    optimal_params <- coef(model_result)
    
    fitted_function <- switch(model_type,
                              'standard_garch' = standard_garch,
                              'realised_garch' = realised_garch,
                              'realised_har_garch' = realised_har_garch,
                              'realised_har_iv_garch' = realised_har_iv_garch)
    
    fitted_values <- fitted_function(optimal_params, data)
    
    fitted_models_list[[model_type]] <- list(
      model_result = model_result,
      optimal_params = optimal_params,
      fitted_values = fitted_values
    )
    
    results[[model_type]] <- optimal_params
  }
  
  all_param_names <- unique(unlist(param_names))
  param_table <- data.frame(matrix(ncol = length(model_types), nrow = length(all_param_names)))
  colnames(param_table) <- model_types
  rownames(param_table) <- all_param_names
  
  for (model_type in model_types) {
    param_names_for_model <- param_names[[model_type]]
    param_indices <- match(param_names_for_model, rownames(param_table))
    param_table[param_indices, model_type] <- results[[model_type]]
  }
  
  return(list(
    param_table = param_table,
    fitted_models = fitted_models_list
  ))
}
