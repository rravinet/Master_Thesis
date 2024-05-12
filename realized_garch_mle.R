library(tsgarch)
library(zoo)
library(xts)
library(dplyr)
library(lubridate)
library(rugarch)
library(tseries)
library(stats)
library(data.table)
library(Metrics)
library(Rsolnp)
library(knitr)

data("spyreal")

### MLE REALIZED GARCH### 
garch11_realised <- function (params, returns, RM) {
  #Starting our parameters
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
  log_x <- log(RM)
  ut <- numeric(T)
  z <- numeric(T)
  tau_z <- numeric(T)
  
  numerator = omega + gamma * xi
  denominator = 1 - (beta + gamma * phi)
  
  numerator = omega + gamma * xi
  denominator = 1 - (beta + gamma * phi)
  
  # We were getting Nans if we started with log_sigma2 unconditional variance, so we're trying to control for tha
  if (numerator > 0 && denominator > 0) {
    log_sigma2[1] <- log(numerator / denominator)
  } else {
    log_sigma2[1] <- log(var(returns))  # Fallback to variance of returns
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
  return(list(sigma2 = sigma2, ut = ut, z = z))
}

objective_fn <- function(params,returns, RM){
  omega <- params[1]
  beta <- params[2]
  gamma <- params[3]
  xi <- params[4]
  phi <- params[5]
  tau1 <- params[6]
  tau2 <- params[7]
  sigma_ut <- params[8] #variance of our ut in the measurement equation
  
  T <- length(returns)
  
  model_results <- garch11_realised(params,returns, RM)
  sigma2 <- model_results$sigma2
  ut <- model_results$ut
  
  #required conditions for realized garch -  https://core.ac.uk/download/pdf/41239655.pdf - page 6 of this paper
  # 0 < β + φγ < 1 and  ω + γξ > 0
  if (all(is.finite(params)) && omega > 0 && beta > 0 && gamma > 0 && 0 < (beta + phi * gamma) && (beta + phi * gamma) < 1 && (omega + gamma * xi) > 0) {
    #quasi log-likelihood
    log_lik_1 <- - T/2* log(2*pi) - 1/2 * sum(log(sigma2)) - 1/2 * sum(returns^2 / sigma2) #this is the first part l(r)
    log_lik_2 <- - T/2 * log(2 * pi) - 1/2 * sum(log(sigma_ut)) -1/2 * sum((ut ^ 2 / sigma_ut ^ 2)) #this is the second part l(x|r)
    neg_lik <- - (log_lik_1 + log_lik_2)
  } else {
    neg_lik <- Inf
  }
  return(neg_lik)
}


estimating_mle_realised <- function(start_params,returns, RM) {
  lower_bounds = c(1e-8, 1e-8, 1e-8, 8e-8, -10, 1e-8, 1e-8, 1e-8)
  upper_bounds = c(1, 0.99, 0.99, 1, 99, 1, 1, 1)
  
  
  mle <- nlminb(start = start_params,
                objective = function(start_params) objective_fn(start_params,returns, RM),
                lower = lower_bounds,
                upper = upper_bounds)
  return(mle)
}

hybridSolver <- function(start_params, returns, RM) {
  #Also trying with a hybrid solver as this is usually used in rugarch package for realised garch models
  ##but we are facing some issues with solnp Inf warning...
  lower_bounds = c(1e-8, 1e-8, 1e-8, -1e-6, -10, 1e-8, 1e-8, 1e-8)
  upper_bounds = c(1, 0.99, 0.99, 1, 99, 1, 1, 1)
  
  wrapped_objective_fn <- function(start_params) objective_fn(start_params, returns, RM)
  
  # Trying with 'solnp'
  solnp_result <- solnp(pars = start_params, fun = wrapped_objective_fn, eqfun = function(p) NULL, eqB = NULL, 
                        LB = lower_bounds, UB = upper_bounds, control = list(trace = 1))
  
  # Check if solnp converges
  if (solnp_result$convergence == 0) {
    cat("\nsolnp solver succeeded...\n")
    return(solnp_result)
  }
  
  # If solnp fails, trying with 'nlminb'
  cat("\nTrying nlminb solver...\n")
  nlminb_result <- nlminb(start = start_params, objective = wrapped_objective_fn, 
                          lower = lower_bounds, upper = upper_bounds, 
                          control = list(trace = 1))
  
  if (nlminb_result$convergence == 0) {
    cat("\nnlminb solver succeeded...\n")
    return(nlminb_result)
  }
  
  #if both solvers fail, we return the best one based on their lowest objective value
  cat("\nNeither solver achieved convergence. Returning the best available solution...\n")
  if (nlminb_result$objective < solnp_result$objective) {
    return(nlminb_result)
  } else {
    return(solnp_result)
  }
}


# Initial parameters - We are unsure in how to correctly initialised these,
start_params_realised = c(0.001, 0.5, 0.3, 0.01, 0.1, 0.01, 0.01, 0.3)
initial_log_lik <- objective_fn(start_params_realised, spyreal[,1], spyreal[,2])
#nlimb etimation
nlimb_est <-  estimating_mle_realised(start_params_realised, spyreal[, 1], spyreal[, 2])
log_lik_nlimb <- nlimb_est$objective
nlimb_par <- nlimb_est$par
#hybrid_est
hybrid_est <- hybridSolver(start_params_realised,spyreal[, 1], spyreal[, 2])
hybrid_est_par <- hybrid_est$par
log_lik_hybrid <- hybrid_est$objective
#Comparing loglik
log_lik_comparison <- cbind(initial_log_lik,log_lik_hybrid, log_lik_nlimb, log_lik_hybrid)
log_lik_comparison
names(nlimb_par) <- c("omega", "beta", "gamma", "xi", "phi", "tau1", "tau2", "sigma_ut")
names(hybrid_est_par) <- c("omega", "beta", "gamma", "xi", "phi", "tau1", "tau2", "sigma_ut")


#RUGARCH ESTIMATION###
realised_spec = ugarchspec(mean.model = list(armaOrder = c(0, 0), include.mean = FALSE), variance.model = list(model = 'realGARCH', garchOrder = c(1, 1)))
#multiplying by 100 because rugarch uses variables type as percentage https://www.r-bloggers.com/2014/01/the-realized-garch-model/
realised_fit = ugarchfit(realised_spec, spyreal[, 1] * 100, realizedVol = spyreal[,2] *100)
params_rugarch_realised <- coef(realised_fit)
names(params_rugarch_realised) <- c("omega", "gamma", "beta", "tau1", "tau2", "phi", "sigma_ut", "xi")

#Comparing coefficients
options(scipen = 999)
comparison_realised <- data.frame(
  nlimb = nlimb_par,
  hybrid = hybrid_est_par,
  Rugarch = params_rugarch_realised[1:8]
)
comparison_realised[] <- lapply(comparison_realised, function(x) sprintf("%.4f", as.numeric(x)))
print(comparison_realised)

#Comparing ACF, PACF and QQQ plots
# residuals_mine <- results_fit$ut
# residuals_rugarch <- residuals(realised_fit)
# plot(residuals_mine, type = 'l')
# plot(residuals_rugarch)
# 
# plot_acf(residuals_mine, "Realised GARCH")
# plot_acf(residuals_rugarch, "Rugarch Realised")
# 
# plot_pacf(residuals_mine, "Realised GARCH")
# plot_pacf(residuals_rugarch, "Rugarch Realised")
# 
# plot_qq(residuals_mine, "Realised GARCH")
# plot_qq(residuals_rugarch, "Rugarch Realised")

# shapiro.test(residuals_rugarch)  
#   
# Box.test(residuals_rugarch, lag = 20, type = "Ljung-Box")