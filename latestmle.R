library(dplyr)
library(zoo)
library(xts)
library(lubridate)
library(rugarch)
library(tseries)
library(stats)
library(data.table)
library(Metrics)
data("spyreal")

##DATA### - We are testing with simulated data
#Simulating data - Data simulated by using Alessandro's code.
t_max <- 1000
w <- 0.01
alpha <- 0.05
beta <- 0.949

z <- rnorm(t_max)
sigma <- c(1, rep(NA, t_max))
r     <- c(sigma[1]*z[1], rep(NA, t_max))
for (t in 2:(t_max+1)) {
  set.seed(t*212) # for reproducibility
  sigma[t] <- w + alpha*r[t-1] + beta*sigma[t-1]
  r[t]     <- sigma[t]*z[t]
}
r <- r[100:1000]
# plot.ts(r)
# plot.ts(r^2)


#Simulation 2  - Let's simulate our own data now
set.seed(123)
omega_true <- 0.01
alpha_true <- 0.0109
beta_true <- 0.89

#Number of obs
T <- 1000

sigma2 <- numeric(T)
epsilon <- numeric(T)
returns <- numeric(T)

#initial value for sigma2
sigma2[1] <- omega_true / (1 - alpha_true - beta_true)  # initialising sigma as unconditional variance

# Generating the data
for (t in 2:T) {
  epsilon[t] <- rnorm(1, mean = 0, sd = sqrt(sigma2[t-1]))
  returns[t] <- epsilon[t]
  sigma2[t] <- omega_true + alpha_true * returns[t-1]^2 + beta_true * sigma2[t-1]
}

returns <- returns[200 : T]
#END OF SIMULATIONS

#APPLE RETURNS 
# apple_daily_data <- read.csv('AAPL_daily_data.csv')
# apple_daily_data <- na.omit(apple_daily_data)
# apple_returns <- exp(apple_daily_data$log_ret) - 1
# #BELOW IS THE REALIZE VAR THAT TWE DON'T CARE FOR NOW
# apple_daily_data$Date <- as.Date(apple_daily_data$Day)
# index(realized_var) <- as.Date(index(realized_var))
# # Filtering realized_var to match the dates in apple_daily_returns
# aligned_realized_var <- realized_var[index(realized_var) %in% apple_daily_data$Day, ]
# aligned_apple_daily <- apple_daily_data[(apple_daily_data$Day) %in% index(realized_var), ]
# colnames(aligned_realized_var) <- "RV"
# returns <- exp(aligned_apple_daily$log_ret) - 1
# RM <- aligned_realized_var$RV
# returns <- xts(returns, order.by = aligned_apple_daily$Date)


###STANDARD GARCH ###
##OUR OWN MLE FUNCTIONS###
garch11 <- function (returns, params) {
  omega <- params[1]
  alpha <- params[2]
  beta <- params[3]
  T <- length(returns)
  z <- length(returns)
  sigma_2 <- numeric(T)
  sigma_2[1] <- omega / (1 - alpha - beta) #same thing, starting sigma2 as our unconditional variance
  z[1] <- returns[1] / sigma_2[1]
  for (t in 2:T){
      sigma_2[t] <- omega + alpha * returns[t-1]^2 + beta * sigma_2[t-1]
      z[t] <- returns[t] / sqrt(sigma_2[t])
    }
    return(list(sigma_2 = sigma_2,z = z))
  }
  
neg_loglik <- function(returns, params){
  omega <- params[1]
  alpha <- params[2]
  beta <- params[3]
  T <- length(returns)
  sigma_2 <- garch11(returns,params)$sigma_2
  
  if (all(is.finite(params)) && omega > 0 && alpha > 0 && beta > 0 & alpha + beta < 1) {
    log_lik <- - T/2* log(2*pi) - 1/2 * sum(log(sigma_2)) - 1/2 * sum(returns^2 / sigma_2)
    neg_lik <- - log_lik
  } else {
    neg_lik <- Inf
  }
  return(neg_lik)
}

estimating_mle <- function(returns, start_params) {
  mle <- nlminb(start = start_params,
                objective = function(params) neg_loglik(returns, params),
                lower = c(0, 0, 0),
                upper = c(Inf, 1, 1))
  
  return(mle)
}
#END OF OUR MLE FUNCTIONS

#Setting initial parameters 
start_params <- c(0.01, 0.01, 0.9)

#Estimating models - Trying with both simulated datas and apple returns
mle_simulated <- estimating_mle(returns = r, start_params = start_params)
params_mine_simulated <- mle_simulated$par
mle_simulated2 <- estimating_mle(returns = returns, start_params = start_params)
params_mine_simulated2 <- mle_simulated2$par
# mle <- estimating_mle(returns = apple_returns, start_params = start_params)
# params_mine<- mle$par
residuals_mine_simulated_1 <- garch11(returns = r, params = start_params)$z
residuals_mine_simulated_2 <- garch11(returns = returns, params = start_params)$z

#Estimating models with rugarch library
spec_rugarch<- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                           mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                           distribution.model = "norm") # defining the spec - this will be the same for all our models in this part
#simulated data
fit_rugarch_simulated <- ugarchfit(spec = spec_rugarch, data = r)
params_rugarch_simulated <- as.numeric(coef(fit_rugarch_simulated))
residuals_rugarch_simulated <- residuals(fit_rugarch_simulated)
#simulated data 2
fit_rugarch_simulated2 <- ugarchfit(spec = spec_rugarch, data = returns)
params_rugarch_simulated2 <- as.numeric(coef(fit_rugarch_simulated2))
residuals_rugarch_simulated2 <- residuals(fit_rugarch_simulated2)

#apple
# fit_rugarch <- ugarchfit(spec = spec_rugarch, data = apple_returns)
# params_rugarch <- as.numeric(coef(fit_rugarch))

#Estimating models using tseries
garch_tseries_simulated <- garch(r, c(1,1), verbose = FALSE)
params_tseries_simulated <- as.numeric(coef(garch_tseries_simulated))
#simulated 2
garch_tseries_simulated2 <- garch(returns, c(1,1), verbose = FALSE)
params_tseries_simulated2 <- as.numeric(coef(garch_tseries_simulated2))
#apple
# garch_tseries <- garch(apple_returns, c(1,1), verbose = FALSE)
# params_tseries <- as.numeric(coef(garch_tseries))

# True parameters used in the simulations
true_params_1 <- c(w, alpha, beta)
true_params_2 <- c(w, alpha_true, beta_true)

# Combining all results into a data frame
comparison <- data.frame(
  True = true_params_1,
  Mine = params_mine_simulated,
  Rugarch = params_rugarch_simulated[1:3],
  tseries = params_tseries_simulated[1:3]
)

row.names(comparison) <- c("Omega", "Alpha", "Beta")
comparison2 <- data.frame(
  True = true_params_2,
  Mine = params_mine_simulated2,
  Rugarch = params_rugarch_simulated2[1:3],
  tseries = params_tseries_simulated2[1:3]
)

row.names(comparison2) <- c("Omega", "Alpha", "Beta")

# comparison_apple <- data.frame(
#   Mine = params_mine,
#   Rugarch = params_rugarch[1:3],
#   tseries = params_tseries[1:3]
# )
# 
# row.names(comparison_apple) <- c("Omega", "Alpha", "Beta")

print(comparison) # results for the first simulated dataset
print(comparison2)# results for the second simulated dataset
# print(comparison_apple) # results for apple data.


#Comparing ACF, PACF and QQQ plots

plot_acf <- function(residuals, name){
  acf(residuals, main = sprintf("ACF of %s Residuals", name))
}
plot_pacf <- function(residuals, name){
  pacf(residuals, main = sprintf("PACF of %s Residuals", name))
}
plot_qq <- function(residuals, name){
  qqnorm(residuals,main = sprintf("QQ Plot of %s Residuals", name))
  qqline(residuals, col = "red")
}

plot_acf(residuals_mine_simulated_1, "Manual S1")
plot_acf(residuals_mine_simulated_2, "Manual S2")
plot_acf(residuals_rugarch_simulated, "RUGARCH S1")
plot_acf(residuals_rugarch_simulated2, "RUGARCH S2")

plot_pacf(residuals_mine_simulated_1, "Manual S1")
plot_pacf(residuals_mine_simulated_2, "Manual S2")
plot_pacf(residuals_rugarch_simulated, "RUGARCH S1")
plot_pacf(residuals_rugarch_simulated2, "RUGARCH S2")

plot_qq(residuals_mine_simulated_1, "Manual S1")
plot_qq(residuals_mine_simulated_2, "Manual S2")
plot_qq(residuals_rugarch_simulated, "RUGARCH S1")
plot_qq(residuals_rugarch_simulated2, "RUGARCH S2")


### MLE FOR REALIZED GARCH### 
#Here is the bit where we are unsure about. We are following the equations from the papers and
#it seems it is correct. We are testing with SPY dataset and trying to compare with the RUGARCH package.
#We can see that our ACF and PACFS are terrible..
#At the end of the day, it isn't about matching rugarch package, because from what we've seen, 
#the rugarch package handles the realised garch model differently, it has an alpha and a return term in the
#sigma equation, instead of a realised measure. 
#But this is more about we estimating the realised garch ourselves correctly, which is definitely not happening, haha.

  
garch11_realised <- function (returns, params, RM) {
  omega <- params[1]
  beta <- params[2]
  gamma <- params[3]
  xi <- params[4]
  phi <- params[5]
  tau1 <- params[6]
  tau2 <- params[7]
  T <- length(returns)
  
  log_sigma2 <- numeric(T)
  log_x <- log(RM)
  ut <- numeric(T)
  z <- numeric(T)
  
  log_sigma2[1] <- log(var(returns))
  z[1] <- returns[1] / sqrt(exp(log_sigma2[1]))
  
  for (t in 2:T){
    log_sigma2[t] <- omega + beta * log_sigma2[t-1] + gamma * log_x[t-1]
    z[t] <- returns[t] / sqrt(exp(log_sigma2[t]))
    
    tau_zt <- tau1 *z[t] + tau2 * (z[t] ^ 2 - 1) #our leverage function
    ut[t]<- log_x[t] - xi - log_sigma2[t] - tau_zt # rearranging measurement equation to calculate the residuals
    
  }
  return(list(log_sigma2 = log_sigma2, ut = ut, z = z))
}

neg_loglik_realised <- function(returns, params, RM){
  omega <- params[1]
  beta <- params[2]
  gamma <- params[3]
  xi <- params[4]
  phi <- params[5]
  tau1 <- params[6]
  tau2 <- params[7]
  sigma_ut <- params[8]
  T <- length(returns)
  model_results <- garch11_realised(returns,params, RM)
  log_sigma2 <- model_results$log_sigma2
  ut <- model_results$ut
  
  #(beta  + phi * gamma) < 1 
  if (all(is.finite(params)) && omega > 0 && beta > 0 && gamma > 0 && xi > 0 && phi > 0 && tau1 > 0 && tau2 > 0 && beta + phi * gamma < 1) {
    #quasi log-likelihood
    log_lik_1 <- - T/2* log(2*pi) - 1/2 * sum(log_sigma2) - 1/2 * sum(returns^2 / exp(log_sigma2)) #this is the first part l(r)
    log_lik_2 <- - T/2 * log(2 * pi) - 1/2 * log(sigma_ut) -1/2 * (ut ^ 2 / sigma_ut ^ 2) #this is the second part l(x|r)
    neg_lik <- - (log_lik_1 + log_lik_2)
  } else {
    neg_lik <- Inf
  }
  return(neg_lik)
}

estimating_mle_realised <- function(returns, start_params, RM) {
  mle <- nlminb(start = start_params,
                objective = function(params) neg_loglik_realised(returns, params, RM),
                lower = c(0, 0, 0),
                upper = c(Inf, 1, 1))
  
  return(mle)
}

estimating_mle_realised <- function(returns, start_params, RM) {
  lower_bounds <- c(0.00001, 0, 0, 0.00001, 0, 0.00001, 0.00001, 0.00001)  # All parameters have a positive lower bound
  upper_bounds <- c(Inf, 1, 1, Inf, 1, Inf, Inf, Inf)  # Some parameters are unbounded on the upper side, like our coefficients.
  
  # Run the optimization
  mle <- nlminb(start = start_params,
                objective = function(params) neg_loglik_realised(returns, params, RM),
                lower = lower_bounds,
                upper = upper_bounds)
  
  return(mle)
}

# Initial parameter
start_params_realised = c(0.01, 0.5, 0.35, 0.01, 0.1, 0.01, 0.01, 0.01)
results = estimating_mle_realised(spyreal[, 1], start_params_realised, spyreal[, 2])
params_mine_realised <-results$par 
results$convergence
results_fit <- garch11_realised(spyreal[,1], start_params_realised, spyreal[,2])

plot(results_fit$ut)

realised_spec = ugarchspec(mean.model = list(armaOrder = c(0, 0), include.mean = FALSE), variance.model = list(model = 'realGARCH', garchOrder = c(1, 1)))
#multiplying by 100 because rugarch uses variables type as percentage https://www.r-bloggers.com/2014/01/the-realized-garch-model/
realised_fit = ugarchfit(realised_spec, spyreal[, 1] * 100, solver = 'hybrid', realizedVol = spyreal[,2] *100)
params_rugarch_realised <- coef(realised_fit)


#Comparing coefficients
comparison_realised <- data.frame(
  Mine = params_mine_realised,
  Rugarch = params_rugarch_realised[1:8]
)

# row.names(comparison_realised) <- c("Omega", "Alpha", "Beta","Omega", "Al", "delta", "xi"  )

print(comparison_realised)

#Comparing ACF, PACF and QQQ plots
residuals_mine <- results_fit$ut
residuals_rugarch <- residuals(realised_fit)
plot(residuals_mine, type = 'l')
plot(residual_rugarch)

plot_acf(residuals_mine, "Realised GARCH")
plot_acf(residuals_rugarch, "Rugarch Realised")

plot_pacf(residuals_mine, "Realised GARCH")
plot_pacf(residuals_rugarch, "Rugarch Realised")

plot_qq(residuals_mine, "Realised GARCH")
plot_qq(residuals_rugarch, "Rugarch Realised")
  
  
  
  
  
