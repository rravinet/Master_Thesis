library(ggplot2)
library(gridExtra)

# Load the data
ES_0.01_rhg <- read.csv("/Users/raphaelravinet/Downloads/FINAL Forecast VaR Results/AAPL.csv/realised_har_garchhorizon1_ES_0.01.csv")
ES_0.01_rg <- read.csv("/Users/raphaelravinet/Downloads/FINAL Forecast VaR Results/AAPL.csv/realised_garchhorizon1_ES_0.01.csv")
ES_0.05_rhg <- read.csv("/Users/raphaelravinet/Downloads/FINAL Forecast VaR Results/AAPL.csv/realised_har_garchhorizon1_ES_0.05.csv")
ES_0.05_rg <- read.csv("/Users/raphaelravinet/Downloads/FINAL Forecast VaR Results/AAPL.csv/realised_garchhorizon1_ES_0.05.csv")
appl_ret <- all_results$AAPL.csv$test_data$returns

# Extract the ES values and dates if available
es_rhg_values <- ES_0.01_rhg$Value
es_rg_values <- ES_0.01_rg$Value
es_rhg_values_005 <- ES_0.05_rhg$Value
es_rg_values_005 <- ES_0.05_rg$Value

# Function to calculate realized ES
calculate_realized_es <- function(returns, quantile) {
  threshold <- quantile(returns, quantile)
  es_values <- returns[returns <= threshold]
  return(mean(es_values))
}

# historical ES for the given quantiles (1% and 5%)
historical_es_001 <- calculate_realized_es(appl_ret, 0.01)
historical_es_005 <- calculate_realized_es(appl_ret, 0.05)
print(historical_es_005)
print(historical_es_001)

# Define the range for accepted risk levels L around the historical ES values
L_values <- seq(historical_es_001 - 0.02, historical_es_001 + 0.04, by = 0.001)
L_values_005 <- seq(historical_es_005 - 0.05, historical_es_005 + 0.04, by = 0.001)
print(L_values)
print(L_values_005)

# Function to calculate hedge ratio H_t(L)
calculate_hedge_ratio <- function(predicted_es, L) {
  ratio <- (predicted_es - L) / predicted_es
  ratio[ratio < 0] <- 0
  return(ratio)
}

# Function to calculate hedged returns using the given formula
calculate_hedged_returns <- function(returns, hedge_ratios) {
  hedged_returns <- returns * (1 - hedge_ratios)
  return(hedged_returns)
}

# Function to calculate annualized average percentage returns
calculate_annualized_returns <- function(returns) {
  daily_returns <- returns + 1
  cumulative_returns <- prod(daily_returns)
  annualized_returns <- cumulative_returns^(252/length(returns)) - 1
  return(annualized_returns)
}

#df to store the results
results_df <- data.frame(L = numeric(), Realized_ES_RG_001 = numeric(), Realized_ES_RHG_001 = numeric())
returns_results_df <- data.frame(L = numeric(), Annualized_Returns_RG_001 = numeric(), Annualized_Returns_RHG_001 = numeric())

results_df_005 <- data.frame(L = numeric(), Realized_ES_RG_005 = numeric(), Realized_ES_RHG_005 = numeric())
returns_results_df_005 <- data.frame(L = numeric(), Annualized_Returns_RG_005 = numeric(), Annualized_Returns_RHG_005 = numeric())

# 1% ES
for (L in L_values) {
  # Calculate the hedge ratios for the given level of L
  hedge_ratios_rg_001 <- calculate_hedge_ratio(es_rg_values, L)
  hedge_ratios_rhg_001 <- calculate_hedge_ratio(es_rhg_values, L)
  
  # Calculate hedged returns for the given level of L using the provided formula
  hedged_returns_rg_001 <- calculate_hedged_returns(appl_ret, hedge_ratios_rg_001)
  hedged_returns_rhg_001 <- calculate_hedged_returns(appl_ret, hedge_ratios_rhg_001)
  cost_of_hedge_rg_001 <- appl_ret - hedged_returns_rg_001
  cost_of_hedge_rhg_001 <- appl_ret - hedged_returns_rhg_001
  
  # Calculate realized ES for hedged returns
  realized_es_rg_001 <- calculate_realized_es(hedged_returns_rg_001, 0.01)
  realized_es_rhg_001 <- calculate_realized_es(hedged_returns_rhg_001, 0.01)
  
  results_df <- rbind(results_df, data.frame(L = L, Realized_ES_RG_001 = realized_es_rg_001, Realized_ES_RHG_001 = realized_es_rhg_001))
  
  # Calculate annualized average percentage returns for hedged returns
  annualized_returns_rg_001 <- calculate_annualized_returns(hedged_returns_rg_001)
  annualized_returns_rhg_001 <- calculate_annualized_returns(hedged_returns_rhg_001)
  
  returns_results_df <- rbind(returns_results_df, data.frame(L = L, Annualized_Returns_RG_001 = annualized_returns_rg_001, Annualized_Returns_RHG_001 = annualized_returns_rhg_001))
}

# 5% ES
for (L in L_values_005) {
  # Calculate the hedge ratios for the given level of L
  hedge_ratios_rg_005 <- calculate_hedge_ratio(es_rg_values_005, L)
  hedge_ratios_rhg_005 <- calculate_hedge_ratio(es_rhg_values_005, L)
  
  # Calculate hedged returns for the given level of L using the provided formula
  hedged_returns_rg_005 <- calculate_hedged_returns(appl_ret, hedge_ratios_rg_005)
  hedged_returns_rhg_005 <- calculate_hedged_returns(appl_ret, hedge_ratios_rhg_005)
  
  # Calculate realized ES for hedged returns
  realized_es_rg_005 <- calculate_realized_es(hedged_returns_rg_005, 0.05)
  realized_es_rhg_005 <- calculate_realized_es(hedged_returns_rhg_005, 0.05)
  
  # Append the results to the data frame
  results_df_005 <- rbind(results_df_005, data.frame(L = L, Realized_ES_RG_005 = realized_es_rg_005, Realized_ES_RHG_005 = realized_es_rhg_005))
  
  # Calculate annualized average percentage returns for hedged returns
  annualized_returns_rg_005 <- calculate_annualized_returns(hedged_returns_rg_005)
  annualized_returns_rhg_005 <- calculate_annualized_returns(hedged_returns_rhg_005)
  annualized_returns_test <- calculate_annualized_returns(appl_ret)
  
  # Append the results to the data frame
  returns_results_df_005 <- rbind(returns_results_df_005, data.frame(L = L, Annualized_Returns_RG_005 = annualized_returns_rg_005, Annualized_Returns_RHG_005 = annualized_returns_rhg_005))
}

# Plotting the results for 1% ES and returns
p1 <- ggplot(results_df, aes(x = L)) +
  geom_line(aes(y = Realized_ES_RG_001, color = "RG 0.01"), size = 1) +
  geom_line(aes(y = Realized_ES_RHG_001, color = "RHG 0.01"), size = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  labs(title = "Expected Shortfall for Hedged Returns (1% Level)",
       x = "Accepted Risk Level (L)",
       y = "Expected Shortfall (Hedged)",
       color = "Model") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Plotting the adjusted annualized returns for 1% ES
p2 <- ggplot(returns_results_df, aes(x = L)) +
  geom_line(aes(y = Annualized_Returns_RG_001, color = "RG 0.01"), size = 1) +
  geom_line(aes(y = Annualized_Returns_RHG_001, color = "RHG 0.01"), size = 1) +
  labs(title = "Annualized Returns for Hedged Strategies (1% Level)",
       x = "Accepted Risk Level (L)",
       y = "Annualized Returns",
       color = "Model") + 
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Display plot
print(p2)


# Plotting the results for 5% ES
p3 <- ggplot(results_df_005, aes(x = L)) +
  geom_line(aes(y = Realized_ES_RG_005, color = "RG 0.05"), size = 1) +
  geom_line(aes(y = Realized_ES_RHG_005, color = "RHG 0.05"), size = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  labs(title = "Expected Shortfall for Hedged Returns (5% Level)",
       x = "Accepted Risk Level (L)",
       y = "Expected Shortfall (Hedged)",
       color = "Model") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Plotting the annualized returns for 5% ES
p4 <- ggplot(returns_results_df_005, aes(x = L)) +
  geom_line(aes(y = Annualized_Returns_RG_005, color = "RG 0.05"), size = 1) +
  geom_line(aes(y = Annualized_Returns_RHG_005, color = "RHG 0.05"), size = 1) +
  labs(title = "Annualized Returns for Hedged Strategies (5% Level)",
       x = "Accepted Risk Level (L)",
       y = "Annualized Returns",
       color = "Model") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Combining plots
grid.arrange(p1, p2, ncol = 1)
grid.arrange(p3, p4, ncol = 1)

mse_results
