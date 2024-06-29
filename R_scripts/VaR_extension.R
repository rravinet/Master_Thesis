library(ggplot2)
library(gridExtra)

# Load the data
# ES_0.01_rhg <- read.csv("/Users/raphaelravinet/Downloads/FINAL Forecast VaR Results/AAPL.csv/realised_har_garchhorizon1_ES_0.01.csv")
# ES_0.01_rg <- read.csv("/Users/raphaelravinet/Downloads/FINAL Forecast VaR Results/AAPL.csv/realised_garchhorizon1_ES_0.01.csv")
# ES_0.05_rhg <- read.csv("/Users/raphaelravinet/Downloads/FINAL Forecast VaR Results/AAPL.csv/realised_har_garchhorizon1_ES_0.05.csv")
# ES_0.05_rg <- read.csv("/Users/raphaelravinet/Downloads/FINAL Forecast VaR Results/AAPL.csv/realised_garchhorizon1_ES_0.05.csv")

ES_0.01_rg <- rg_pred_h1_ibm$ES_0.01
ES_0.01_rhg <-  rhg_pred_h1_ibm$ES_0.01
ES_0.05_rg <- rg_pred_h1_ibm$ES_0.05
ES_0.05_rhg <- rhg_pred_h1_ibm$ES_0.05


# Function to calculate realized ES
calculate_realized_es <- function(returns, quantile) {
  threshold <- quantile(returns, quantile)
  es_values <- returns[returns <= threshold]
  return(mean(es_values))
}

# Calculate the historical ES for the given quantiles (1% and 5%)
historical_es_001 <- calculate_realized_es(all_results$IBM.csv$train_data$returns, 0.01)
historical_es_005 <- calculate_realized_es(all_results$IBM.csv$train_data$returns, 0.05)
print(historical_es_005)
print(historical_es_001)

# Define the range for accepted risk levels L around the historical ES values
L_values <- seq(historical_es_001 - 0.03, historical_es_001 + 0.03, by = 0.01)
L_values_005 <- seq(historical_es_005 - 0.03, historical_es_005 + 0.03, by = 0.01)
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

# Initialize data frames to store the results
results_df <- data.frame(L = numeric(), Realized_ES_RG_001 = numeric(), Realized_ES_RHG_001 = numeric())
returns_results_df <- data.frame(L = numeric(), Annualized_Returns_RG_001 = numeric(), Annualized_Returns_RHG_001 = numeric(), Cost_of_Hedging_RG_001 = numeric(), Cost_of_Hedging_RHG_001 = numeric())

results_df_005 <- data.frame(L = numeric(), Realized_ES_RG_005 = numeric(), Realized_ES_RHG_005 = numeric())
returns_results_df_005 <- data.frame(L = numeric(), Annualized_Returns_RG_005 = numeric(), Annualized_Returns_RHG_005 = numeric(), Cost_of_Hedging_RG_005 = numeric(), Cost_of_Hedging_RHG_005 = numeric())

# Loop over each risk level for 1% ES
for (L in L_values) {
  # Calculate the hedge ratios for the given level of L
  hedge_ratios_rg_001 <- calculate_hedge_ratio(ES_0.01_rg, L)
  hedge_ratios_rhg_001 <- calculate_hedge_ratio(ES_0.01_rhg, L)
  
  # Calculate hedged returns for the given level of L using the provided formula
  hedged_returns_rg_001 <- calculate_hedged_returns(all_results$IBM.csv$test_data$returns, hedge_ratios_rg_001)
  hedged_returns_rhg_001 <- calculate_hedged_returns(all_results$IBM.csv$test_data$returns, hedge_ratios_rhg_001)
  
  # Calculate the cost of hedging
  cost_of_hedge_rg_001 <- all_results$IBM.csv$test_data$returns - hedged_returns_rg_001
  cost_of_hedge_rhg_001 <- all_results$IBM.csv$test_data$returns - hedged_returns_rhg_001
  
  # Calculate realized ES for hedged returns
  realized_es_rg_001 <- calculate_realized_es(hedged_returns_rg_001, 0.01)
  realized_es_rhg_001 <- calculate_realized_es(hedged_returns_rhg_001, 0.01)
  
  # Append the results to the data frame
  results_df <- rbind(results_df, data.frame(L = L, Realized_ES_RG_001 = realized_es_rg_001, Realized_ES_RHG_001 = realized_es_rhg_001))
  
  # Calculate annualized average percentage returns for hedged returns
  annualized_returns_rg_001 <- calculate_annualized_returns(hedged_returns_rg_001)
  annualized_returns_rhg_001 <- calculate_annualized_returns(hedged_returns_rhg_001)
  
  # Append the results to the data frame
  returns_results_df <- rbind(returns_results_df, data.frame(L = L, Annualized_Returns_RG_001 = annualized_returns_rg_001, Annualized_Returns_RHG_001 = annualized_returns_rhg_001, Cost_of_Hedging_RG_001 = mean(cost_of_hedge_rg_001), Cost_of_Hedging_RHG_001 = mean(cost_of_hedge_rhg_001)))
}

# Loop over each risk level for 5% ES
for (L in L_values_005) {
  # Calculate the hedge ratios for the given level of L
  hedge_ratios_rg_005 <- calculate_hedge_ratio(ES_0.05_rg, L)
  hedge_ratios_rhg_005 <- calculate_hedge_ratio(ES_0.05_rhg, L)
  
  # Calculate hedged returns for the given level of L using the provided formula
  hedged_returns_rg_005 <- calculate_hedged_returns(all_results$IBM.csv$test_data$returns, hedge_ratios_rg_005)
  hedged_returns_rhg_005 <- calculate_hedged_returns(all_results$IBM.csv$test_data$returns, hedge_ratios_rhg_005)
  
  # Calculate the cost of hedging
  cost_of_hedge_rg_005 <- all_results$IBM.csv$test_data$returns - hedged_returns_rg_005
  cost_of_hedge_rhg_005 <- all_results$IBM.csv$test_data$returns - hedged_returns_rhg_005
  
  # Calculate realized ES for hedged returns
  realized_es_rg_005 <- calculate_realized_es(hedged_returns_rg_005, 0.05)
  realized_es_rhg_005 <- calculate_realized_es(hedged_returns_rhg_005, 0.05)
  
  # Append the results to the data frame
  results_df_005 <- rbind(results_df_005, data.frame(L = L, Realized_ES_RG_005 = realized_es_rg_005, Realized_ES_RHG_005 = realized_es_rhg_005))
  
  # Calculate annualized average percentage returns for hedged returns
  annualized_returns_rg_005 <- calculate_annualized_returns(hedged_returns_rg_005)
  annualized_returns_rhg_005 <- calculate_annualized_returns(hedged_returns_rhg_005)
  
  # Append the results to the data frame
  returns_results_df_005 <- rbind(returns_results_df_005, data.frame(L = L, Annualized_Returns_RG_005 = annualized_returns_rg_005, Annualized_Returns_RHG_005 = annualized_returns_rhg_005, Cost_of_Hedging_RG_005 = mean(cost_of_hedge_rg_005), Cost_of_Hedging_RHG_005 = mean(cost_of_hedge_rhg_005)))
}
L_values
library(ggplot2)
L_values
# Plotting the results for 1% ES and returns
p1 <- ggplot(results_df, aes(x = L)) +
  geom_line(aes(y = Realized_ES_RG_001, color = "RG 0.01"), size = 1) +
  geom_line(aes(y = Realized_ES_RHG_001, color = "RHG 0.01"), size = 1) +
  geom_line(aes(y = L_values, color = "black"), size = 1) +
  geom_vline(xintercept = historical_es_001, linetype = "dotted", color = "green") +
  labs(title = "Expected Shortfall for Hedged Returns (1% Level)",
       x = "Accepted Risk Level (L)",
       y = "Expected Shortfall (Hedged)",
       color = "Model") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
print(p1)


# Plotting the cost of hedging for 1% ES
p2 <- ggplot(returns_results_df, aes(x = L)) +
  geom_line(aes(y = Annualized_Returns_RG_001, color = "Cost of Hedging RG 0.01"), size = 1) +
  geom_line(aes(y = Annualized_Returns_RHG_001, color = "Cost of Hedging RHG 0.01"), size = 1) +
  geom_vline(xintercept = historical_es_001, linetype = "dotted", color = "orange") +
  labs(title = "Cost of Hedging for Strategies (1% Level)",
       x = "Accepted Risk Level (L)",
       y = "Cost of Hedging",
       color = "Model") + 
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

print(p2)

# Plotting the results for 5% ES
p3 <- ggplot(results_df_005, aes(x = L)) +
  geom_line(aes(y = Realized_ES_RG_005, color = "RG 0.05"), size = 1) +
  geom_line(aes(y = Realized_ES_RHG_005, color = "RHG 0.05"), size = 1) +
  geom_line(aes(y = L_values_005, color = "black"), size = 1) +
  labs(title = "Expected Shortfall for Hedged Returns (5% Level)",
       x = "Accepted Risk Level (L)",
       y = "Expected Shortfall (Hedged)",
       color = "Model") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Plotting the cost of hedging for 5% ES
p4 <- ggplot(returns_results_df_005, aes(x = L)) +
  geom_line(aes(y = Annualized_Returns_RG_005, color = "Cost of Hedging RG 0.05"), size = 1) +
  geom_line(aes(y = Annualized_Returns_RHG_005, color = "Cost of Hedging RHG 0.05"), size = 1) +
  labs(title = "Cost of Hedging for Strategies (5% Level)",
       x = "Accepted Risk Level (L)",
       y = "Cost of Hedging",
       color = "Model") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Combine plots
grid.arrange(p1, p2, ncol = 1)
grid.arrange(p3, p4, ncol = 1)
