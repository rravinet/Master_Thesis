############## Plots for all forecasts ##############

###### APPL ######
# Realised GARCH
# aapl_vol_fitted_10day_realised <- all_results$AAPL.csv$fitted_models$realised_garch$fitted_values$sigma2
appl_vol_forecast_10day_realised <- 100 * exp(read.csv('/Users/raphaelravinet/Code/BSE/Thesis/Results/Forecast_results_final/AAPL.csv/realised_garch_horizon_10.csv')) # 10-day ahead

# HAR-GARCH
appl_vol_forecast_10day_har <- 100 * exp(read.csv('/Users/raphaelravinet/Code/BSE/Thesis/Results/Forecast_results_final/AAPL.csv/realised_har_garch_horizon_10.csv')) # 10-day ahead

combined_aapl_10day
# Make dataframe
combined_aapl_10day <- data.frame(
  Day = as.Date(all_results$AAPL.csv$test_data$Day[1:(length(all_results$AAPL.csv$test_data$Day) - 9)], format = "%Y-%m-%d"),
  Adjusted_RM = 100 * all_results$AAPL.csv$test_data$RM_adj[1:(length(all_results$AAPL.csv$test_data$Day) - 9)],
  Realised_Garch_Volatilty_Forecast_10day = appl_vol_forecast_10day_realised$forecast_data,
  HAR_Garch_Volatility_Forecast_10day = appl_vol_forecast_10day_har$forecast_data
)

aapl_date_range <- range(combined_aapl_10day$Day, na.rm = TRUE)


# Plot graph
aapl_10day_plot <- 
  ggplot(combined_aapl_10day, aes(x = Day)) +
  geom_line(aes(y = Adjusted_RM, color = "Adjusted RM"), size = 0.5) +
  geom_line(aes(y = Realised_Garch_Volatilty_Forecast_10day, color = "Realised GARCH"), size = 0.3, na.rm = TRUE) +
  geom_line(aes(y = HAR_Garch_Volatility_Forecast_10day, color = "Realised HAR-GARCH"), size = 0.4, na.rm = TRUE) +
  labs(x = "AAPL", y = "", color = NULL) +
  scale_color_manual(values = c("Adjusted RM" = "black", "Realised GARCH" = "blue", "Realised HAR-GARCH" = "red")) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 1.5), breaks = seq(0, 3, by = 0.5)) +
  scale_x_date(limits = aapl_date_range, date_labels = "%m/%Y", date_breaks = "6 months") +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.ticks.length = unit(0.2, "cm"),  # Add tick marks
    axis.ticks = element_line(color = "black"),  # Color of tick marks
    axis.line = element_blank(),  # Remove axis lines
    panel.border = element_rect(color = "black", fill = NA, size = 0.6)  # Add border with uniform thickness
  )

ggsave("aapl_10day_plot.png", plot = aapl_10day_plot, width = 10, height = 6, dpi = 600)
print(aapl_10day_plot)

# IBM
ibm_vol_forecast_10day_realised <- 100 * exp(read.csv('/Users/raphaelravinet/Code/BSE/Thesis/Results/Forecast_results_final/IBM.csv/realised_garch_horizon_10.csv')) # 10-day ahead
ibm_vol_forecast_10day_har <- 100 * exp(read.csv('/Users/raphaelravinet/Code/BSE/Thesis/Results/Forecast_results_final/IBM.csv/realised_har_garch_horizon_10.csv')) # 10-day ahead


combined_ibm_10day <- data.frame(
  Day = as.Date(all_results$IBM.csv$test_data$Day[10:(length(all_results$IBM.csv$test_data$Day))], format = "%Y-%m-%d"),
  Adjusted_RM = 100 * all_results$IBM.csv$test_data$RM_adj[10:(length(all_results$IBM.csv$test_data$Day))],
  Realised_Garch_Volatilty_Forecast_10day = ibm_vol_forecast_10day_realised$forecast_data,
  HAR_Garch_Volatility_Forecast_10day = ibm_vol_forecast_10day_har$forecast_data
)

ibm_date_range <- range(combined_ibm_10day$Day, na.rm = TRUE)


# IBM plot
ibm_10day_plot <- 
  ggplot(combined_ibm_10day, aes(x = Day)) +
  geom_line(aes(y = Adjusted_RM, color = "Adjusted RM"), size = 0.5) +
  geom_line(aes(y = Realised_Garch_Volatilty_Forecast_10day, color = "Realised GARCH"), size = 0.3, na.rm = TRUE) +
  geom_line(aes(y = HAR_Garch_Volatility_Forecast_10day, color = "Realised HAR-GARCH"), size = 0.4, na.rm = TRUE) +
  labs(x = "IBM", y = "", color = NULL) +
  scale_color_manual(values = c("Adjusted RM" = "black", "Realised GARCH" = "blue", "Realised HAR-GARCH" = "red")) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 3, by = 0.25)) +
  scale_x_date(limits = ibm_date_range, date_labels = "%m/%Y", date_breaks = "6 months") +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.ticks.length = unit(0.2, "cm"),  # Add tick marks
    axis.ticks = element_line(color = "black"),  # Color of tick marks
    axis.line = element_blank(),  # Remove axis lines
    panel.border = element_rect(color = "black", fill = NA, size = 0.6)  # Add border with uniform thickness
  )
print(ibm_10day_plot)
ggsave("IBM_10day_plot.png", plot = ibm_10day_plot, width = 10, height = 6, dpi = 600)



#### PG ####
# PG data preparation
pg_vol_forecast_10day_realised <- 100 * exp(read.csv('/Users/raphaelravinet/Code/BSE/Thesis/Results/Forecast_results_final/PG.csv/realised_garch_horizon_10.csv')) # 10-day ahead
pg_vol_forecast_10day_har <- 100 * exp(read.csv('/Users/raphaelravinet/Code/BSE/Thesis/Results/Forecast_results_final/PG.csv/realised_har_garch_horizon_10.csv')) # 10-day ahead

combined_pg_10day <- data.frame(
  Day = as.Date(all_results$PG.csv$test_data$Day[1:(length(all_results$PG.csv$test_data$Day) - 9)], format = "%Y-%m-%d"),
  Adjusted_RM = 100 * all_results$PG.csv$test_data$RM_adj[1:(length(all_results$PG.csv$test_data$Day) - 9)],
  Realised_Garch_Volatilty_Forecast_10day = pg_vol_forecast_10day_realised$forecast_data,
  HAR_Garch_Volatility_Forecast_10day = pg_vol_forecast_10day_har$forecast_data
)

pg_date_range <- range(combined_pg_10day$Day, na.rm = TRUE)

# PG plot
pg_10day_plot <- 
  ggplot(combined_pg_10day, aes(x = Day)) +
  geom_line(aes(y = Adjusted_RM, color = "Adjusted RM"), size = 0.5) +
  geom_line(aes(y = Realised_Garch_Volatilty_Forecast_10day, color = "Realised GARCH"), size = 0.4, na.rm = TRUE) +
  geom_line(aes(y = HAR_Garch_Volatility_Forecast_10day, color = "Realised HAR-GARCH"), size = 0.4, na.rm = TRUE) +
  labs(x = "PG", y = "", color = NULL) +
  scale_color_manual(values = c("Adjusted RM" = "black", "Realised GARCH" = "blue", "Realised HAR-GARCH" = "red")) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 0.60), breaks = seq(0, 3, by = 0.25)) +
  scale_x_date(limits = pg_date_range, date_labels = "%m/%Y", date_breaks = "6 months") +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.ticks.length = unit(0.2, "cm"),  # Add tick marks
    axis.ticks = element_line(color = "black"),  # Color of tick marks
    axis.line = element_blank(),  # Remove axis lines
    panel.border = element_rect(color = "black", fill = NA, size = 0.6)  # Add border with uniform thickness
  )
print(pg_10day_plot)
ggsave("PG_10day_plot.png", plot = pg_10day_plot, width = 10, height = 6, dpi = 600)


#### WMT ####
# WMT data preparation
wmt_vol_forecast_10day_realised <- 100 * exp(read.csv('/Users/raphaelravinet/Code/BSE/Thesis/Results/Forecast_results_final/WMT.csv/realised_garch_horizon_10.csv')) # 10-day ahead
wmt_vol_forecast_10day_har <- 100 * exp(read.csv('/Users/raphaelravinet/Code/BSE/Thesis/Results/Forecast_results_final/WMT.csv/realised_har_garch_horizon_10.csv')) # 10-day ahead

combined_wmt_10day <- data.frame(
  Day = as.Date(all_results$WMT.csv$test_data$Day[1:(length(all_results$WMT.csv$test_data$Day) - 9)], format = "%Y-%m-%d"),
  Adjusted_RM = 100 * all_results$WMT.csv$test_data$RM_adj[1:(length(all_results$WMT.csv$test_data$Day) - 9)],
  Realised_Garch_Volatilty_Forecast_10day = wmt_vol_forecast_10day_realised$forecast_data,
  HAR_Garch_Volatility_Forecast_10day = wmt_vol_forecast_10day_har$forecast_data
)

wmt_date_range <- range(combined_wmt_10day$Day, na.rm = TRUE)

# WMT plot
wmt_10day_plot <- 
  ggplot(combined_wmt_10day, aes(x = Day)) +
  geom_line(aes(y = Adjusted_RM, color = "Adjusted RM"), size = 0.4) +
  geom_line(aes(y = Realised_Garch_Volatilty_Forecast_10day, color = "Realised GARCH"), size = 0.4, na.rm = TRUE) +
  geom_line(aes(y = HAR_Garch_Volatility_Forecast_10day, color = "Realised HAR-GARCH"), size = 0.4, na.rm = TRUE) +
  labs(x = "WMT", y = "", color = NULL) +
  scale_color_manual(values = c("Adjusted RM" = "black", "Realised GARCH" = "blue", "Realised HAR-GARCH" = "red")) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 0.75), breaks = seq(0, 3, by = 0.25)) +
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
print(wmt_10day_plot)
ggsave("WMT_10day_plot.png", plot = wmt_10day_plot, width = 10, height = 6, dpi = 600)


#### JPM ####
# JP data preparation
jp_vol_forecast_10day_realised <- 100 * exp(read.csv('/Users/raphaelravinet/Code/BSE/Thesis/Results/Forecast_results_final/JPM.csv/realised_garch_horizon_10.csv')) # 10-day ahead
jp_vol_forecast_10day_har <- 100 * exp(read.csv('/Users/raphaelravinet/Code/BSE/Thesis/Results/Forecast_results_final/JPM.csv/realised_har_garch_horizon_10.csv')) # 10-day ahead

combined_jp_10day <- data.frame(
  Day = as.Date(all_results$JPM.csv$test_data$Day[1:(length(all_results$JPM.csv$test_data$Day) - 8)], format = "%Y-%m-%d"),
  Adjusted_RM = 100 * all_results$JPM.csv$test_data$RM_adj[1:(length(all_results$JPM.csv$test_data$Day) - 8)],
  Realised_Garch_Volatilty_Forecast_10day = jp_vol_forecast_10day_realised$forecast_data,
  HAR_Garch_Volatility_Forecast_10day = jp_vol_forecast_10day_har$forecast_data
)

jp_date_range <- range(combined_jp_10day$Day, na.rm = TRUE)

# JP plot
jp_10day_plot <- 
  ggplot(combined_jp_10day, aes(x = Day)) +
  geom_line(aes(y = Adjusted_RM, color = "Adjusted RM"), size = 0.5) +
  geom_line(aes(y = Realised_Garch_Volatilty_Forecast_10day, color = "Realised GARCH"), size = 0.35, na.rm = TRUE) +
  geom_line(aes(y = HAR_Garch_Volatility_Forecast_10day, color = "Realised HAR-GARCH"), size = 0.4, na.rm = TRUE) +
  labs(x = "JP", y = "", color = NULL) +
  scale_color_manual(values = c("Adjusted RM" = "black", "Realised GARCH" = "blue", "Realised HAR-GARCH" = "red")) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 1.25), breaks = seq(0, 3, by = 0.25)) +
  scale_x_date(limits = jp_date_range, date_labels = "%m/%Y", date_breaks = "6 months") +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.ticks.length = unit(0.2, "cm"),  # Add tick marks
    axis.ticks = element_line(color = "black"),  # Color of tick marks
    axis.line = element_blank(),  # Remove axis lines
    panel.border = element_rect(color = "black", fill = NA, size = 0.6)  # Add border with uniform thickness
  )
print(jp_10day_plot)
ggsave("JP_10day_plot.png", plot = jp_10day_plot, width = 10, height = 6, dpi = 600)



# DIS data preparation
dis_vol_forecast_10day_realised <- 100 * exp(read.csv('/Users/raphaelravinet/Code/BSE/Thesis/Results/Forecast_results_final/DIS.csv/realised_garch_horizon_10.csv')) # 10-day ahead
dis_vol_forecast_10day_har <- 100 * exp(read.csv('/Users/raphaelravinet/Code/BSE/Thesis/Results/Forecast_results_final/DIS.csv/realised_har_garch_horizon_10.csv')) # 10-day ahead

combined_dis_10day <- data.frame(
  Day = as.Date(all_results$DIS.csv$test_data$Day[1:(length(all_results$DIS.csv$test_data$Day) - 9)], format = "%Y-%m-%d"),
  Adjusted_RM = 100 * all_results$DIS.csv$test_data$RM_adj[1:(length(all_results$DIS.csv$test_data$Day) - 9)],
  Realised_Garch_Volatilty_Forecast_10day = dis_vol_forecast_10day_realised$forecast_data,
  HAR_Garch_Volatility_Forecast_10day = dis_vol_forecast_10day_har$forecast_data
)

dis_date_range <- range(combined_dis_10day$Day, na.rm = TRUE)

# DIS plot
dis_10day_plot <- 
  ggplot(combined_dis_10day, aes(x = Day)) +
  geom_line(aes(y = Adjusted_RM, color = "Adjusted RM"), size = 0.5) +
  geom_line(aes(y = Realised_Garch_Volatilty_Forecast_10day, color = "Realised GARCH"), size = 0.4, na.rm = TRUE) +
  geom_line(aes(y = HAR_Garch_Volatility_Forecast_10day, color = "Realised HAR-GARCH"), size = 0.4, na.rm = TRUE) +
  labs(x = "DIS", y = "", color = NULL) +
  scale_color_manual(values = c("Adjusted RM" = "black", "Realised GARCH" = "blue", "Realised HAR-GARCH" = "red")) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 1.5), breaks = seq(0, 3, by = 0.5)) +
  scale_x_date(limits = dis_date_range, date_labels = "%m/%Y", date_breaks = "6 months") +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.ticks.length = unit(0.2, "cm"),  # Add tick marks
    axis.ticks = element_line(color = "black"),  # Color of tick marks
    axis.line = element_blank(),  # Remove axis lines
    panel.border = element_rect(color = "black", fill = NA, size = 0.6)  # Add border with uniform thickness
  )
print(dis_10day_plot)
ggsave("DIS_10day_plot.png", plot = dis_10day_plot, width = 10, height = 6, dpi = 600)



# BA data preparation
ba_vol_forecast_10day_realised <- 100 * exp(read.csv('/Users/raphaelravinet/Code/BSE/Thesis/Results/Forecast_results_final/BA.csv/realised_garch_horizon_10.csv')) # 10-day ahead
ba_vol_forecast_10day_har <- 100 * exp(read.csv('/Users/raphaelravinet/Code/BSE/Thesis/Results/Forecast_results_final/BA.csv/realised_har_garch_horizon_10.csv')) # 10-day ahead

combined_ba_10day <- data.frame(
  Day = as.Date(all_results$BA.csv$test_data$Day[1:(length(all_results$BA.csv$test_data$Day) - 9)], format = "%Y-%m-%d"),
  Adjusted_RM = 100 * all_results$BA.csv$test_data$RM_adj[1:(length(all_results$BA.csv$test_data$Day) - 9)],
  Realised_Garch_Volatilty_Forecast_10day = ba_vol_forecast_10day_realised$forecast_data,
  HAR_Garch_Volatility_Forecast_10day = ba_vol_forecast_10day_har$forecast_data
)

ba_date_range <- range(combined_ba_10day$Day, na.rm = TRUE)

# BA plot
ba_10day_plot <- 
  ggplot(combined_ba_10day, aes(x = Day)) +
  geom_line(aes(y = Adjusted_RM, color = "Adjusted RM"), size = 0.5) +
  geom_line(aes(y = Realised_Garch_Volatilty_Forecast_10day, color = "Realised GARCH"), size = 0.4, na.rm = TRUE) +
  geom_line(aes(y = HAR_Garch_Volatility_Forecast_10day, color = "Realised HAR-GARCH"), size = 0.4, na.rm = TRUE) +
  labs(x = "BA", y = "", color = NULL) +
  scale_color_manual(values = c("Adjusted RM" = "black", "Realised GARCH" = "blue", "Realised HAR-GARCH" = "red")) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 1.5), breaks = seq(0, 3, by = 0.5)) +
  scale_x_date(limits = ba_date_range, date_labels = "%m/%Y", date_breaks = "6 months") +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.ticks.length = unit(0.2, "cm"),  # Add tick marks
    axis.ticks = element_line(color = "black"),  # Color of tick marks
    axis.line = element_blank(),  # Remove axis lines
    panel.border = element_rect(color = "black", fill = NA, size = 0.6)  # Add border with uniform thickness
  )
print(ba_10day_plot)
ggsave("BA_10day_plot.png", plot = ba_10day_plot, width = 10, height = 6, dpi = 600)


# CVX data preparation
cvx_vol_forecast_10day_realised <- 100 * exp(read.csv('/Users/raphaelravinet/Code/BSE/Thesis/Results/Forecast_results_final/CVX.csv/realised_garch_horizon_10.csv')) # 10-day ahead
cvx_vol_forecast_10day_har <- 100 * exp(read.csv('/Users/raphaelravinet/Code/BSE/Thesis/Results/Forecast_results_final/CVX.csv/realised_har_garch_horizon_10.csv')) # 10-day ahead

combined_cvx_10day <- data.frame(
  Day = as.Date(all_results$CVX.csv$test_data$Day[1:(length(all_results$CVX.csv$test_data$Day) - 9)], format = "%Y-%m-%d"),
  Adjusted_RM = 100 * all_results$CVX.csv$test_data$RM_adj[1:(length(all_results$CVX.csv$test_data$Day) - 9)],
  Realised_Garch_Volatilty_Forecast_10day = cvx_vol_forecast_10day_realised$forecast_data,
  HAR_Garch_Volatility_Forecast_10day = cvx_vol_forecast_10day_har$forecast_data
)

cvx_date_range <- range(combined_cvx_10day$Day, na.rm = TRUE)

# CVX plot
cvx_10day_plot <- 
  ggplot(combined_cvx_10day, aes(x = Day)) +
  geom_line(aes(y = Adjusted_RM, color = "Adjusted RM"), size = 0.5) +
  geom_line(aes(y = Realised_Garch_Volatilty_Forecast_10day, color = "Realised GARCH"), size = 0.4, na.rm = TRUE) +
  geom_line(aes(y = HAR_Garch_Volatility_Forecast_10day, color = "Realised HAR-GARCH"), size = 0.4, na.rm = TRUE) +
  labs(x = "CVX", y = "", color = NULL) +
  scale_color_manual(values = c("Adjusted RM" = "black", "Realised GARCH" = "blue", "Realised HAR-GARCH" = "red")) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 1.5), breaks = seq(0, 3, by = 0.5)) +
  scale_x_date(limits = cvx_date_range, date_labels = "%m/%Y", date_breaks = "6 months") +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.ticks.length = unit(0.2, "cm"),  # Add tick marks
    axis.ticks = element_line(color = "black"),  # Color of tick marks
    axis.line = element_blank(),  # Remove axis lines
    panel.border = element_rect(color = "black", fill = NA, size = 0.6)  # Add border with uniform thickness
  )
print(cvx_10day_plot)
ggsave("CVX_10day_plot.png", plot = cvx_10day_plot, width = 10, height = 6, dpi = 600)

