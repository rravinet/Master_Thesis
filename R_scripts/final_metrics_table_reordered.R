library(ggplot2)
library(readr)
library(dplyr)
library(gridExtra)

# Function to plot data for a given stock
plot_stock_data <- function(stock, data_dir, output_dir) {
  # Read the data
  data_path <- file.path(data_dir, paste0(stock, ".csv"))
  data <- read_csv(data_path)
  
  # Multiply necessary columns by 100
  data <- data %>%
    mutate(returns = returns * 100,
           RM = RM * 100)
  
  # Plot Return
  p1 <- ggplot(data, aes(x = Day, y = returns)) +
    geom_line() +
    labs(title = "Return", x = "") +
    theme(axis.title.y = element_blank(),
          plot.title = element_text(hjust = 0.5, face = "bold"))
  
  # Plot Realized Variance
  p2 <- ggplot(data, aes(x = Day, y = RM)) +
    geom_line() +
    labs(title = "Realized Variance", x = "Date") +
    theme(axis.title.y = element_blank(),
          plot.title = element_text(hjust = 0.5, face = "bold"))
  
  # Combine the plots
  combined_plot <- grid.arrange(p1, p2, ncol = 1)
  
  # Save the combined plot as an image
  output_path <- file.path(output_dir, paste0(stock, "_plots.png"))
  ggsave(output_path, combined_plot, width = 10, height = 8)
}

# Directory containing the datasets
data_dir <- "/Users/raphaelravinet/Code/BSE/Thesis/final_datasets_iv"

# Directory to save the output plots
output_dir <- "/Users/raphaelravinet/Code/BSE/Thesis/plots"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# List of stocks to analyze
stocks <- c("AAPL", "SPY", "IBM", "PG", "WMT", "DIS", "JPM")

for (stock in stocks) {
  plot_stock_data(stock, data_dir, output_dir)
}
