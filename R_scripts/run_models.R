###This script fits our models and run the forecast function for every dataset we have####

data_dir <- "/Users/raphaelravinet/Code/BSE/Thesis/final_datasets copy 2/final_run"
data_dir2 <- "/Users/raphaelravinet/Code/BSE/Thesis/final_datasets_iv/testing_i " ##using this as a test with only 2 datasets

##Fitting all our models###
apply_models_to_all_datasets <- function(data_dir, model_types, param_names, split = 0.75) {
  file_list <- list.files(data_dir, full.names = TRUE)
  all_results <- list()
  
  for (file in file_list) {
    data <- read.csv(file)  # Assuming the datasets are in CSV format
    
    # Split the data into train and test sets
    split_index <- floor(split * nrow(data))
    train_data <- data[1:split_index, ]
    test_data <- data[(split_index + 1):nrow(data), ]
    
    results <- fit_models_and_get_parameters(train_data, model_types, param_names)
    results$train_data <- train_data
    results$test_data <- test_data
    results$full_data <- data
    all_results[[basename(file)]] <- results
  }
  
  return(all_results = all_results)
}


calculate_in_sample_VaR_all <- function(model_types, results, forecast_horizons = c(1, 10), alpha_0.05 = 0.05, alpha_0.01 = 0.01) {
  all_in_sample_VaR <- list()
  
  for (dataset_name in names(results)) {
    dataset_results <- results[[dataset_name]]
    
    for (model_type in model_types) {
      for (forecast_horizon in forecast_horizons) {
        print(paste("Processing Dataset:", dataset_name, "Model:", model_type, "Forecast Horizon:", forecast_horizon))
        in_sample_VaR <- tryCatch({
          in_sample_all_models_VaR(model_type, forecast_horizon, dataset_results, alpha_0.05, alpha_0.01)
        }, error = function(e) {
          print(paste("Error in model", model_type, "for dataset", dataset_name, ":", e$message))
          return(NULL)
        })
        
        if (!is.null(in_sample_VaR)) {
          if (!is.null(all_in_sample_VaR[[dataset_name]])) {
            if (!is.null(all_in_sample_VaR[[dataset_name]][[model_type]])) {
              all_in_sample_VaR[[dataset_name]][[model_type]][[paste0("VaR_", forecast_horizon, "_days")]] <- in_sample_VaR
            } else {
              all_in_sample_VaR[[dataset_name]][[model_type]] <- list()
              all_in_sample_VaR[[dataset_name]][[model_type]][[paste0("VaR_", forecast_horizon, "_days")]] <- in_sample_VaR
            }
          } else {
            all_in_sample_VaR[[dataset_name]] <- list()
            all_in_sample_VaR[[dataset_name]][[model_type]] <- list()
            all_in_sample_VaR[[dataset_name]][[model_type]][[paste0("VaR_", forecast_horizon, "_days")]] <- in_sample_VaR
          }
        }
      }
    }
  }
  
  return(all_in_sample_VaR)
}

#FORECASTING###
apply_forecasts_to_all_datasets <- function(results, model_types, forecast_horizons = c(1,5,10,20)) {
  all_forecasts <- list()
  
  for (dataset_name in names(results)) {
    print(paste("Processing dataset:", dataset_name))
    train_data <- results[[dataset_name]]$train_data
    test_data <- results[[dataset_name]]$test_data
    dataset_forecasts <- list()
    
    for (model_type in model_types) {
      params <- results[[dataset_name]]$fitted_models[[model_type]]$optimal_params
      
      for (forecast_horizon in forecast_horizons) {
        print(paste("Processing forecast horizon:", forecast_horizon, "for model:", model_type))
        
        forecasted_values <- tryCatch({
          if (model_type == "realised_har_iv_garch") {
            har_garch_var_forecast(params, model_type, forecast_horizon, results[[dataset_name]], train_data, test_data)
          } else if (model_type == "realised_garch") {
            realised_garch_var_forecast(params, model_type, forecast_horizon, results[[dataset_name]], train_data, test_data)
          } else {
            next
          }
        }, error = function(e) {
          print(paste("Error in forecasting for model", model_type, "with horizon", forecast_horizon, ":", e$message))
          return(NULL)
        })
        
        if (!is.null(forecasted_values)) {
          dataset_forecasts[[paste0(model_type, "_horizon_", forecast_horizon)]] <- forecasted_values
        }
      }
    }
    
    all_forecasts[[dataset_name]] <- dataset_forecasts
  }
  
  return(all_forecasts)
}

##THIS ONE HAS VAR AND ES###
apply_forecasts_to_all_datasets_2 <- function(results, model_types, forecast_horizons = c(1, 10), alpha_0.05 = 0.05, alpha_0.01 = 0.01, p = 8) {
  all_forecasts <- list()
  
  for (dataset_name in names(results)) {
    print(paste("Processing dataset:", dataset_name))
    train_data <- results[[dataset_name]]$train_data
    test_data <- results[[dataset_name]]$test_data
    dataset_forecasts <- list()
    
    for (model_type in model_types) {
      params <- results[[dataset_name]]$fitted_models[[model_type]]$optimal_params
      
      for (forecast_horizon in forecast_horizons) {
        print(paste("Processing forecast horizon:", forecast_horizon, "for model:", model_type))
        
        forecasted_values <- tryCatch({
          if (model_type == "realised_har_garch") {
            har_garch_var_forecast(params, model_type, forecast_horizon, results[[dataset_name]], train_data, test_data, alpha_0.05, alpha_0.01, p)
          } else if (model_type == "realised_garch") {
            realised_garch_var_forecast(params, model_type, forecast_horizon, results[[dataset_name]], train_data, test_data, alpha_0.05, alpha_0.01, p)
          } else {
            next
          }
        }, error = function(e) {
          print(paste("Error in forecasting for model", model_type, "with horizon", forecast_horizon, ":", e$message))
          return(NULL)
        })
        
        if (!is.null(forecasted_values)) {
          dataset_forecasts[[paste0(model_type, "_horizon_", forecast_horizon)]] <- forecasted_values
        }
      }
    }
    
    all_forecasts[[dataset_name]] <- dataset_forecasts
  }
  
  return(all_forecasts)
}

all_results <- apply_models_to_all_datasets(data_dir, model_types, param_names)
in_sample_VaR_results_2 <- calculate_in_sample_VaR_all(model_types, all_results, forecast_horizons_2)
all_forecasts <- apply_forecasts_to_all_datasets(all_results, model_types)
all_results$AAPL.csv$param_table
###testing##
apply_forecasts_to_all_datasets <- function(results, model_types, rolling_forecast_horizons = c(1,10), var_es_horizons = c(1, 10), alpha_0.05 = 0.05, alpha_0.01 = 0.01, p = 8) {
  all_forecasts <- list()
  
  for (dataset_name in names(results)) {
    print(paste("Processing dataset:", dataset_name))
    train_data <- results[[dataset_name]]$train_data
    test_data <- results[[dataset_name]]$test_data
    dataset_forecasts <- list()
    
    for (model_type in model_types) {
      params <- results[[dataset_name]]$fitted_models[[model_type]]$optimal_params
      
      # Rolling forecasts for 1, 5, 10, and 20 days
      for (forecast_horizon in rolling_forecast_horizons) {
        print(paste("Processing rolling forecast horizon:", forecast_horizon, "for model:", model_type))
        
        forecasted_values <- tryCatch({
          if (model_type == "realised_har_garch") {
            har_garch_var_forecast(params, model_type, forecast_horizon, results[[dataset_name]], train_data, test_data, alpha_0.05, alpha_0.01, p)
          } else if (model_type == "realised_garch") {
            realised_garch_var_forecast(params, model_type, forecast_horizon, results[[dataset_name]], train_data, test_data, alpha_0.05, alpha_0.01, p)
          } else {
            next
          }
        }, error = function(e) {
          print(paste("Error in forecasting for model", model_type, "with horizon", forecast_horizon, ":", e$message))
          return(NULL)
        })
        
        if (!is.null(forecasted_values)) {
          dataset_forecasts[[paste0(model_type, "_rolling_horizon_", forecast_horizon)]] <- list(rolling_forecasts = forecasted_values$rolling_forecasts)
        }
      }
      
      # VaR and ES calculations for 1 and 10 days
      for (forecast_horizon in var_es_horizons) {
        print(paste("Processing VaR/ES forecast horizon:", forecast_horizon, "for model:", model_type))
        
        forecasted_values <- tryCatch({
          if (model_type == "realised_har_garch") {
            har_garch_var_forecast(params, model_type, forecast_horizon, results[[dataset_name]], train_data, test_data, alpha_0.05, alpha_0.01, p)
          } else if (model_type == "realised_garch") {
            realised_garch_var_forecast(params, model_type, forecast_horizon, results[[dataset_name]], train_data, test_data, alpha_0.05, alpha_0.01, p)
          } else {
            next
          }
        }, error = function(e) {
          print(paste("Error in forecasting for model", model_type, "with horizon", forecast_horizon, ":", e$message))
          return(NULL)
        })
        
        if (!is.null(forecasted_values)) {
          dataset_forecasts[[paste0(model_type, "_var_es_horizon_", forecast_horizon)]] <- list(VaR_0.05 = forecasted_values$VaR_0.05, VaR_0.01 = forecasted_values$VaR_0.01, ES_0.05 = forecasted_values$ES_0.05, ES_0.01 = forecasted_values$ES_0.01)
        }
      }
    }
    
    all_forecasts[[dataset_name]] <- dataset_forecasts
  }
  
  return(all_forecasts)
}


all_stock_names <- names(all_results)
filtered_stock_names <- all_stock_names[all_stock_names != "SPY.csv"]
filtered_all_results <- all_results[filtered_stock_names] # removing spy for now


all_forecasts <- apply_forecasts_to_all_datasets_2(filtered_all_results, model_types, forecast_horizons = c(1, 10))
  

