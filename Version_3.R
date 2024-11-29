library(mice)
library(dplyr)

Data_Imputation_mice <- function(data, id_col = "ID", target_col = "Income", 
                                 lagged_cols = c("EmploymentTypes", "MaritalStatus", "Sex"), 
                                 squared_cols = c("Age"), m = 3, maxit = 20, method = "pmm") {
  
  # Ensure 'ID' and 'Year' are factors for proper lagging
  data[[id_col]] <- as.factor(data[[id_col]])
  data$Year <- as.numeric(data$Year)  # Ensure 'Year' is numeric
  
  # Create lagged variables
  for (col in lagged_cols) {
    lagged_name <- paste0("lag_", col)
    data <- data %>%
      group_by(!!sym(id_col)) %>%
      arrange(Year, .by_group = TRUE) %>%
      mutate(!!sym(lagged_name) := lag(!!sym(col))) %>%
      ungroup()
  }
  
  # Create squared variables
  for (col in squared_cols) {
    squared_name <- paste0(col, "_squared")
    data[[squared_name]] <- data[[col]]^2
  }
  
  # Remove rows where lagged variables are NA (first year for each ID)
  data <- data %>% filter(!is.na(lag_EmploymentTypes) & !is.na(lag_MaritalStatus) & !is.na(lag_Sex))
  
  # Create the predictor matrix
  pred_matrix <- make.predictorMatrix(data)
  
  # Exclude the 'ID' column from being a predictor
  pred_matrix[, id_col] <- 0
  
  # Only allow predictors to predict 'Income'
  pred_matrix[, ] <- 0
  pred_matrix[colnames(pred_matrix) %in% c("lag_EmploymentTypes", "lag_MaritalStatus", 
                                           "lag_Sex", "Age_squared"), target_col] <- 1
  
  # Impute missing values
  imputed_data <- mice(data, m = m, maxit = maxit, method = method, predictorMatrix = pred_matrix)
  
  # Get completed datasets
  complete_data_list <- lapply(1:m, function(i) complete(imputed_data, action = i))
  
  # Retain only original columns
  original_cols <- c("ID", "Year", "Age", "EmploymentTypes", "Income", "MaritalStatus", 
                     "EmploymentHours", "Education", "Sex")
  complete_data_list <- lapply(complete_data_list, function(df) df[, original_cols])
  
  return(complete_data_list)
}

mice_bal_mcar_50 <- Data_Imputation_mice(balanced_panel_data_mcar_50)


########################
########################

Data_Imputation_mice <- function(data, id_col = "ID", target_col = "Income", 
                                 lagged_cols = c("EmploymentTypes", "MaritalStatus", "Sex", "Income"), 
                                 m = 5, maxit = 10, method = "pmm") {
  
  # Ensure 'ID' and 'Year' are factors for proper lagging
  data[[id_col]] <- as.factor(data[[id_col]])
  data$Year <- as.numeric(data$Year)  # Ensure 'Year' is numeric
  
  # Create lagged variables
  for (col in lagged_cols) {
    lagged_name <- paste0("lag_", col)
    data <- data %>%
      group_by(!!sym(id_col)) %>%
      arrange(Year, .by_group = TRUE) %>%
      mutate(!!sym(lagged_name) := lag(!!sym(col))) %>%
      ungroup()
  }
  
  # Remove rows where lagged variables are NA (first year for each ID)
  lagged_vars <- paste0("lag_", lagged_cols)
  data <- data %>% filter(rowSums(is.na(select(., all_of(lagged_vars)))) == 0)
  
  # Create the predictor matrix
  pred_matrix <- make.predictorMatrix(data)
  
  # Exclude the 'ID' column from being a predictor
  pred_matrix[, id_col] <- 0
  
  # Only allow predictors to predict 'Income'
  pred_matrix[, ] <- 0
  pred_matrix[colnames(pred_matrix) %in% c("lag_EmploymentTypes", "lag_MaritalStatus", 
                                           "lag_Sex", "lag_Income", "Age"), target_col] <- 1
  
  # Impute missing values
  imputed_data <- mice(data, m = m, maxit = maxit, method = method, predictorMatrix = pred_matrix)
  
  # Get completed datasets
  complete_data_list <- lapply(1:m, function(i) complete(imputed_data, action = i))
  
  # Retain only original columns
  original_cols <- c("ID", "Year", "Age", "EmploymentTypes", "Income", "MaritalStatus", 
                     "EmploymentHours", "Education", "Sex")
  complete_data_list <- lapply(complete_data_list, function(df) df[, original_cols])
  
  return(complete_data_list)
}

mice_bal_mcar_50 <- Data_Imputation_mice(balanced_panel_data_mcar_50)








