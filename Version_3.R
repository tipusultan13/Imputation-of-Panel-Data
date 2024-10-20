fill_missing_income_lstm <- function(data) {
  
  # Step 1: Remove rows with missing 'Income' and save the removed rows
  missing_rows <- which(is.na(data$Income))
  complete_data <- data[!is.na(data$Income), ]
  
  # Normalize numeric features (Age and Income) for the LSTM model
  age_mean <- mean(complete_data$Age, na.rm = TRUE)
  age_sd <- sd(complete_data$Age, na.rm = TRUE)
  
  income_mean <- mean(complete_data$Income, na.rm = TRUE)
  income_sd <- sd(complete_data$Income, na.rm = TRUE)
  
  complete_data$Age <- (complete_data$Age - age_mean) / age_sd
  complete_data$Income <- (complete_data$Income - income_mean) / income_sd
  
  # Step 2: Prepare data for LSTM training
  # Convert categorical columns to one-hot encoding (ID, Year, Education)
  complete_data <- complete_data %>%
    mutate(ID = as.numeric(factor(ID)),
           Year = as.numeric(factor(Year)),
           Education = as.numeric(factor(Education)))
  
  # Prepare training input (excluding the target column 'Income')
  X_train <- as.matrix(complete_data %>% select(ID, Year, Education, Age))
  y_train <- complete_data$Income
  
  # Reshape the data to 3D for LSTM: [samples, timesteps=1, features=4]
  X_train <- array(X_train, dim = c(nrow(X_train), 1, ncol(X_train)))
  
  # Step 3: Build and train the LSTM model with `return_sequences=TRUE`
  model <- keras_model_sequential() %>%
    layer_lstm(units = 50, input_shape = c(1, 4), return_sequences = FALSE) %>%
    layer_dense(units = 1)
  
  model %>% compile(
    loss = 'mean_squared_error',
    optimizer = optimizer_adam()
  )
  
  # Train the model
  model %>% fit(X_train, y_train, epochs = 50, batch_size = 32, verbose = 1)
  
  # Step 4: Predict the missing values
  # Prepare the data for the missing rows
  data_missing <- data[missing_rows, ]
  
  data_missing <- data_missing %>%
    mutate(ID = as.numeric(factor(ID)),
           Year = as.numeric(factor(Year)),
           Education = as.numeric(factor(Education)),
           Age = (Age - age_mean) / age_sd)  # Normalize Age for missing data
  
  X_missing <- as.matrix(data_missing %>% select(ID, Year, Education, Age))
  X_missing <- array(X_missing, dim = c(nrow(X_missing), 1, ncol(X_missing)))
  
  # Predict the missing values
  predicted_income <- model %>% predict(X_missing)
  
  # De-normalize the predicted income
  predicted_income <- predicted_income * income_sd + income_mean
  
  # Fill the missing values with the predicted income
  data$Income[missing_rows] <- predicted_income
  
  # Return the complete dataset
  return(data)
}
lstm_bal_mcar_50 <- fill_missing_income_lstm(balanced_panel_data_mcar_50)
summary(lstm_bal_mcar_50)


