library(keras)
library(dplyr)

# Function to impute the data using LSTM
Data_Imputation_LSTM <- function(data) {
  
  # Temporarily filling the missing values in the Income column with mean
  temp_data <- data %>%
    mutate(Income = ifelse(is.na(Income), mean(Income, na.rm = TRUE), Income))
  
  # Convert 'Education' to numeric using one-hot encoding
  encoded_data <- temp_data %>%
    mutate(Education = as.factor(Education)) %>%
    select(Age, Income, Education)
  
  encoded_data <- as.data.frame(model.matrix(~ Education - 1, data = encoded_data))
  
  # Include other numeric columns (Age and Income)
  input_data <- cbind(Age = temp_data$Age, Income = temp_data$Income, encoded_data)
  
  # Ensure no NA values in the Income column during training
  complete_cases <- input_data[!is.na(input_data$Income), ]
  
  # Convert the data to a 3D array, suitable for LSTM
  train_array <- array(as.matrix(complete_cases), dim = c(nrow(complete_cases), 1, ncol(complete_cases)))
  
  # Define LSTM model
  model <- keras_model_sequential() %>%
    layer_lstm(units = 50, input_shape = c(1, ncol(complete_cases)), return_sequences = FALSE) %>%
    layer_dense(units = 1)
  
  # Compile the model
  model %>% compile(
    loss = "mean_squared_error",
    optimizer = optimizer_adam()
  )
  
  # Fit the model (training)
  model %>% fit(
    x = train_array,
    y = complete_cases$Income,  # Target is the Income column
    epochs = 50,
    batch_size = 32
  )
  
  # Handle missing cases
  missing_cases <- input_data[is.na(data$Income), ]
  if (nrow(missing_cases) > 0) {
    # Predict missing values
    missing_array <- array(as.matrix(missing_cases), dim = c(nrow(missing_cases), 1, ncol(missing_cases)))
    predicted_values <- model %>% predict(missing_array)
    
    # Replace missing values in the original data
    data$Income[is.na(data$Income)] <- predicted_values
  }
  
  return(data)
}

# Apply the function to the dataset
lstm_bal_mcar_50 <- Data_Imputation_LSTM(balanced_panel_data_mcar_50)

