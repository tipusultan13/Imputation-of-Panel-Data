library(keras)
library(dplyr)
library(plm)
packageVersion("keras")


# Function to impute the datasets
Data_Imputation_LSTM <- function(data) {
  
  # Temporarily replace the missing values with mean
  DataTemp <- data %>%
    mutate(Income = ifelse(is.na(Income), mean(Income, na.rm = TRUE), Income))
  
  # Convert 'Education' to numeric to make it more compatible with the model and including other numeric columns
  DataEncoded <- DataTemp %>%
    mutate(Education = as.factor(Education)) %>%
    select(Age, Income, Education)
  
  DataEncoded <- as.data.frame(model.matrix(~ Education - 1, data = DataEncoded))
  
  InputData <- cbind(Age = DataTemp$Age, Income = DataTemp$Income, DataEncoded)
  
  CompleteCase <- InputData[!is.na(InputData$Income), ]
  
  TrainArray <- array(as.matrix(CompleteCase), dim = c(nrow(CompleteCase), 1, ncol(CompleteCase)))   # Because 3D array is more suitable for LSTM
  
  # define and compile the LSTM model
  model <- keras_model_sequential() %>%
    layer_lstm(units = 50, input_shape = c(1, ncol(CompleteCase)), return_sequences = FALSE) %>%
    layer_dense(units = 1)
  
  model %>% compile(
    loss = "mean_squared_error",
    optimizer = optimizer_adam()
  )
  
  # Train the model
  model %>% fit(
    x = TrainArray,
    y = CompleteCase$Income,  # Target is the Income column
    epochs = 50,
    batch_size = 32
  )
  
  # Predict and replacing the missing values
  MissingCase <- InputData[is.na(data$Income), ]
  if (nrow(MissingCase) > 0) {
    MissingArray <- array(as.matrix(MissingCase), dim = c(nrow(MissingCase), 1, ncol(MissingCase)))
    PredictedValue <- model %>% predict(MissingArray)
    data$Income[is.na(data$Income)] <- PredictedValue
  }
  
  return(data)
}

# Apply the function to the dataset
lstm_bal_mcar_50 <- Data_Imputation_LSTM(balanced_panel_data_mcar_50)
lstm_bal_mcar_30 <- Data_Imputation_LSTM(balanced_panel_data_mcar_30)
lstm_bal_mcar_10 <- Data_Imputation_LSTM(balanced_panel_data_mcar_10)

lstm_bal_mar_50 <- Data_Imputation_LSTM(balanced_panel_data_mar_50)
lstm_bal_mar_30 <- Data_Imputation_LSTM(balanced_panel_data_mar_30)
lstm_bal_mar_10 <- Data_Imputation_LSTM(balanced_panel_data_mar_10)

lstm_bal_mnar_50 <- Data_Imputation_LSTM(balanced_panel_data_mnar_50)
lstm_bal_mnar_30 <- Data_Imputation_LSTM(balanced_panel_data_mnar_30)
lstm_bal_mnar_10 <- Data_Imputation_LSTM(balanced_panel_data_mnar_10)

lstm_unbal_mcar_50 <- Data_Imputation_LSTM(unbalanced_panel_data_mcar_50)
lstm_unbal_mcar_30 <- Data_Imputation_LSTM(unbalanced_panel_data_mcar_30)
lstm_unbal_mcar_10 <- Data_Imputation_LSTM(unbalanced_panel_data_mcar_10)

lstm_unbal_mar_50 <- Data_Imputation_LSTM(unbalanced_panel_data_mar_50)
lstm_unbal_mar_30 <- Data_Imputation_LSTM(unbalanced_panel_data_mar_30)
lstm_unbal_mar_10 <- Data_Imputation_LSTM(unbalanced_panel_data_mar_10)

lstm_unbal_mnar_50 <- Data_Imputation_LSTM(unbalanced_panel_data_mnar_50)
lstm_unbal_mnar_30 <- Data_Imputation_LSTM(unbalanced_panel_data_mnar_30)
lstm_unbal_mnar_10 <- Data_Imputation_LSTM(unbalanced_panel_data_mnar_10)


StartTime_LSTM <- Sys.time()  # Starting time

# Function to generate coefficients and intercepts
# Function to perform analysis on imputed data
Analyze_Amelia <- function(data) {
  
  # Step 1: Imputation and extraction of data
  amelia_fit <- Data_Imputation_Amelia(data)
  imputed_list <- amelia_fit$imputations
  
  # Step 2: Perform Breusch-Pagan test to check for a panel effect
  breusch_pagan_results <- lapply(imputed_list, function(x) {
    pdata <- pdata.frame(x, index = c("ID", "Year"))
    bp_test <- plmtest(plm(Income ~ Year + Education + Age, data = pdata, model = "pooling"), type = "bp")
    return(bp_test$p.value)
  })
  
  # Step 3: Based on Breusch-Pagan test, perform the appropriate regression
  model_list <- lapply(1:length(imputed_list), function(i) {
    pdata <- pdata.frame(imputed_list[[i]], index = c("ID", "Year"))
    if (breusch_pagan_results[[i]] > 0.05) {
      # Pooled OLS for no panel effect
      return(plm(Income ~ Year + Education + Age, data = pdata, model = "pooling"))
    } else {
      # Hausman test if panel effect exists
      random_model <- plm(Income ~ Year + Education + Age, data = pdata, model = "random")
      fixed_model <- plm(Income ~ Year + Education + Age, data = pdata, model = "within")
      
      hausman_test <- phtest(fixed_model, random_model) # Hausman test
      
      if (hausman_test$p.value <= 0.05) {
        print("Fixed Effects")
        return(fixed_model) # Fixed Effect model if correlation exists
      } else {
        print("Random Effects")
        return(random_model) # Random Effect model if correlation does not exists
      }
    }
  })
  
  # Step 4: Pool and return the coefficients
  pooled_results <- testEstimates(model_list)
  return(pooled_results)
}

# Apply the function to each dataset and store results
analyze_lstm_bal_mcar_50 <- Analyze_LSTM(balanced_panel_data_mcar_50)