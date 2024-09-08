Data_Imputation_mitml_Bal <- function(panel_data) {
  # Load required libraries
  library(mitml)
  library(plm)
  
  # Step 1: Prepare the data by selecting relevant columns
  selected_data <- panel_data[c("ID", "Year", "Education", "Age", "IndividualIncome")]
  
  # Define the type vector and assign column names
  type <- c(0, -2, 3, 3, 1)
  names(type) <- colnames(selected_data)
  
  # Step 2: Impute missing data
  imputed_data <- panImpute(selected_data, type = type, n.burn = 1000, n.iter = 100, m = 3)
  
  # Step 3: Extract imputed datasets
  imputed_list <- mitmlComplete(imputed_data, print = "all")
  
  # Initialize a list to store the models
  model_list <- list()
  
  # Step 4: Fit the model to each imputed dataset
  for (i in 1:length(imputed_list)) {
    imputed_dataset <- imputed_list[[i]]
    
    # Ensure the data is in the appropriate panel data format
    pdata <- pdata.frame(imputed_dataset, index = c("ID", "Year"))
    
    # Fit a fixed-effects model using the "within" method
    model <- plm(IndividualIncome ~ Education + Age, data = pdata, model = "within")
    
    # Store the model
    model_list[[i]] <- model
    
    # Print the summary of the model to see the coefficients
    print(summary(model))
  }
  
  return(model_list)
}


# Apply the function to the panel data
Data_Imputation_mitml_Bal(balanced_panel_data_mcar_50)

Data_Imputation_mitml_Bal(balanced_panel_data_mcar_30)
Data_Imputation_mitml_Bal(balanced_panel_data_mcar_10)
Data_Imputation_mitml_Bal(balanced_panel_data_mar_50)
Data_Imputation_mitml_Bal(balanced_panel_data_mar_30)
Data_Imputation_mitml_Bal(balanced_panel_data_mar_10)
Data_Imputation_mitml_Bal(balanced_panel_data_mnar_50)
Data_Imputation_mitml_Bal(balanced_panel_data_mnar_30)
Data_Imputation_mitml_Bal(balanced_panel_data_mnar_10)

