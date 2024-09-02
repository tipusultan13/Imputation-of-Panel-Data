library(mitml)

Data_Imputation_mitml_Bal <- function(panel_data) {
  # Select the required columns
  selected_data <- panel_data[c("ID", "Year", "Education", "Age", "IndividualIncome")]
  type <- c(0, -2, 2, 2, 1) 
  names(type) <- colnames(selected_data)
  
  # Impute missing data using the panImpute function
  imputed_data <- panImpute(selected_data, type = type, n.burn = 1000, n.iter = 100, m = 3)
  
  # Complete the imputed datasets
  imputed_list <- mitmlComplete(imputed_data)
  
  # Calculate the average IndividualIncome across all imputed datasets
  imputed_averages <- lapply(imputed_list, function(df) df[,"IndividualIncome"])
  average_income <- Reduce("+", imputed_averages) / length(imputed_averages)
  
  # Create a new dataset with the old columns and the averaged IndividualIncome
  new_data <- panel_data[c("ID", "Year", "Education", "Age")]
  new_data$IndividualIncome <- average_income
  
  return(new_data)
}

# Assuming balanced_panel_data_mcar_50 is your input dataset
result_data <- Data_Imputation_mitml_Bal(balanced_panel_data_mcar_50)
View(result_data)

# Convert to panel data structure
result_data_panel <- pdata.frame(result_data, index = c("ID", "Year"))

# Fit the model
model <- plm(IndividualIncome ~ Education + Age, data = result_data_panel, model = "within")

# Show the summary of the model to see coefficients
summary(model)
