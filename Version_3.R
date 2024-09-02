Data_Imputation_mitml_Bal <- function(panel_data) {
  selected_data <- panel_data[c("ID", "Year", "Education", "Age", "IndividualIncome")]
  type <- c(0, -2, 2, 2, 1) 
  names(type) <- colnames(selected_data)
  
  imputed_data <- panImpute(selected_data, type = type, n.burn = 1000, n.iter = 100, m = 3)
  imputed_list <- mitmlComplete(imputed_data, print = "all")
  
  breusch_pagan_results <- lapply(imputed_list, function(x) {
    pdata <- pdata.frame(x, index = c("ID", "Year"))
    bp_test <- plmtest(plm(IndividualIncome ~ Education + Age, data = pdata, model = "pooling"), type = "bp")
    return(bp_test$p.value)
  })
  
  model_list <- lapply(1:length(imputed_list), function(i) {
    pdata <- pdata.frame(imputed_list[[i]], index = c("ID", "Year"))
    if (breusch_pagan_results[[i]] > 0.05) {
      return(plm(IndividualIncome ~ Education + Age, data = pdata, model = "pooling"))
    } else {
      random_model <- plm(IndividualIncome ~ Education + Age, data = pdata, model = "random")
      fixed_model <- plm(IndividualIncome ~ Education + Age, data = pdata, model = "within")
      hausman_test <- phtest(fixed_model, random_model)
      
      if (hausman_test$p.value <= 0.05) {
        cat("Fixed Effects Model Summary for imputation", i, ":\n")
        print(summary(fixed_model))  # Print summary for each Fixed Effects model
        return(fixed_model)
      } else {
        return(random_model)
      }
    }
  })
  
  pooled_results <- testEstimates(model_list)
  return(summary(pooled_results))
}
Data_Imputation_mitml_Bal(balanced_panel_data_mcar_50)
