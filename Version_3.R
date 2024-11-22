# True coefficients
true_coef <- c(EmploymentTypes = -7.1148978471,
               Age = -0.0008014146,
               MaritalStatus = 0.1157570148,
               EmploymentHours = 0.2799675101)

# Coefficients for each imputation method
coef_lstm <- c(EmploymentTypes = -7.2622037785,
               Age = -0.0008538329,
               MaritalStatus = 0.1022849018,
               EmploymentHours = 0.1226411015)

coef_amelia <- c(EmploymentTypes = -7.275262115,
                 Age = 0.001235834,
                 MaritalStatus = 0.070931374,
                 EmploymentHours = 0.142571039)

coef_mitml <- c(EmploymentTypes = -3.566126897,
                Age = -0.027311932,
                MaritalStatus = 0.008206598,
                EmploymentHours = 0.136693599)

coef_mice <- c(EmploymentTypes = -4.64371085,
               Age = -0.02603853,
               MaritalStatus = 0.32597490,
               EmploymentHours = 0.12117332)

# Calculate the bias for each method (absolute differences)
bias_lstm <- abs(coef_lstm - true_coef)
bias_amelia <- abs(coef_amelia - true_coef)
bias_mitml <- abs(coef_mitml - true_coef)
bias_mice <- abs(coef_mice - true_coef)

# Compute the mean bias for each method
mean_bias_lstm <- mean(bias_lstm)
mean_bias_amelia <- mean(bias_amelia)
mean_bias_mitml <- mean(bias_mitml)
mean_bias_mice <- mean(bias_mice)

# Results comparison
comparison_bias <- data.frame(
  Method = c("LSTM", "Amelia", "mitml", "MICE"),
  Mean_Bias = c(mean_bias_lstm, mean_bias_amelia, mean_bias_mitml, mean_bias_mice)
)

print(comparison_bias)