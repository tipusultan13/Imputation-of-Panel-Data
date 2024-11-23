Data_Imputation_mice <- function(data, m = 2, maxit = 5, method = 'pmm') {
  
  # Lagged exogenous variables
  data$Lagged_MaritalStatus <- lag(data$MaritalStatus, 1)
  data$Lagged_Sex <- lag(data$Sex, 1)  # Replace lagged EmploymentHours with lagged Sex
  
  ID <- data$ID
  Year <- data$Year
  EmploymentTypes <- data$EmploymentTypes
  MaritalStatus <- data$MaritalStatus
  EmploymentHours <- data$EmploymentHours
  Education <- data$Education
  Sex <- data$Sex
  
  # Create the temporary dataset for imputation, now including lagged Sex
  DataTemp <- data[c("Lagged_MaritalStatus", "Lagged_Sex", "Age", "Income", "Education", "Sex")]
  
  # Perform MICE imputation
  miceImp <- mice(DataTemp, method = method, m = m, maxit = maxit)
  
  # Re-add the ID, Year, EmploymentTypes, MaritalStatus, EmploymentHours, Education, and Sex columns
  CompleteDataset <- lapply(1:m, function(i) {
    CompleteData <- complete(miceImp, action = i)
    CompleteData <- cbind(ID = ID,
                          Year = Year,
                          EmploymentTypes = EmploymentTypes,
                          MaritalStatus = MaritalStatus,
                          EmploymentHours = EmploymentHours,
                          Education = Education,
                          Sex = Sex,
                          CompleteData)
    CompleteData <- CompleteData[c("ID", "Year", "Age", "EmploymentTypes", "Income", "MaritalStatus", "EmploymentHours", "Education", "Sex")]
    return(CompleteData)
  })
  
  return(CompleteDataset)  # Return a list of completed datasets
}

mice_bal_mcar_50 <- Data_Imputation_mice(balanced_panel_data_mcar_50)
