



ConDisttributionComparisionBal <- function(data_list, colors, title) {
  
  # Check the format
  data <- as.data.frame(balanced_panel_data) 
  data$Age <- as.numeric(as.vector(data$Age))
  data$Income <- as.numeric(as.vector(data$Income))
  data$MaritalStatus <- factor(data$MaritalStatus, levels = c("1", "2", "3", "4"), 
                               labels = c("Single", "Married", "Divorced", "Widowed"))
  
  
  ConAge <- data %>% filter(Age >= 15 & Age <= 65) # Condition on Age
  
  # Plot the conditional distribution of "Income" by "Marital Status"
  p <- ggplot(ConAge, aes(x = Income)) +
    geom_density(color = "black", size = 1) + # Original data coloured black
    labs(
      title = title,
      x = "Income",
      y = "Density"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    facet_wrap(~ MaritalStatus)
  
  # Compare imputed datasets
  for (i in 1:length(data_list)) {
    ImpData <- as.data.frame(data_list[[i]])
    ImpData$Age <- as.numeric(as.vector(ImpData$Age))
    ImpData$Income <- as.numeric(as.vector(ImpData$Income))
    ImpData$MaritalStatus <- factor(ImpData$MaritalStatus, levels = c("1", "2", "3", "4"), 
                                         labels = c("Single", "Married", "Divorced", "Widowed"))
    ImpDataConAge <- ImpData %>% filter(Age >= 15 & Age <= 65)
    
    # Add density line for each imputed dataset with the specified colors
    p <- p + geom_density(data = ImpDataConAge, aes(x = Income), 
                          color = colors[i], size = .5, linetype = "solid") 
  }
  
  print(p) # Print the plot
  

}


colors <- c("blue", "red", "green", "purple") # mice = blue, amelia = red, mitml = green, lstm = purple


###### MCAR ##### 
# 10% missingness
ConDisttributionComparisionBal(
  list(mice_imp_bal_mcar_10, amelia_imp_bal_mcar_10, mitml_imp_bal_mcar_10, lstm_bal_mcar_10),
  colors,
  "Balanced Panel - MCAR 10%"
)

# 30% missingness
ConDisttributionComparisionBal(
  list(mice_imp_bal_mcar_30, amelia_imp_bal_mcar_30, mitml_imp_bal_mcar_30, lstm_bal_mcar_30),
  colors,
  "Balanced Panel - MCAR 30%"
)

# 50% missingness
ConDisttributionComparisionBal(
  list(mice_imp_bal_mcar_50, amelia_imp_bal_mcar_50, mitml_imp_bal_mcar_50, lstm_bal_mcar_50),
  colors,
  "Balanced Panel - MCAR 50%"
)

##### MAR #####

# 10% missingness
ConDisttributionComparisionBal(
  list(mice_imp_bal_mar_10, amelia_imp_bal_mar_10, mitml_imp_bal_mar_10, lstm_bal_mar_10),
  colors,
  "Balanced Panel - MAR 10%"
)

# 30% missingness
ConDisttributionComparisionBal(
  list(mice_imp_bal_mar_30, amelia_imp_bal_mar_30, mitml_imp_bal_mar_30, lstm_bal_mar_30),
  colors,
  "Balanced Panel - MAR 30%"
)

# 50% missingness
ConDisttributionComparisionBal(
  list(mice_imp_bal_mar_50, amelia_imp_bal_mar_50, mitml_imp_bal_mar_50, lstm_bal_mar_50),
  colors,
  "Balanced Panel - MAR 50%"
)

##### MNAR #####

# 10% missingness
ConDisttributionComparisionBal(
  list(mice_imp_bal_mnar_10, amelia_imp_bal_mnar_10, mitml_imp_bal_mnar_10, lstm_bal_mnar_10),
  colors,
  "Balanced Panel - MNAR 10%"
)

# 30% missingness
ConDisttributionComparisionBal(
  list(mice_imp_bal_mnar_30, amelia_imp_bal_mar_30, mitml_imp_bal_mnar_30, lstm_bal_mnar_30),
  colors,
  "Balanced Panel - MNAR 30%"
)

# 50% missingness
ConDisttributionComparisionBal(
  list(mice_imp_bal_mnar_50, amelia_imp_bal_mnar_50, mitml_imp_bal_mnar_50, lstm_bal_mnar_50),
  colors,
  "Balanced Panel - MNAR 50%"
)







ConDisttributionComparisionUnbal <- function(data_list, colors, title) {
  
  # Check the format, because the p.data format does not work properly
  data <- as.data.frame(unbalanced_panel_data) 
  data$Age <- as.numeric(as.vector(data$Age))
  data$Income <- as.numeric(as.vector(data$Income))
  data$MaritalStatus <- factor(data$MaritalStatus, levels = c("1", "2", "3", "4"), 
                               labels = c("Single", "Married", "Divorced", "Widowed"))
  
  
  ConAge <- data %>% filter(Age >= 15 & Age <= 65) # Condition on Age
  
  # Plot the conditional distribution of "Income" by "Marital Status"
  p <- ggplot(ConAge, aes(x = Income)) +
    geom_density(color = "black", size = 1) + # Original data coloured black
    labs(
      title = title,
      x = "Income",
      y = "Density"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    facet_wrap(~ MaritalStatus)
  
  # Compare imputed datasets
  for (i in 1:length(data_list)) {
    ImpData <- as.data.frame(data_list[[i]])
    ImpData$Age <- as.numeric(as.vector(ImpData$Age))
    ImpData$Income <- as.numeric(as.vector(ImpData$Income))
    ImpData$MaritalStatus <- factor(ImpData$MaritalStatus, levels = c("1", "2", "3", "4"), 
                                    labels = c("Single", "Married", "Divorced", "Widowed"))
    ImpDataConAge <- ImpData %>% filter(Age >= 15 & Age <= 65)
    
    # Add density line for each imputed dataset with the specified colors
    p <- p + geom_density(data = ImpDataConAge, aes(x = Income), 
                          color = colors[i], size = .5, linetype = "solid") 
  }
  
  print(p) # Print the plot
  
  
}



### MCAR ###
# 10% missingness
ConDisttributionComparisionUnbal(
  list(mice_imp_unbal_mcar_10, amelia_imp_unbal_mcar_10, mitml_imp_unbal_mcar_10, lstm_unbal_mcar_10),
  colors,
  "Unbalanced Panel - MCAR 10%"
)

# 30% missingness
ConDisttributionComparisionUnbal(
  list(mice_imp_unbal_mcar_30, amelia_imp_unbal_mcar_30, mitml_imp_unbal_mcar_30, lstm_unbal_mcar_30),
  colors,
  "Unbalanced Panel - MCAR 30%"
)

# 50% missingness
ConDisttributionComparisionUnbal(
  list(mice_imp_unbal_mcar_50, amelia_imp_unbal_mcar_50, mitml_imp_unbal_mcar_50, lstm_unbal_mcar_50),
  colors,
  "Unbalanced Panel - MCAR 50%"
)

### MAR ###

# 10% missingness
ConDisttributionComparisionUnbal(
  list(mice_imp_unbal_mar_10, amelia_imp_unbal_mar_10, mitml_imp_unbal_mar_10, lstm_unbal_mar_10),
  colors,
  "Unbalanced Panel - MAR 10%"
)

# 30% missingness
ConDisttributionComparisionUnbal(
  list(mice_imp_unbal_mar_30, amelia_imp_unbal_mar_30, mitml_imp_unbal_mar_30, lstm_unbal_mar_30),
  colors,
  "Unbalanced Panel - MAR 30%"
)

# 50% missingness
ConDisttributionComparisionUnbal(
  list(mice_imp_unbal_mar_50, amelia_imp_unbal_mar_50, mitml_imp_unbal_mar_50, lstm_unbal_mar_50),
  colors,
  "Unbalanced Panel - MAR 50%"
)

### MNAR ###

# 10% missingness
ConDisttributionComparisionUnbal(
  list(mice_imp_unbal_mnar_10, amelia_imp_unbal_mnar_10, mitml_imp_unbal_mnar_10, lstm_unbal_mnar_10),
  colors,
  "Unbalanced Panel - MNAR 10%"
)

# 30% missingness
ConDisttributionComparisionUnbal(
  list(mice_imp_unbal_mnar_30, amelia_imp_unbal_mnar_30, mitml_imp_unbal_mnar_30, lstm_unbal_mnar_30),
  colors,
  "Unbalanced Panel - MCAR 30%"
)

# 50% missingness
ConDisttributionComparisionUnbal(
  list(mice_imp_unbal_mnar_50, amelia_imp_unbal_mnar_50, mitml_imp_unbal_mnar_50, lstm_unbal_mnar_50),
  colors,
  "Unbalanced Panel - MNAR 50%"
)