# Assuming CoeffDF and balanced_panel_data_coef are already loaded

# Merge the two data frames on the 'Variable' column
merged_df <- merge(CoeffDF, balanced_panel_data_coef, by = "Variable")

# Calculate bias by subtracting the true coefficients from each imputed column
bias_df <- merged_df
for (col in names(CoeffDF)[-1]) {  # Exclude 'Variable'
  bias_df[[col]] <- merged_df[[col]] - merged_df$Coefficient
}

# Remove the true coefficient column for clarity
bias_df <- bias_df[, !(names(bias_df) %in% "Coefficient")]

# View the bias data frame
print(bias_df)
View(merged_df)



# Calculate the mean bias for each dataset (excluding the 'Variable' column)
mean_bias <- colMeans(bias_df[, -1])  # Exclude 'Variable' column

# Create a data frame of mean bias values
mean_bias_df <- data.frame(
  Dataset = names(mean_bias),
  MeanBias = mean_bias
)

# Rank the datasets by mean bias in ascending order
mean_bias_df <- mean_bias_df[order(mean_bias_df$MeanBias), ]

# Add a rank column
mean_bias_df$Rank <- seq_len(nrow(mean_bias_df))

# View the ranked mean bias data frame
print(mean_bias_df)
View(mean_bias_df)




#############################



# Calculate the RMSE for each dataset
rmse <- apply(bias_df[, -1], 2, function(x) sqrt(mean(x^2)))  # Exclude 'Variable' column

# Create a data frame of RMSE values
rmse_df <- data.frame(
  Dataset = names(rmse),
  RMSE = rmse
)

# Rank the datasets by RMSE in ascending order
rmse_df <- rmse_df[order(rmse_df$RMSE), ]

# Add a rank column
rmse_df$Rank <- seq_len(nrow(rmse_df))

# View the ranked RMSE data frame
print(rmse_df)
View(rmse_df)



#################################
# Load ggplot2 package
library(ggplot2)

# Boxplot to show Income dependency on MaritalStatus and Age
ggplot(mice_imp_bal_mcar_50, aes(x = factor(MaritalStatus), y = Income, fill = factor(MaritalStatus))) +
  geom_boxplot() +
  facet_wrap(~ cut(Age, breaks = 4), scales = "free", ncol = 2) +
  labs(
    title = "Income Distribution by Marital Status and Age Groups",
    x = "Marital Status",
    y = "Income",
    fill = "Marital Status"
  ) +
  theme_minimal()




# Load ggplot2 package
library(ggplot2)

# Density plot with adjusted fill based on MaritalStatus
ggplot(mice_imp_bal_mcar_50, aes(x = Income, fill = factor(MaritalStatus))) +
  geom_density(alpha = 0.6) +  # Adjust transparency to make overlapping clearer
  facet_wrap(~ cut(Age, breaks = 4), scales = "free", ncol = 2) + # Facet by age group
  labs(
    title = "Income Distribution by Marital Status and Age Groups",
    x = "Income",
    y = "Density",
    fill = "Marital Status"
  ) +
  theme_minimal()


































