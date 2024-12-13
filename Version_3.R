# Load necessary libraries
library(ggplot2)
library(plm)
library(dplyr)

# Convert pdata.frame to regular data.frame
data <- as.data.frame(balanced_panel_data)

# Convert pseries columns to numeric (because pseries is a special class for panel data)
data$Age <- as.numeric(as.vector(data$Age))
data$Income <- as.numeric(as.vector(data$Income))

# Ensure MaritalStatus is a factor
data$MaritalStatus <- factor(data$MaritalStatus, levels = c("1", "2", "3", "4"), 
                             labels = c("Single", "Married", "Divorced", "Widowed"))

# Filter data for Age between 20 and 60
data_filtered <- data %>% filter(Age >= 20 & Age <= 60)

# Plot the conditional distribution of income for the filtered age group
ggplot(data_filtered, aes(x = Income, color = MaritalStatus, fill = MaritalStatus)) +
  geom_density(alpha = 0.3) +  # Density plot with transparency
  labs(
    title = "Conditional Distribution of Income by Marital Status and Age (20-60)",
    x = "Income",
    y = "Density",
    color = "Marital Status",
    fill = "Marital Status"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

library(ggplot2)
library(dplyr)

# Function to plot Confidence Interval Overlap with Ranking
plot_CI_overlap <- function(BiasMeanDF, Bias_Bal, dataset_col_name = "Dataset", mean_col_name = "MeanBias") {
  
  # Calculate confidence intervals for each dataset
  BiasMeanDF <- BiasMeanDF %>%
    rowwise() %>%
    mutate(
      LowerCI = !!sym(mean_col_name) - qt(0.975, df = nrow(Bias_Bal) - 1) * sd(Bias_Bal[[!!sym(dataset_col_name)]], na.rm = TRUE) / sqrt(nrow(Bias_Bal)),
      UpperCI = !!sym(mean_col_name) + qt(0.975, df = nrow(Bias_Bal) - 1) * sd(Bias_Bal[[!!sym(dataset_col_name)]], na.rm = TRUE) / sqrt(nrow(Bias_Bal))
    ) %>%
    ungroup()
  
  # Calculate the width of the confidence interval (UpperCI - LowerCI)
  BiasMeanDF <- BiasMeanDF %>%
    mutate(CIWidth = UpperCI - LowerCI)
  
  # Rank datasets by CIWidth (ascending order: smaller interval at the top)
  BiasMeanDF <- BiasMeanDF %>%
    arrange(CIWidth) %>%
    mutate(Rank = row_number())
  
  # Create the confidence interval overlap plot with rankings
  ggplot(BiasMeanDF, aes(x = reorder(!!sym(dataset_col_name), CIWidth), y = !!sym(mean_col_name))) +
    geom_point(size = 3, color = "blue") +
    geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), width = 0.2, color = "red") +
    geom_text(aes(label = Rank), hjust = -0.5, size = 3, color = "black") +  # Add rank labels
    coord_flip() +
    labs(
      title = "Confidence Interval Overlap of Mean Bias (Ranked by CI Width)",
      x = "Dataset",
      y = "Mean Bias"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Example Usage for Balanced Dataset (BiasMeanDF_Bal, Bias_Bal as input)
plot_CI_overlap(BiasMeanDF_Unbal, Bias_Unbal)






library(ggplot2)
library(dplyr)

# Function to plot RMSE comparison with confidence intervals (or standard errors)
plot_RMSE_CI_overlap <- function(RMSEDF, dataset_col_name = "Dataset", rmse_col_name = "RMSE_Bal") {
  
  # Create a basic plot for RMSE comparison (no CI if not available)
  ggplot(RMSEDF, aes(x = reorder(!!sym(dataset_col_name), !!sym(rmse_col_name)), y = !!sym(rmse_col_name))) +
    geom_point(size = 3, color = "blue") +
    geom_text(aes(label = Rank), hjust = -0.5, size = 3, color = "black") +  # Add rank labels
    coord_flip() +
    labs(
      title = "RMSE Comparison (Ranked by RMSE)",
      x = "Dataset",
      y = "RMSE"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Example Usage for Balanced Dataset (RMSEDF_Bal as input)
plot_RMSE_CI_overlap(RMSEDF_Bal)



# Load the required libraries
library(reshape2)
library(ggplot2)
library(pheatmap)

# Define the function to generate correlation heatmaps
create_correlation_heatmap <- function(corr_df) {
  
  # Step 1: Reshape the data into a long format
  corr_df_long <- melt(corr_df, id.vars = NULL, variable.name = "Variable", value.name = "Correlation")
  
  # Step 2: Create the heatmap plot using ggplot2
  ggplot(corr_df_long, aes(x = Variable, y = Variable, fill = Correlation)) +
    geom_tile() + 
    scale_fill_gradient2(low = "gray", high = "red", mid = "white", midpoint = 0) + 
    theme_minimal() +
    labs(title = "Correlation Heatmap", x = "Variable", y = "Dataset", fill = "Correlation") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_discrete(limits = rev(levels(corr_df_long$Variable)))  # Reverse x-axis if needed
  
  # Step 3: Prepare the data for pheatmap
  # Filter out non-numeric columns
  corr_df_numeric <- corr_df[, sapply(corr_df, is.numeric)]
  
  # Convert the dataframe into a matrix for pheatmap
  corr_df_matrix <- as.matrix(corr_df_numeric)
  
  # Step 4: Create the heatmap using pheatmap
  pheatmap(corr_df_matrix, 
           color = colorRampPalette(c("gray", "white", "red"))(50), 
           clustering_distance_rows = "euclidean", 
           clustering_distance_cols = "euclidean", 
           clustering_method = "complete", 
           display_numbers = TRUE, 
           main = "Correlation Heatmap")
}

# Example usage with the balanced correlation data
create_correlation_heatmap(CorrDF_Bal)

# Example usage with unbalanced data
create_correlation_heatmap(CorrDF_Unbal)
