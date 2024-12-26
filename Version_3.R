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


#################
BiasPlot <- function(BiasDF, bias_column, algorithm_name) {
  
  # Filter for the specific algorithm
  BiasDF_filtered <- BiasDF %>%
    filter(Algorithm == algorithm_name) %>%
    mutate(Dataset = as.factor(Dataset),
           Bias = as.numeric(get(bias_column)))
  
  # Vertical bar chart (similar to RMSEPlot)
  ggplot(BiasDF_filtered, aes(y = reorder(Dataset, Bias), x = Bias)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_text(aes(label = round(Bias, 3)), hjust = -0.2, size = 3) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
    labs(title = paste("Bias of Imputed Datasets:", algorithm_name),
         x = "Bias",
         y = "Dataset") +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 8))
}

# Create plots for each algorithm
plot_mice <- BiasPlot(BiasMeanDF_Bal, "MeanBias", "mice")
plot_amelia <- BiasPlot(BiasMeanDF_Bal, "MeanBias", "amelia")
plot_lstm <- BiasPlot(BiasMeanDF_Bal, "MeanBias", "lstm")
plot_mitml <- BiasPlot(BiasMeanDF_Bal, "MeanBias", "mitml")

# Arrange the plots in a grid (same as for RMSE)
grid.arrange(plot_mice, plot_amelia, plot_lstm, plot_mitml, nrow = 2) # Adjusted for a 2-row layout
