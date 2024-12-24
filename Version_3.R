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

