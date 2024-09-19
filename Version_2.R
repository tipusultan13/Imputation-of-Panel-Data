###################################
## Tipu Sultan
###################################

# Set Directory
setwd("/Users/tipusultan/Documents/GitHub/Imputation-of-Panel-Data")

########################
## Data
########################

library(dplyr)
library(readxl)
library(ggplot2)

# Load and clean the data
RawData <- readRDS("population.RDS")
RawData = data.frame(RawData)
data = RawData[c("id", "year","EF310", "EF44", "inc.ind")]
colnames(data) <- c("ID", "Year", "Education", "Age", "IndividualIncome")
summary(data)

# 'ID' column
count(data, ID)

# 'Year' column
count(data, Year)
# Filter the data to keep only rows where the 'Year' is between 2013 and 2023 inclusive
data <- subset(data, Year >= 2013 & Year <= 2023)

# 'Education' column - Highest general school degree
count(data, Education)
data$Education[is.na(data$Education)] <- 7

# Age
count(data, Age)
summary(data$Age)

# Plot the density curve for Age
ggplot(data, aes(x = Age)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Curve of Age",
       x = "Age",
       y = "Density") +
  theme_minimal()

# 'IndividualIncome' column - Income
summary(data$IndividualIncome)
count(data, IndividualIncome)
data$IndividualIncome[is.na(data$IndividualIncome)] <- 0
summary(data$IndividualIncome)

# Plot the histogram for IndividualIncome
ggplot(data, aes(x = IndividualIncome)) +
  geom_histogram(binwidth = 5000, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Individual Income",
       x = "Individual Income",
       y = "Frequency") +
  theme_minimal()

# Apply log transformation to IndividualIncome (adding 1 to avoid log(0))
data$LogIndividualIncome <- log(data$IndividualIncome + 1)

# Plot the histogram with the log-transformed IndividualIncome
ggplot(data, aes(x = LogIndividualIncome)) +
  geom_histogram(binwidth = 0.2, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Log-Transformed Individual Income",
       x = "Log of Individual Income",
       y = "Frequency") +
  theme_minimal()

sum(is.na(data)) #Total number of NA values in the data frame
data <- data[c("ID", "Year", "Education", "Age", "IndividualIncome")]
summary(data)

########################
## Balanced Panel
########################

# Ensure the 'Year' column is numeric or integer to make the calculation easy
data$Year <- as.numeric(data$Year)

# Find IDs that are present in all years between 2013 and 2023
years <- 2013:2023
common_ids <- Reduce(intersect, lapply(years, function(year) {
  unique(data$ID[data$Year == year])
}))

# Filter the data to keep only rows with common IDs
panel_data <- data[data$ID %in% common_ids, ]

# Count occurrences of each ID
id_counts <- panel_data %>%
  count(ID)

# Extract IDs with exactly 11 occurrences
ids_with_11_occurrences <- id_counts %>%
  filter(n == 11) %>%
  pull(ID)

# Filter the original dataset to include only rows with these IDs
balanced_panel_data <- panel_data %>%
  filter(ID %in% ids_with_11_occurrences)

# Count the number of unique years each ID appears in
id_year_count <- aggregate(Year ~ ID, data = balanced_panel_data, 
                           FUN = function(x) length(unique(x)))

# Check if every ID appears in all the years (2013 to 2023)
is_balanced_panel <- all(id_year_count$Year == length(2013:2023))

if (is_balanced_panel) {
  print("The data is a balanced panel.")
} else {
  print("The data is not a balanced panel.")
}

# Optionally, print the IDs that do not appear in all years
unbalanced_ids <- id_year_count$ID[id_year_count$Year != length(2013:2023)]
if (length(unbalanced_ids) > 0) {
  print("IDs that do not appear in all years:")
  print(unbalanced_ids)
}

# Count occurrences of each ID
id_counts <- balanced_panel_data %>%
  count(ID)

# Filter IDs that appear more than 11 times
more_than_11 <- id_counts %>%
  filter(n > 11)
num_more_than_11 <- nrow(more_than_11)

# Filter IDs that appear less than 11 times
less_than_11 <- id_counts %>%
  filter(n < 11)
num_less_than_11 <- nrow(less_than_11)

print(paste("Number of IDs appearing more than 11 times:", num_more_than_11))
print(paste("Number of IDs appearing less than 11 times:", num_less_than_11))

count(balanced_panel_data, Year)
summary(balanced_panel_data)

########################
## Unalanced Panel
########################

library(dplyr)

# Initialize an empty data frame to store the sampled data
sampled_data <- data.frame()

# Loop through each year to ensure 3455 unique ID-Year combinations
for (yr in unique(data$Year)) {
  # Filter the data for the current year
  year_data <- data %>% filter(Year == yr)
  
  # Sample 3455 unique ID-Year combinations for this year
  temp_sample <- year_data %>%
    distinct(ID, Year, .keep_all = TRUE) %>%
    slice_sample(n = 3455)
  
  # Append the sampled data to the main data frame
  sampled_data <- bind_rows(sampled_data, temp_sample)
}

unbalanced_panel_data <- sampled_data

# Count the number of unique years each ID appears in
id_year_count <- aggregate(Year ~ ID, data = unbalanced_panel_data, 
                           FUN = function(x) length(unique(x)))

# Check if every ID appears in all the years (2013 to 2023)
is_balanced_panel <- all(id_year_count$Year == length(2013:2023))

if (is_balanced_panel) {
  print("The data is a balanced panel.")
} else {
  print("The data is not a balanced panel.")
}

# Optionally, print the IDs that do not appear in all years
unbalanced_ids <- id_year_count$ID[id_year_count$Year != length(2013:2023)]
if (length(unbalanced_ids) > 0) {
  print("IDs that do not appear in all years:")
  print(unbalanced_ids)
}

# Check for duplicate rows in the data
print(unbalanced_panel_data[duplicated(unbalanced_panel_data), ])

# Display the count of observations per year and summary statistics
count(unbalanced_panel_data, Year)
summary(unbalanced_panel_data)

########################
## Missingness in Balanced Panel
########################

library(VIM)

#### MCAR ####
##############

#### 50% ####

p_mis_50 <- 0.50
num_rows <- nrow(balanced_panel_data)  # Get the number of rows in balanced_panel_data
mis_simulated_mcar_50 <- sample(1:num_rows, p_mis_50 * num_rows, replace = FALSE)    
balanced_panel_data_mcar_50 <- balanced_panel_data
balanced_panel_data_mcar_50[mis_simulated_mcar_50, 5] <- NA
summary(balanced_panel_data_mcar_50)

# Visualize the missing data pattern using the VIM package
aggr(balanced_panel_data_mcar_50, 
     col = c('navyblue', 'red'), 
     numbers = TRUE, 
     sortVars = TRUE, 
     labels = names(balanced_panel_data_mcar_50), 
     cex.axis = .7, 
     gap = 3, 
     ylab = c("Missing data", "Pattern"))

#### 30% ####

p_mis_30 <- 0.30
num_rows <- nrow(balanced_panel_data)  # Get the number of rows in balanced_panel_data
mis_simulated_mcar_30 <- sample(1:num_rows, p_mis_30 * num_rows, replace = FALSE)    
balanced_panel_data_mcar_30 <- balanced_panel_data
balanced_panel_data_mcar_30[mis_simulated_mcar_30, 5] <- NA
summary(balanced_panel_data_mcar_30)

# Visualize the missing data pattern
aggr(balanced_panel_data_mcar_30, 
     col = c('navyblue', 'red'), 
     numbers = TRUE, 
     sortVars = TRUE, 
     labels = names(balanced_panel_data_mcar_30), 
     cex.axis = .7, 
     gap = 3, 
     ylab = c("Missing data", "Pattern"))

#### 30% ####

p_mis_10 <- 0.10
num_rows <- nrow(balanced_panel_data)  # Get the number of rows in balanced_panel_data
mis_simulated_mcar_10 <- sample(1:num_rows, p_mis_10 * num_rows, replace = FALSE)    
balanced_panel_data_mcar_10 <- balanced_panel_data
balanced_panel_data_mcar_10[mis_simulated_mcar_10, 5] <- NA
summary(balanced_panel_data_mcar_10)

# Visualize the missing data pattern
aggr(balanced_panel_data_mcar_10, 
     col = c('navyblue', 'red'), 
     numbers = TRUE, 
     sortVars = TRUE, 
     labels = names(balanced_panel_data_mcar_10), 
     cex.axis = .7, 
     gap = 3, 
     ylab = c("Missing data", "Pattern"))

#### MAR ####
#############

#### 50% ####

balanced_panel_data_mar_50 <- balanced_panel_data
set.seed(123)
p_mis_50 <- 0.5  # 50% missingness

# Depending on Education, Age, and random error
mis_simulated_mar_50 <- 0.5 + 0.1 * balanced_panel_data_mar_50$Education + 
  0.2 * balanced_panel_data_mar_50$Age + 
  rnorm(nrow(balanced_panel_data_mar_50), 0, 3)

# All below the 50% quantile are set to missing
mis_simulated_mar_50 <- mis_simulated_mar_50 < quantile(mis_simulated_mar_50, p_mis_50)

# Proportion of missing data
mean(as.numeric(mis_simulated_mar_50))

# Set values to NA in IndividualIncome where missingness occurs
balanced_panel_data_mar_50$IndividualIncome[mis_simulated_mar_50] <- NA

# Summary of IndividualIncome after introducing missingness
summary(balanced_panel_data_mar_50)

# Visualize the missing data pattern
aggr(balanced_panel_data_mar_50, 
     col = c('navyblue', 'red'), 
     numbers = TRUE, 
     sortVars = TRUE, 
     labels = names(balanced_panel_data_mar_50), 
     cex.axis = .7, 
     gap = 3, 
     ylab = c("Missing data", "Pattern"))


#### 30% ####

balanced_panel_data_mar_30 <- balanced_panel_data
set.seed(123)
p_mis_50 <- 0.3  # 30% missingness

# Depending on Education, Age, and random error
mis_simulated_mar_30 <- 0.7 + 0.1 * balanced_panel_data_mar_30$Education + 
  0.2 * balanced_panel_data_mar_30$Age + 
  rnorm(nrow(balanced_panel_data_mar_30), 0, 3)

# All below the 50% quantile are set to missing
mis_simulated_mar_30 <- mis_simulated_mar_30 < quantile(mis_simulated_mar_30, p_mis_30)

# Proportion of missing data
mean(as.numeric(mis_simulated_mar_30))

# Set values to NA in IndividualIncome where missingness occurs
balanced_panel_data_mar_30$IndividualIncome[mis_simulated_mar_30] <- NA

# Summary of IndividualIncome after introducing missingness
summary(balanced_panel_data_mar_30)

# Visualize the missing data pattern
aggr(balanced_panel_data_mar_30, 
     col = c('navyblue', 'red'), 
     numbers = TRUE, 
     sortVars = TRUE, 
     labels = names(balanced_panel_data_mar_30), 
     cex.axis = .7, 
     gap = 3, 
     ylab = c("Missing data", "Pattern"))

#### 10% ####

balanced_panel_data_mar_10 <- balanced_panel_data
set.seed(123)
p_mis_50 <- 0.1  # 10% missingness

# Depending on Education, Age, and random error
mis_simulated_mar_10 <- 0.1 + 0.1 * balanced_panel_data_mar_10$Education + 
  0.2 * balanced_panel_data_mar_10$Age + 
  rnorm(nrow(balanced_panel_data_mar_10), 0, 3)

# All below the 50% quantile are set to missing
mis_simulated_mar_10 <- mis_simulated_mar_10 < quantile(mis_simulated_mar_10, p_mis_10)

# Proportion of missing data
mean(as.numeric(mis_simulated_mar_10))

# Set values to NA in IndividualIncome where missingness occurs
balanced_panel_data_mar_10$IndividualIncome[mis_simulated_mar_10] <- NA

# Summary of IndividualIncome after introducing missingness
summary(balanced_panel_data_mar_10)

# Visualize the missing data pattern
aggr(balanced_panel_data_mar_10, 
     col = c('navyblue', 'red'), 
     numbers = TRUE, 
     sortVars = TRUE, 
     labels = names(balanced_panel_data_mar_10), 
     cex.axis = .7, 
     gap = 3, 
     ylab = c("Missing data", "Pattern"))

#### MNAR ####
##############

#### Probabilistic, Linear Regression model ####
#### 50% ####

p_mis_50 <- .50
balanced_panel_data_mnar_50 <- balanced_panel_data
# the missing of a value now also depends on IndividualIncome itself
mis_simulated_mnar_50 <- 0.5 + .2 * balanced_panel_data_mnar_50$Education + 
  0.1 * balanced_panel_data_mnar_50$Age + 
  0.5 * balanced_panel_data_mnar_50$IndividualIncome + rnorm(nrow(balanced_panel_data), 0, 3)
mis_simulated_mnar_50 <- mis_simulated_mnar_50 < quantile(mis_simulated_mnar_50, p_mis_50)
balanced_panel_data_mnar_50$IndividualIncome[mis_simulated_mnar_50] <- NA
summary(balanced_panel_data_mnar_50)

# Visualize the missing data pattern
aggr(balanced_panel_data_mnar_50, 
     col = c('navyblue', 'red'), 
     numbers = TRUE, 
     sortVars = TRUE, 
     labels = names(balanced_panel_data_mnar_50), 
     cex.axis = .7, 
     gap = 3, 
     ylab = c("Missing data", "Pattern"))

#### 30% ####

p_mis_30 <- .30
balanced_panel_data_mnar_30 <- balanced_panel_data
# the missing of a value now also depends on IndividualIncome itself
mis_simulated_mnar_30 <- 0.7 + 0.1 * balanced_panel_data_mnar_30$Education + 
  0.2 * balanced_panel_data_mnar_30$Age + 
  0.5 * balanced_panel_data_mnar_30$IndividualIncome + rnorm(nrow(balanced_panel_data), 0, 3)
mis_simulated_mnar_30 <- mis_simulated_mnar_30 < quantile(mis_simulated_mnar_30, p_mis_30)
balanced_panel_data_mnar_30$IndividualIncome[mis_simulated_mnar_30] <- NA
summary(balanced_panel_data_mnar_30)

# Visualize the missing data pattern
aggr(balanced_panel_data_mnar_30, 
     col = c('navyblue', 'red'), 
     numbers = TRUE, 
     sortVars = TRUE, 
     labels = names(balanced_panel_data_mnar_30), 
     cex.axis = .7, 
     gap = 3, 
     ylab = c("Missing data", "Pattern"))

#### 10% ####

p_mis_10 <- .10
balanced_panel_data_mnar_10 <- balanced_panel_data
# the missing of a value now also depends on IndividualIncome itself
mis_simulated_mnar_10 <- 0.1 + 0.1 * balanced_panel_data_mnar_10$Education + 
  0.2 * balanced_panel_data_mnar_10$Age + 
  0.5 * balanced_panel_data_mnar_10$IndividualIncome + rnorm(nrow(balanced_panel_data), 0, 3)
mis_simulated_mnar_10 <- mis_simulated_mnar_10 < quantile(mis_simulated_mnar_10, p_mis_10)
balanced_panel_data_mnar_10$IndividualIncome[mis_simulated_mnar_10] <- NA
summary(balanced_panel_data_mnar_10)

# Visualize the missing data pattern
aggr(balanced_panel_data_mnar_10, 
     col = c('navyblue', 'red'), 
     numbers = TRUE, 
     sortVars = TRUE, 
     labels = names(balanced_panel_data_mnar_10), 
     cex.axis = .7, 
     gap = 3, 
     ylab = c("Missing data", "Pattern"))

########################
## Missingness in Unalanced Panel
########################

#### MCAR ####
##############

#### 50% ####

p_mis_50 <- 0.50
num_rows <- nrow(unbalanced_panel_data)  # Get the number of rows in balanced_panel_data
mis_simulated_mcar_50 <- sample(1:num_rows, p_mis_50 * num_rows, replace = FALSE)    
unbalanced_panel_data_mcar_50 <- unbalanced_panel_data
unbalanced_panel_data_mcar_50[mis_simulated_mcar_50, 5] <- NA
summary(unbalanced_panel_data_mcar_50)

# Visualize the missing data pattern
aggr(unbalanced_panel_data_mcar_50, 
     col = c('navyblue', 'red'), 
     numbers = TRUE, 
     sortVars = TRUE, 
     labels = names(unbalanced_panel_data_mcar_50), 
     cex.axis = .7, 
     gap = 3, 
     ylab = c("Missing data", "Pattern"))

#### 30% ####

p_mis_30 <- 0.30
num_rows <- nrow(unbalanced_panel_data)  # Get the number of rows in balanced_panel_data
mis_simulated_mcar_30 <- sample(1:num_rows, p_mis_30 * num_rows, replace = FALSE)    
unbalanced_panel_data_mcar_30 <- unbalanced_panel_data
unbalanced_panel_data_mcar_30[mis_simulated_mcar_30, 5] <- NA
summary(unbalanced_panel_data_mcar_30)

# Visualize the missing data pattern
aggr(unbalanced_panel_data_mcar_30, 
     col = c('navyblue', 'red'), 
     numbers = TRUE, 
     sortVars = TRUE, 
     labels = names(unbalanced_panel_data_mcar_30), 
     cex.axis = .7, 
     gap = 3, 
     ylab = c("Missing data", "Pattern"))

#### 10% ####

p_mis_10 <- 0.10
num_rows <- nrow(unbalanced_panel_data)  # Get the number of rows in balanced_panel_data
mis_simulated_mcar_10 <- sample(1:num_rows, p_mis_10 * num_rows, replace = FALSE)    
unbalanced_panel_data_mcar_10 <- unbalanced_panel_data
unbalanced_panel_data_mcar_10[mis_simulated_mcar_10, 5] <- NA
summary(unbalanced_panel_data_mcar_10)

# Visualize the missing data pattern
aggr(unbalanced_panel_data_mcar_10, 
     col = c('navyblue', 'red'), 
     numbers = TRUE, 
     sortVars = TRUE, 
     labels = names(unbalanced_panel_data_mcar_10), 
     cex.axis = .7, 
     gap = 3, 
     ylab = c("Missing data", "Pattern"))

#### MAR ####
#############

#### 50% ####
unbalanced_panel_data_mar_50 <- unbalanced_panel_data
set.seed(123)
p_mis_50 <- 0.5  # 50% missingness

# Depending on Education, Age, and random error
mis_simulated_mar_50 <- 0.5 + 0.1 * unbalanced_panel_data_mar_50$Education + 
  0.2 * unbalanced_panel_data_mar_50$Age + 
  rnorm(nrow(unbalanced_panel_data_mar_50), 0, 3)

# All below the 50% quantile are set to missing
mis_simulated_mar_50 <- mis_simulated_mar_50 < quantile(mis_simulated_mar_50, p_mis_50)

# Proportion of missing data
mean(as.numeric(mis_simulated_mar_50))

# Set values to NA in IndividualIncome where missingness occurs
unbalanced_panel_data_mar_50$IndividualIncome[mis_simulated_mar_50] <- NA

# Summary of IndividualIncome after introducing missingness
summary(unbalanced_panel_data_mar_50)

# Visualize the missing data pattern
aggr(unbalanced_panel_data_mar_50, 
     col = c('navyblue', 'red'), 
     numbers = TRUE, 
     sortVars = TRUE, 
     labels = names(unbalanced_panel_data_mar_50), 
     cex.axis = .7, 
     gap = 3, 
     ylab = c("Missing data", "Pattern"))

#### 30% ####
unbalanced_panel_data_mar_30 <- unbalanced_panel_data
set.seed(123)
p_mis_50 <- 0.3  # 30% missingness

# Depending on Education, Age, and random error
mis_simulated_mar_30 <- 0.7 + 0.1 * unbalanced_panel_data_mar_30$Education + 
  0.2 * unbalanced_panel_data_mar_30$Age + 
  rnorm(nrow(unbalanced_panel_data_mar_30), 0, 3)

# All below the 50% quantile are set to missing
mis_simulated_mar_30 <- mis_simulated_mar_30 < quantile(mis_simulated_mar_30, p_mis_30)

# Proportion of missing data
mean(as.numeric(mis_simulated_mar_30))

# Set values to NA in IndividualIncome where missingness occurs
unbalanced_panel_data_mar_30$IndividualIncome[mis_simulated_mar_30] <- NA

# Summary of IndividualIncome after introducing missingness
summary(unbalanced_panel_data_mar_30)

# Visualize the missing data pattern
aggr(unbalanced_panel_data_mar_30, 
     col = c('navyblue', 'red'), 
     numbers = TRUE, 
     sortVars = TRUE, 
     labels = names(unbalanced_panel_data_mar_30), 
     cex.axis = .7, 
     gap = 3, 
     ylab = c("Missing data", "Pattern"))

#### 10% ####

unbalanced_panel_data_mar_10 <- unbalanced_panel_data
set.seed(123)
p_mis_50 <- 0.1  # 10% missingness

# Depending on Education, Age, and random error
mis_simulated_mar_10 <- 0.1 + 0.1 * unbalanced_panel_data_mar_10$Education + 
  0.2 * unbalanced_panel_data_mar_10$Age + 
  rnorm(nrow(unbalanced_panel_data_mar_10), 0, 3)

# All below the 50% quantile are set to missing
mis_simulated_mar_10 <- mis_simulated_mar_10 < quantile(mis_simulated_mar_10, p_mis_10)

# Proportion of missing data
mean(as.numeric(mis_simulated_mar_10))

# Set values to NA in IndividualIncome where missingness occurs
unbalanced_panel_data_mar_10$IndividualIncome[mis_simulated_mar_10] <- NA

# Summary of IndividualIncome after introducing missingness
summary(unbalanced_panel_data_mar_10)

# Visualize the missing data pattern
aggr(unbalanced_panel_data_mar_30, 
     col = c('navyblue', 'red'), 
     numbers = TRUE, 
     sortVars = TRUE, 
     labels = names(unbalanced_panel_data_mar_10), 
     cex.axis = .7, 
     gap = 3, 
     ylab = c("Missing data", "Pattern"))

#### MNAR ####
##############

#### 50% ####
#### Probabilistic, Linear Regression model, Real data ####

p_mis_50 <- .50
unbalanced_panel_data_mnar_50 <- unbalanced_panel_data
# the missing of a value now also depends on IndividualIncome itself
mis_simulated_mnar_50 <- 0.5 + 0.1 * unbalanced_panel_data_mnar_50$Education + 
  0.2 * unbalanced_panel_data_mnar_50$Age + 
  0.5 * unbalanced_panel_data_mnar_50$IndividualIncome + rnorm(nrow(unbalanced_panel_data), 0, 3)
mis_simulated_mnar_50 <- mis_simulated_mnar_50 < quantile(mis_simulated_mnar_50, p_mis_50)
unbalanced_panel_data_mnar_50$IndividualIncome[mis_simulated_mnar_50] <- NA
summary(unbalanced_panel_data_mnar_50)

# Visualize the missing data pattern
aggr(unbalanced_panel_data_mnar_50, 
     col = c('navyblue', 'red'), 
     numbers = TRUE, 
     sortVars = TRUE, 
     labels = names(unbalanced_panel_data_mnar_50), 
     cex.axis = .7, 
     gap = 3, 
     ylab = c("Missing data", "Pattern"))

#### 30% ####

p_mis_30 <- .30
unbalanced_panel_data_mnar_30 <- unbalanced_panel_data
# the missing of a value now also depends on IndividualIncome itself
mis_simulated_mnar_30 <-0.7 + 0.1 * unbalanced_panel_data_mnar_30$Education + 
  0.2 * unbalanced_panel_data_mnar_30$Age + 
  0.5 * unbalanced_panel_data_mnar_30$IndividualIncome + rnorm(nrow(unbalanced_panel_data), 0, 3)
mis_simulated_mnar_30 <- mis_simulated_mnar_30 < quantile(mis_simulated_mnar_30, p_mis_30)
unbalanced_panel_data_mnar_30$IndividualIncome[mis_simulated_mnar_30] <- NA
summary(unbalanced_panel_data_mnar_30)

# Visualize the missing data pattern
aggr(unbalanced_panel_data_mnar_30, 
     col = c('navyblue', 'red'), 
     numbers = TRUE, 
     sortVars = TRUE, 
     labels = names(unbalanced_panel_data_mnar_30), 
     cex.axis = .7, 
     gap = 3, 
     ylab = c("Missing data", "Pattern"))

#### 10% ####

p_mis_10 <- .10
unbalanced_panel_data_mnar_10 <- unbalanced_panel_data
# the missing of a value now also depends on IndividualIncome itself
mis_simulated_mnar_10 <- 0.1 + 0.1 * unbalanced_panel_data_mnar_10$Education + 
  0.2 * unbalanced_panel_data_mnar_10$Age + 
  0.5 * unbalanced_panel_data_mnar_10$IndividualIncome + rnorm(nrow(unbalanced_panel_data), 0, 3)
mis_simulated_mnar_10 <- mis_simulated_mnar_10 < quantile(mis_simulated_mnar_10, p_mis_10)
unbalanced_panel_data_mnar_10$IndividualIncome[mis_simulated_mnar_10] <- NA
summary(unbalanced_panel_data_mnar_10)

# Visualize the missing data pattern
aggr(unbalanced_panel_data_mnar_10, 
     col = c('navyblue', 'red'), 
     numbers = TRUE, 
     sortVars = TRUE, 
     labels = names(unbalanced_panel_data_mnar_10), 
     cex.axis = .7, 
     gap = 3, 
     ylab = c("Missing data", "Pattern"))

###################
### Data Sets Overview and Formating
###################

## Format Vriable to Their Originl Format
convert_to_factors <- function(df) {
  df$ID <- as.factor(df$ID)
  df$Year <- as.factor(df$Year)
  df$Education <- as.factor(df$Education)
  return(df)
}

# Apply the function to each dataset
balanced_panel_data_mcar_50 <- convert_to_factors(balanced_panel_data_mcar_50)
balanced_panel_data_mcar_30 <- convert_to_factors(balanced_panel_data_mcar_30)
balanced_panel_data_mcar_10 <- convert_to_factors(balanced_panel_data_mcar_10)

balanced_panel_data_mar_50 <- convert_to_factors(balanced_panel_data_mar_50)
balanced_panel_data_mar_30 <- convert_to_factors(balanced_panel_data_mar_30)
balanced_panel_data_mar_10 <- convert_to_factors(balanced_panel_data_mar_10)

balanced_panel_data_mnar_50 <- convert_to_factors(balanced_panel_data_mnar_50)
balanced_panel_data_mnar_30 <- convert_to_factors(balanced_panel_data_mnar_30)
balanced_panel_data_mnar_10 <- convert_to_factors(balanced_panel_data_mnar_10)

unbalanced_panel_data_mcar_50 <- convert_to_factors(unbalanced_panel_data_mcar_50)
unbalanced_panel_data_mcar_30 <- convert_to_factors(unbalanced_panel_data_mcar_30)
unbalanced_panel_data_mcar_10 <- convert_to_factors(unbalanced_panel_data_mcar_10)

unbalanced_panel_data_mar_50 <- convert_to_factors(unbalanced_panel_data_mar_50)
unbalanced_panel_data_mar_30 <- convert_to_factors(unbalanced_panel_data_mar_30)
unbalanced_panel_data_mar_10 <- convert_to_factors(unbalanced_panel_data_mar_10)

unbalanced_panel_data_mnar_50 <- convert_to_factors(unbalanced_panel_data_mnar_50)
unbalanced_panel_data_mnar_30 <- convert_to_factors(unbalanced_panel_data_mnar_30)
unbalanced_panel_data_mnar_10 <- convert_to_factors(unbalanced_panel_data_mnar_10)

# balanced_panel_data_mcar_50
# balanced_panel_data_mcar_30
# balanced_panel_data_mcar_10
# balanced_panel_data_mar_50
# balanced_panel_data_mar_30
# balanced_panel_data_mar_10
# balanced_panel_data_mnar_50
# balanced_panel_data_mnar_30
# balanced_panel_data_mnar_10
# unbalanced_panel_data_mcar_50
# unbalanced_panel_data_mcar_30
# unbalanced_panel_data_mcar_10
# unbalanced_panel_data_mar_50
# unbalanced_panel_data_mar_30
# unbalanced_panel_data_mar_10
# unbalanced_panel_data_mnar_50
# unbalanced_panel_data_mnar_30
# unbalanced_panel_data_mnar_10

######################
## mice package
######################

library(mice)
library(broom)

Data_Imputation_mice <- function(data, m = 3, maxit = 500, method = 'pmm') {
  # Select the necessary columns
  data_temp <- data[c("Year", "Education", "Age", "IndividualIncome")]
  
  # Perform MICE imputation
  mice_imp <- mice(data_temp, method = method, m = m, maxit = maxit)
  
  # Return the imputation object
  return(mice_imp)
}

# Apply imputation
mice_balanced_mcar_50 <- Data_Imputation_mice(balanced_panel_data_mcar_50)
mice_balanced_mcar_30 <- Data_Imputation_mice(balanced_panel_data_mcar_30)
mice_balanced_mcar_10 <- Data_Imputation_mice(balanced_panel_data_mcar_10)

mice_balanced_mar_50 <- Data_Imputation_mice(balanced_panel_data_mar_50)
mice_balanced_mar_30 <- Data_Imputation_mice(balanced_panel_data_mar_30)
mice_balanced_mar_10 <- Data_Imputation_mice(balanced_panel_data_mar_10)

mice_balanced_mnar_50 <- Data_Imputation_mice(balanced_panel_data_mnar_50)
mice_balanced_mnar_30 <- Data_Imputation_mice(balanced_panel_data_mnar_30)
mice_balanced_mnar_10 <- Data_Imputation_mice(balanced_panel_data_mnar_10)

mice_unbalanced_mcar_50 <- Data_Imputation_mice(unbalanced_panel_data_mcar_50)
mice_unbalanced_mcar_30 <- Data_Imputation_mice(unbalanced_panel_data_mcar_30)
mice_unbalanced_mcar_10 <- Data_Imputation_mice(unbalanced_panel_data_mcar_10)

mice_unbalanced_mar_50 <- Data_Imputation_mice(unbalanced_panel_data_mar_50)
mice_unbalanced_mar_30 <- Data_Imputation_mice(unbalanced_panel_data_mar_30)
mice_unbalanced_mar_10 <- Data_Imputation_mice(unbalanced_panel_data_mar_10)

mice_unbalanced_mnar_50 <- Data_Imputation_mice(unbalanced_panel_data_mnar_50)
mice_unbalanced_mnar_30 <- Data_Imputation_mice(unbalanced_panel_data_mnar_30)
mice_unbalanced_mnar_10 <- Data_Imputation_mice(unbalanced_panel_data_mnar_10)

Analyze_mice <- function(mice_imp) {
  # Fit the linear model
  model <- with(mice_imp, lm(IndividualIncome ~ Year + Education + Age))
  
  # Pool the results
  pooled_results <- pool(model)
  
  # Return the summary of the pooled results
  return(summary(pooled_results))
}

# Apply analysis
summary_balanced_mcar_50 <- Analyze_mice(mice_balanced_mcar_50)
summary_balanced_mcar_30 <- Analyze_mice(mice_balanced_mcar_30)
summary_balanced_mcar_10 <- Analyze_mice(mice_balanced_mcar_10)

summary_balanced_mar_50 <- Analyze_mice(mice_balanced_mar_50)
summary_balanced_mar_30 <- Analyze_mice(mice_balanced_mar_30)
summary_balanced_mar_10 <- Analyze_mice(mice_balanced_mar_10)

summary_balanced_mnar_50 <- Analyze_mice(mice_balanced_mnar_50)
summary_balanced_mnar_30 <- Analyze_mice(mice_balanced_mnar_30)
summary_balanced_mnar_10 <- Analyze_mice(mice_balanced_mnar_10)

summary_unbalanced_mcar_50 <- Analyze_mice(mice_unbalanced_mcar_50)
summary_unbalanced_mcar_30 <- Analyze_mice(mice_unbalanced_mcar_30)
summary_unbalanced_mcar_10 <- Analyze_mice(mice_unbalanced_mcar_10)

summary_unbalanced_mar_50 <- Analyze_mice(mice_unbalanced_mar_50)
summary_unbalanced_mar_30 <- Analyze_mice(mice_unbalanced_mar_30)
summary_unbalanced_mar_10 <- Analyze_mice(mice_unbalanced_mar_10)

summary_unbalanced_mnar_50 <- Analyze_mice(mice_unbalanced_mnar_50)
summary_unbalanced_mnar_30 <- Analyze_mice(mice_unbalanced_mnar_30)
summary_unbalanced_mnar_10 <- Analyze_mice(mice_unbalanced_mnar_10)

######################
## mitml package
######################

library(mitml)
library(dplyr)
library(plm)
library(lmtest)

## Balanced Panel

# Cheching the effects

# Convert the data frame to a panel data frame
pdata_bal <- pdata.frame(balanced_panel_data, index = c("ID", "Year"))

# Estimate the fixed effects model
fe_model <- plm(IndividualIncome ~ Year + Education + Age, data = pdata_bal, model = "within")

# Estimate the random effects model
re_model <- plm(IndividualIncome ~ Year + Education + Age, data = pdata_bal, model = "random")

# Perform the Hausman test to compare the fixed and random effects models
hausman_test <- phtest(fe_model, re_model)

# Print the results of the Hausman test
print(hausman_test)
# p-value = 0.592, which is > 0.05, null hypothesis cannot be rejected.
# implying that the random effects in Education and Age is more appropriate.

# Define a function to perform the imputation, model fitting, and additional steps
Data_Imputation_mitml_Bal <- function(panel_data) {
  # Step 1: Prepare the data by selecting relevant columns
  selected_data <- panel_data[c("ID", "Year", "Education", "Age", "IndividualIncome")]
  
  # Define the type vector and assign column names
  type <- c(0, -2, 3, 3, 1)
  names(type) <- colnames(selected_data)
  
  # Impute missing data
  imputed_data <- panImpute(selected_data, type = type, n.burn = 1000, n.iter = 100, m = 3)
  
  # Extract imputed datasets
  imputed_list <- mitmlComplete(imputed_data, print = "all")
  
  # Step 2: Perform Breusch-Pagan test to check for a panel effect
  breusch_pagan_results <- lapply(imputed_list, function(x) {
    pdata <- pdata.frame(x, index = c("ID", "Year"))
    bp_test <- plmtest(plm(IndividualIncome ~ Education + Age, data = pdata, model = "pooling"), type = "bp")
    return(bp_test$p.value)
  })
  
  # Step 3: Based on Breusch-Pagan test, perform the appropriate regression
  model_list <- lapply(1:length(imputed_list), function(i) {
    pdata <- pdata.frame(imputed_list[[i]], index = c("ID", "Year"))
    if (breusch_pagan_results[[i]] > 0.05) {
      # No panel effect, proceed with Pooled OLS model
      return(plm(IndividualIncome ~ Year + Education + Age, data = pdata, model = "pooling"))
    } else {
      # Panel effect exists, proceed to Hausman test
      random_model <- plm(IndividualIncome ~ Year + Education + Age, data = pdata, model = "random")
      fixed_model <- plm(IndividualIncome ~ Year + Education + Age, data = pdata, model = "within")
      
      # Perform Hausman test
      hausman_test <- phtest(fixed_model, random_model)
      
      if (hausman_test$p.value <= 0.05) {
        # Correlation exists, use Fixed Effects Model
        print("Fixed Effect Model:")
        return(fixed_model)
      } else {
        # No correlation, use Random Effects Model
        print("Random Effect Model:")
        return(random_model)
      }
    }
  })
  
  # Step 4: Pool the results
  pooled_results <- testEstimates(model_list)
  
  # Return the pooled results summary
  return(summary(pooled_results))
}


# Apply the function to each dataset in the list and store results
Data_Imputation_mitml_Bal(balanced_panel_data_mcar_50)
Data_Imputation_mitml_Bal(balanced_panel_data_mcar_30)
Data_Imputation_mitml_Bal(balanced_panel_data_mcar_10)
Data_Imputation_mitml_Bal(balanced_panel_data_mar_50)
Data_Imputation_mitml_Bal(balanced_panel_data_mar_30)
Data_Imputation_mitml_Bal(balanced_panel_data_mar_10)
Data_Imputation_mitml_Bal(balanced_panel_data_mnar_50)
Data_Imputation_mitml_Bal(balanced_panel_data_mnar_30)
Data_Imputation_mitml_Bal(balanced_panel_data_mnar_10)

# Unbalanced Panel

# Cheching the effects

# Convert the data frame to a panel data frame
pdata_unbal <- pdata.frame(unbalanced_panel_data, index = c("ID", "Year"))

# Estimate the fixed effects model
fe_model <- plm(IndividualIncome ~ Year + Education + Age, data = pdata_unbal, model = "within")

# Estimate the random effects model
re_model <- plm(IndividualIncome ~ Year + Education + Age, data = pdata_unbal, model = "random")

# Perform the Hausman test to compare the fixed and random effects models
hausman_test <- phtest(fe_model, re_model)

# Print the results of the Hausman test
print(hausman_test)
# p-value = 9.944e-09, which is < 0.05, null hypothesis can be rejected.
# implying that the fixed effects in Education and Age is more appropriate.

# Define a function to perform the imputation, model fitting, and additional steps
Data_Imputation_mitml_Unbal <- function(panel_data) {
  
  # Step 1: Prepare the data by ungrouping and selecting relevant columns
  panel_data <- panel_data %>% 
    ungroup()
  
  selected_data <- as.data.frame(panel_data[c("ID", "Year", "Education", "Age", "IndividualIncome")])
  
  # Define the type vector and assign column names
  type <- c(0, -2, 2, 2, 1) 
  names(type) <- colnames(selected_data)
  
  # Impute missing data
  imputed_data <- panImpute(selected_data, type = type, n.burn = 1000, n.iter = 100, m = 3)
  
  # Extract imputed datasets
  imputed_list <- mitmlComplete(imputed_data, print = "all")
  
  # Step 2: Perform Breusch-Pagan test to check for a panel effect
  breusch_pagan_results <- lapply(imputed_list, function(x) {
    pdata <- pdata.frame(x, index = c("ID", "Year"))
    bp_test <- plmtest(plm(IndividualIncome ~  Year + Education + Age, data = pdata, model = "pooling"), type = "bp")
    return(bp_test$p.value)
  })
  
  # Step 3: Based on Breusch-Pagan test, perform the appropriate regression
  model_list <- lapply(1:length(imputed_list), function(i) {
    pdata <- pdata.frame(imputed_list[[i]], index = c("ID", "Year"))
    if (breusch_pagan_results[[i]] > 0.05) {
      # No panel effect, proceed with Pooled OLS model
      return(plm(IndividualIncome ~  Year + Education + Age, data = pdata, model = "pooling"))
    } else {
      # Panel effect exists, proceed to Hausman test
      random_model <- plm(IndividualIncome ~  Year + Education + Age, data = pdata, model = "random")
      fixed_model <- plm(IndividualIncome ~  Year + Education + Age, data = pdata, model = "within")
      
      # Perform Hausman test
      hausman_test <- phtest(fixed_model, random_model)
      
      if (hausman_test$p.value <= 0.05) {
        # Correlation exists, use Fixed Effects Model
        print("Fixed Effects")
        return(fixed_model)
      } else {
        # No correlation, use Random Effects Model
        print("Random Effects")
        return(random_model)
      }
    }
  })
  
  # Step 4: Pool the results
  pooled_results <- testEstimates(model_list)
  
  # Return the pooled results summary
  return(summary(pooled_results))
}

# Apply the function to each dataset in the list and store results
Data_Imputation_mitml_Unbal(unbalanced_panel_data_mcar_50)
Data_Imputation_mitml_Unbal(unbalanced_panel_data_mcar_30)
Data_Imputation_mitml_Unbal(unbalanced_panel_data_mcar_10)
Data_Imputation_mitml_Unbal(unbalanced_panel_data_mar_50)
Data_Imputation_mitml_Unbal(unbalanced_panel_data_mar_30)
Data_Imputation_mitml_Unbal(unbalanced_panel_data_mar_10)
Data_Imputation_mitml_Unbal(unbalanced_panel_data_mnar_50)
Data_Imputation_mitml_Unbal(unbalanced_panel_data_mnar_30)
Data_Imputation_mitml_Unbal(unbalanced_panel_data_mnar_10)

############################
## Amelia package
############################

library(Amelia)

# Define the function for panel data conversion and imputation
Data_Imputation_Amelia <- function(data) {
  
  # Convert the data to panel data using plm package
  pdata <- pdata.frame(data, index = c("ID", "Year"))
  pdata = pdata[c("ID", "Year", "Education", "Age", "IndividualIncome")]
  pdata$Year <- as.numeric(as.character(pdataX$Year))
  
  
  # Perform the imputation using Amelia
  amelia_fit <- amelia(
    pdata,
    m = 3,
    ts = "Year",
    cs = "ID",
    noms = "Education"
  )
  
  # Return the fitted Amelia object
  return(amelia_fit)
}

# Apply the function to each dataset and store results
amelia_bal_mcar_50 <- Data_Imputation_Amelia(balanced_panel_data_mcar_50)
amelia_bal_mcar_30 <- Data_Imputation_Amelia(balanced_panel_data_mcar_30)
amelia_bal_mcar_10 <- Data_Imputation_Amelia(balanced_panel_data_mcar_10)

amelia_bal_mar_50 <- Data_Imputation_Amelia(balanced_panel_data_mar_50)
amelia_bal_mar_30 <- Data_Imputation_Amelia(balanced_panel_data_mar_30)
amelia_bal_mar_10 <- Data_Imputation_Amelia(balanced_panel_data_mar_10)

amelia_bal_mnar_50 <- Data_Imputation_Amelia(balanced_panel_data_mnar_50)
amelia_bal_mnar_30 <- Data_Imputation_Amelia(balanced_panel_data_mnar_30)
amelia_bal_mnar_10 <- Data_Imputation_Amelia(balanced_panel_data_mnar_10)

amelia_unbal_mcar_50 <- Data_Imputation_Amelia(unbalanced_panel_data_mcar_50)
amelia_unbal_mcar_30 <- Data_Imputation_Amelia(unbalanced_panel_data_mcar_30)
amelia_unbal_mcar_10 <- Data_Imputation_Amelia(unbalanced_panel_data_mcar_10)

amelia_unbal_mar_50 <- Data_Imputation_Amelia(unbalanced_panel_data_mar_50)
amelia_unbal_mar_30 <- Data_Imputation_Amelia(unbalanced_panel_data_mar_30)
amelia_unbal_mar_10 <- Data_Imputation_Amelia(unbalanced_panel_data_mar_10)

amelia_unbal_mnar_50 <- Data_Imputation_Amelia(unbalanced_panel_data_mnar_50)
amelia_unbal_mnar_30 <- Data_Imputation_Amelia(unbalanced_panel_data_mnar_30)
amelia_unbal_mnar_10 <- Data_Imputation_Amelia(unbalanced_panel_data_mnar_10)


# Function to perform analysis on imputed data
Analyze_Amelia <- function(data) {
  
  # Step 1: Perform data imputation
  amelia_fit <- Data_Imputation_Amelia(data)
  
  # Extract imputed datasets
  imputed_list <- amelia_fit$imputations
  
  # Step 2: Perform Breusch-Pagan test to check for a panel effect
  breusch_pagan_results <- lapply(imputed_list, function(x) {
    pdata <- pdata.frame(x, index = c("ID", "Year"))
    bp_test <- plmtest(plm(IndividualIncome ~ Year + Education + Age, data = pdata, model = "pooling"), type = "bp")
    return(bp_test$p.value)
  })
  
  # Step 3: Based on Breusch-Pagan test, perform the appropriate regression
  model_list <- lapply(1:length(imputed_list), function(i) {
    pdata <- pdata.frame(imputed_list[[i]], index = c("ID", "Year"))
    if (breusch_pagan_results[[i]] > 0.05) {
      # No panel effect, proceed with Pooled OLS model
      return(plm(IndividualIncome ~ Year + Education + Age, data = pdata, model = "pooling"))
    } else {
      # Panel effect exists, proceed to Hausman test
      random_model <- plm(IndividualIncome ~ Year + Education + Age, data = pdata, model = "random")
      fixed_model <- plm(IndividualIncome ~ Year + Education + Age, data = pdata, model = "within")
      
      # Perform Hausman test
      hausman_test <- phtest(fixed_model, random_model)
      
      if (hausman_test$p.value <= 0.05) {
        # Correlation exists, use Fixed Effects Model
        print("Fixed Effects")
        return(fixed_model)
      } else {
        # No correlation, use Random Effects Model
        print("Random Effects")
        return(random_model)
      }
    }
  })
  
  # Step 4: Pool the results
  pooled_results <- testEstimates(model_list)
  
  # Return the pooled results summary
  return(summary(pooled_results))
}

# Apply the function to each dataset and store results
analyze_amelia_bal_mcar_50 <- Analyze_Amelia(balanced_panel_data_mcar_50)
analyze_amelia_bal_mcar_30 <- Analyze_Amelia(balanced_panel_data_mcar_30)
analyze_amelia_bal_mcar_10 <- Analyze_Amelia(balanced_panel_data_mcar_10)

analyze_amelia_bal_mar_50 <- Analyze_Amelia(balanced_panel_data_mar_50)
analyze_amelia_bal_mar_30 <- Analyze_Amelia(balanced_panel_data_mar_30)
analyze_amelia_bal_mar_10 <- Analyze_Amelia(balanced_panel_data_mar_10)

analyze_amelia_bal_mnar_50 <- Analyze_Amelia(balanced_panel_data_mnar_50)
analyze_amelia_bal_mnar_30 <- Analyze_Amelia(balanced_panel_data_mnar_30)
analyze_amelia_bal_mnar_10 <- Analyze_Amelia(balanced_panel_data_mnar_10)

analyze_amelia_unbal_mcar_50 <- Analyze_Amelia(unbalanced_panel_data_mcar_50)
analyze_amelia_unbal_mcar_30 <- Analyze_Amelia(unbalanced_panel_data_mcar_30)
analyze_amelia_unbal_mcar_10 <- Analyze_Amelia(unbalanced_panel_data_mcar_10)

analyze_amelia_unbal_mar_50 <- Analyze_Amelia(unbalanced_panel_data_mar_50)
analyze_amelia_unbal_mar_30 <- Analyze_Amelia(unbalanced_panel_data_mar_30)
analyze_amelia_unbal_mar_10 <- Analyze_Amelia(unbalanced_panel_data_mar_10)

analyze_amelia_unbal_mnar_50 <- Analyze_Amelia(unbalanced_panel_data_mnar_50)
analyze_amelia_unbal_mnar_30 <- Analyze_Amelia(unbalanced_panel_data_mnar_30)
analyze_amelia_unbal_mnar_10 <- Analyze_Amelia(unbalanced_panel_data_mnar_10)








############################
## LSTM Network
############################


library(keras)

# Extract necessary columns: ID, Year, Age, Education, and IndividualIncome
input_data <- balanced_panel_data_mcar_50 %>%
  select(Age, starts_with("Education"), IndividualIncome)

# Temporarily fill missing values in IndividualIncome with the column mean for training
temp_data <- input_data
temp_data$IndividualIncome[is.na(temp_data$IndividualIncome)] <- mean(temp_data$IndividualIncome, na.rm = TRUE)

# Convert the data to a 3D array suitable for LSTM
train_array <- array(as.matrix(temp_data), dim = c(nrow(temp_data), 1, ncol(temp_data)))

# Define LSTM model
model <- keras_model_sequential() %>%
  layer_lstm(units = 50, input_shape = c(1, ncol(temp_data)), return_sequences = FALSE) %>%
  layer_dense(units = 1)

# Compile the model
model %>% compile(
  loss = "mean_squared_error",
  optimizer = optimizer_adam()
)

# Fit the model (training)
history <- model %>% fit(
  x = train_array,
  y = temp_data$IndividualIncome,  # Target is the IndividualIncome column
  epochs = 50,
  batch_size = 32
)

# Predict values for the missing data
predicted_values <- model %>% predict(train_array)

# Ensure predicted values are reshaped to match the missing indices
predicted_values <- as.vector(predicted_values)  # Flatten the predicted values

# Replace missing values in the original dataset with predicted values
balanced_panel_data_mcar_50$IndividualIncome[is.na(balanced_panel_data_mcar_50$IndividualIncome)] <- predicted_values[is.na(balanced_panel_data_mcar_50$IndividualIncome)]

# The dataset now has imputed values
balanced_panel_data_mcar_50

















