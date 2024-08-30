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
## Sampling
########################



########################
## Balanced Panel
########################

# Ensure the 'Year' column is numeric or integer
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
# Sample 3455 observations for each year
sampled_data <- data %>%
  group_by(Year) %>%
  sample_n(3455)

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
balanced_panel_data_mcar_50$ID <- as.factor(balanced_panel_data_mcar_50$ID)
balanced_panel_data_mcar_50$Year <- as.factor(balanced_panel_data_mcar_50$Year)
balanced_panel_data_mcar_50$Education <- as.factor(balanced_panel_data_mcar_50$Education)

balanced_panel_data_mcar_30$ID <- as.factor(balanced_panel_data_mcar_30$ID)
balanced_panel_data_mcar_30$Year <- as.factor(balanced_panel_data_mcar_30$Year)
balanced_panel_data_mcar_30$Education <- as.factor(balanced_panel_data_mcar_30$Education)

balanced_panel_data_mcar_10$ID <- as.factor(balanced_panel_data_mcar_10$ID)
balanced_panel_data_mcar_10$Year <- as.factor(balanced_panel_data_mcar_10$Year)
balanced_panel_data_mcar_10$Education <- as.factor(balanced_panel_data_mcar_10$Education)

balanced_panel_data_mar_50$ID <- as.factor(balanced_panel_data_mar_50$ID)
balanced_panel_data_mar_50$Year <- as.factor(balanced_panel_data_mar_50$Year)
balanced_panel_data_mar_50$Education <- as.factor(balanced_panel_data_mar_50$Education)

balanced_panel_data_mar_30$ID <- as.factor(balanced_panel_data_mar_30$ID)
balanced_panel_data_mar_30$Year <- as.factor(balanced_panel_data_mar_30$Year)
balanced_panel_data_mar_30$Education <- as.factor(balanced_panel_data_mar_30$Education)

balanced_panel_data_mar_10$ID <- as.factor(balanced_panel_data_mar_10$ID)
balanced_panel_data_mar_10$Year <- as.factor(balanced_panel_data_mar_10$Year)
balanced_panel_data_mar_10$Education <- as.factor(balanced_panel_data_mar_10$Education)

balanced_panel_data_mnar_50$ID <- as.factor(balanced_panel_data_mnar_50$ID)
balanced_panel_data_mnar_50$Year <- as.factor(balanced_panel_data_mnar_50$Year)
balanced_panel_data_mnar_50$Education <- as.factor(balanced_panel_data_mnar_50$Education)

balanced_panel_data_mnar_30$ID <- as.factor(balanced_panel_data_mnar_30$ID)
balanced_panel_data_mnar_30$Year <- as.factor(balanced_panel_data_mnar_30$Year)
balanced_panel_data_mnar_30$Education <- as.factor(balanced_panel_data_mnar_30$Education)

balanced_panel_data_mnar_10$ID <- as.factor(balanced_panel_data_mnar_10$ID)
balanced_panel_data_mnar_10$Year <- as.factor(balanced_panel_data_mnar_10$Year)
balanced_panel_data_mnar_10$Education <- as.factor(balanced_panel_data_mnar_10$Education)

unbalanced_panel_data_mcar_50$ID <- as.factor(unbalanced_panel_data_mcar_50$ID)
unbalanced_panel_data_mcar_50$Year <- as.factor(unbalanced_panel_data_mcar_50$Year)
unbalanced_panel_data_mcar_50$Education <- as.factor(unbalanced_panel_data_mcar_50$Education)

unbalanced_panel_data_mcar_30$ID <- as.factor(unbalanced_panel_data_mcar_30$ID)
unbalanced_panel_data_mcar_30$Year <- as.factor(unbalanced_panel_data_mcar_30$Year)
unbalanced_panel_data_mcar_30$Education <- as.factor(unbalanced_panel_data_mcar_30$Education)

unbalanced_panel_data_mcar_10$ID <- as.factor(unbalanced_panel_data_mcar_10$ID)
unbalanced_panel_data_mcar_10$Year <- as.factor(unbalanced_panel_data_mcar_10$Year)
unbalanced_panel_data_mcar_10$Education <- as.factor(unbalanced_panel_data_mcar_10$Education)

unbalanced_panel_data_mar_50$ID <- as.factor(unbalanced_panel_data_mar_50$ID)
unbalanced_panel_data_mar_50$Year <- as.factor(unbalanced_panel_data_mar_50$Year)
unbalanced_panel_data_mar_50$Education <- as.factor(unbalanced_panel_data_mar_50$Education)

unbalanced_panel_data_mar_30$ID <- as.factor(unbalanced_panel_data_mar_30$ID)
unbalanced_panel_data_mar_30$Year <- as.factor(unbalanced_panel_data_mar_30$Year)
unbalanced_panel_data_mar_30$Education <- as.factor(unbalanced_panel_data_mar_30$Education)

unbalanced_panel_data_mar_10$ID <- as.factor(unbalanced_panel_data_mar_10$ID)
unbalanced_panel_data_mar_10$Year <- as.factor(unbalanced_panel_data_mar_10$Year)
unbalanced_panel_data_mar_10$Education <- as.factor(unbalanced_panel_data_mar_10$Education)

unbalanced_panel_data_mnar_50$ID <- as.factor(unbalanced_panel_data_mnar_50$ID)
unbalanced_panel_data_mnar_50$Year <- as.factor(unbalanced_panel_data_mnar_50$Year)
unbalanced_panel_data_mnar_50$Education <- as.factor(unbalanced_panel_data_mnar_50$Education)

unbalanced_panel_data_mnar_30$ID <- as.factor(unbalanced_panel_data_mnar_30$ID)
unbalanced_panel_data_mnar_30$Year <- as.factor(unbalanced_panel_data_mnar_30$Year)
unbalanced_panel_data_mnar_30$Education <- as.factor(unbalanced_panel_data_mnar_30$Education)

unbalanced_panel_data_mnar_10$ID <- as.factor(unbalanced_panel_data_mnar_10$ID)
unbalanced_panel_data_mnar_10$Year <- as.factor(unbalanced_panel_data_mnar_10$Year)
unbalanced_panel_data_mnar_10$Education <- as.factor(unbalanced_panel_data_mnar_10$Education)

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

# Balanced Panel

balanced_panel_data_mcar_50_temp <- balanced_panel_data_mcar_50[c("Year", "Education", "Age", "IndividualIncome")]
balanced_panel_data_mcar_50_mice_imp <- mice(balanced_panel_data_mcar_50_temp, method = 'pmm', m = 3, maxit = 500)
model <- with(balanced_panel_data_mcar_50_mice_imp, lm(IndividualIncome ~ Year + Education + Age))
balanced_panel_data_mcar_50_mice_imp_pooled_results <- pool(model)
summary(balanced_panel_data_mcar_50_mice_imp_pooled_results)

# All the imputed datasets:
imputed_data_1 <- complete(balanced_panel_data_mcar_50_mice_imp, 1)
imputed_data_2 <- complete(balanced_panel_data_mcar_50_mice_imp, 2)
imputed_data_3 <- complete(balanced_panel_data_mcar_50_mice_imp, 3)
View(imputed_data_1)
View(imputed_data_2)
View(imputed_data_3)

balanced_panel_data_mcar_30_temp <- balanced_panel_data_mcar_30[c("Year", "Education", "Age", "IndividualIncome")]
balanced_panel_data_mcar_30_mice_imp <- mice(balanced_panel_data_mcar_30_temp, m = 3, maxit = 500, method = 'pmm')
model <- with(balanced_panel_data_mcar_30_mice_imp, lm(IndividualIncome ~ Year + Education + Age))
balanced_panel_data_mcar_30_mice_imp_pooled_results <- pool(model)
summary(balanced_panel_data_mcar_30_mice_imp_pooled_results)

balanced_panel_data_mcar_10_temp <- balanced_panel_data_mcar_10[c("Year", "Education", "Age", "IndividualIncome")]
balanced_panel_data_mcar_10_mice_imp <- mice(balanced_panel_data_mcar_10_temp, m = 3, maxit = 500, method = 'pmm')
model <- with(balanced_panel_data_mcar_10_mice_imp, lm(IndividualIncome ~ Year + Education + Age))
balanced_panel_data_mcar_10_mice_imp_pooled_results <- pool(model)
summary(balanced_panel_data_mcar_10_mice_imp_pooled_results)

balanced_panel_data_mar_50_temp <- balanced_panel_data_mar_50[c("Year", "Education", "Age", "IndividualIncome")]
balanced_panel_data_mar_50_mice_imp <- mice(balanced_panel_data_mar_50_temp, m = 3, maxit = 500, method = 'pmm')
model <- with(balanced_panel_data_mar_50_mice_imp, lm(IndividualIncome ~ Year + Education + Age))
balanced_panel_data_mar_50_mice_imp_pooled_results <- pool(model)
summary(balanced_panel_data_mar_50_mice_imp_pooled_results)

balanced_panel_data_mar_30_temp <- balanced_panel_data_mar_30[c("Year", "Education", "Age", "IndividualIncome")]
balanced_panel_data_mar_30_mice_imp <- mice(balanced_panel_data_mar_30_temp, m = 3, maxit = 500, method = 'pmm')
balanced_panel_data_mar_30_mice_imp <- complete(balanced_panel_data_mar_30_mice_imp,3)
balanced_panel_data_mar_30_mice_imp

balanced_panel_data_mar_10_temp <- balanced_panel_data_mar_10[c("Year", "Education", "Age", "IndividualIncome")]
balanced_panel_data_mar_10_mice_imp <- mice(balanced_panel_data_mar_10_temp, m = 3, maxit = 500, method = 'pmm')
model <- with(balanced_panel_data_mar_10_mice_imp, lm(IndividualIncome ~ Year + Education + Age))
balanced_panel_data_mar_10_mice_imp_pooled_results <- pool(model)
summary(balanced_panel_data_mar_10_mice_imp_pooled_results)

balanced_panel_data_mnar_50_temp <- balanced_panel_data_mnar_50[c("Year", "Education", "Age", "IndividualIncome")]
balanced_panel_data_mnar_50_mice_imp <- mice(balanced_panel_data_mnar_50_temp, m = 3, maxit = 500, method = 'pmm')
model <- with(balanced_panel_data_mnar_50_mice_imp, lm(IndividualIncome ~ Year + Education + Age))
balanced_panel_data_mnar_50_mice_imp_pooled_results <- pool(model)
summary(balanced_panel_data_mnar_50_mice_imp_pooled_results)

balanced_panel_data_mnar_30_temp <- balanced_panel_data_mnar_30[c("Year", "Education", "Age", "IndividualIncome")]
balanced_panel_data_mnar_30_mice_imp <- mice(balanced_panel_data_mnar_30_temp, m = 3, maxit = 500, method = 'pmm')
model <- with(balanced_panel_data_mnar_30_mice_imp, lm(IndividualIncome ~ Year + Education + Age))
balanced_panel_data_mnar_30_mice_imp_pooled_results <- pool(model)
summary(balanced_panel_data_mnar_30_mice_imp_pooled_results)

balanced_panel_data_mnar_10_temp <- balanced_panel_data_mnar_10[c("Year", "Education", "Age", "IndividualIncome")]
balanced_panel_data_mnar_10_mice_imp <- mice(balanced_panel_data_mnar_10_temp, m = 3, maxit = 500, method = 'pmm')
model <- with(balanced_panel_data_mnar_10_mice_imp, lm(IndividualIncome ~ Year + Education + Age))
balanced_panel_data_mnar_10_mice_imp_pooled_results <- pool(model)
summary(balanced_panel_data_mnar_10_mice_imp_pooled_results)

# Unbalanced Panel

unbalanced_panel_data_mcar_50_temp <- unbalanced_panel_data_mcar_50[c("Year", "Education", "Age", "IndividualIncome")]
unbalanced_panel_data_mcar_50_mice_imp <- mice(unbalanced_panel_data_mcar_50_temp, m = 3, maxit = 500, method = 'pmm')
model <- with(unbalanced_panel_data_mcar_50_mice_imp, lm(IndividualIncome ~ Year + Education + Age))
unbalanced_panel_data_mcar_50_mice_imp_pooled_results <- pool(model)
summary(unbalanced_panel_data_mcar_50_mice_imp_pooled_results)

unbalanced_panel_data_mcar_30_temp <- unbalanced_panel_data_mcar_30[c("Year", "Education", "Age", "IndividualIncome")]
unbalanced_panel_data_mcar_30_mice_imp <- mice(unbalanced_panel_data_mcar_30_temp, m = 3, maxit = 500, method = 'pmm')
model <- with(unbalanced_panel_data_mcar_30_mice_imp, lm(IndividualIncome ~ Year + Education + Age))
unbalanced_panel_data_mcar_30_mice_imp_pooled_results <- pool(model)
summary(unbalanced_panel_data_mcar_30_mice_imp_pooled_results)

unbalanced_panel_data_mcar_10_temp <- unbalanced_panel_data_mcar_10[c("Year", "Education", "Age", "IndividualIncome")]
unbalanced_panel_data_mcar_10_mice_imp <- mice(unbalanced_panel_data_mcar_10_temp, m = 3, maxit = 500, method = 'pmm')
model <- with(unbalanced_panel_data_mcar_10_mice_imp, lm(IndividualIncome ~ Year + Education + Age))
unbalanced_panel_data_mcar_10_mice_imp_pooled_results <- pool(model)
summary(unbalanced_panel_data_mcar_10_mice_imp_pooled_results)

unbalanced_panel_data_mar_50_temp <- unbalanced_panel_data_mar_50[c("Year", "Education", "Age", "IndividualIncome")]
unbalanced_panel_data_mar_50_mice_imp <- mice(unbalanced_panel_data_mar_50_temp, m = 3, maxit = 500, method = 'pmm')
model <- with(unbalanced_panel_data_mar_50_mice_imp, lm(IndividualIncome ~ Year + Education + Age))
unbalanced_panel_data_mar_50_mice_imp_pooled_results <- pool(model)
summary(unbalanced_panel_data_mar_50_mice_imp_pooled_results)

unbalanced_panel_data_mar_30_temp <- unbalanced_panel_data_mar_30[c("Year", "Education", "Age", "IndividualIncome")]
unbalanced_panel_data_mar_30_mice_imp <- mice(unbalanced_panel_data_mar_30_temp, m = 3, maxit = 500, method = 'pmm')
model <- with(unbalanced_panel_data_mar_30_mice_imp, lm(IndividualIncome ~ Year + Education + Age))
unbalanced_panel_data_mar_30_mice_imp_pooled_results <- pool(model)
summary(unbalanced_panel_data_mar_30_mice_imp_pooled_results)

unbalanced_panel_data_mar_10_temp <- unbalanced_panel_data_mar_10[c("Year", "Education", "Age", "IndividualIncome")]
unbalanced_panel_data_mar_10_mice_imp <- mice(unbalanced_panel_data_mar_10_temp, m = 3, maxit = 500, method = 'pmm')
model <- with(unbalanced_panel_data_mar_10_mice_imp, lm(IndividualIncome ~ Year + Education + Age))
unbalanced_panel_data_mar_10_mice_imp_pooled_results <- pool(model)
summary(unbalanced_panel_data_mar_10_mice_imp_pooled_results)

unbalanced_panel_data_mnar_50_temp <- unbalanced_panel_data_mnar_50[c("Year", "Education", "Age", "IndividualIncome")]
unbalanced_panel_data_mnar_50_mice_imp <- mice(unbalanced_panel_data_mnar_50_temp, m = 3, maxit = 500, method = 'pmm')
model <- with(unbalanced_panel_data_mnar_50_mice_imp, lm(IndividualIncome ~ Year + Education + Age))
unbalanced_panel_data_mnar_50_mice_imp_pooled_results <- pool(model)
summary(unbalanced_panel_data_mnar_50_mice_imp_pooled_results)

unbalanced_panel_data_mnar_30_temp <- unbalanced_panel_data_mnar_30[c("Year", "Education", "Age", "IndividualIncome")]
unbalanced_panel_data_mnar_30_mice_imp <- mice(unbalanced_panel_data_mnar_30_temp, m = 3, maxit = 500, method = 'pmm')
model <- with(unbalanced_panel_data_mnar_30_mice_imp, lm(IndividualIncome ~ Year + Education + Age))
unbalanced_panel_data_mnar_30_mice_imp_pooled_results <- pool(model)
summary(unbalanced_panel_data_mnar_30_mice_imp_pooled_results)

unbalanced_panel_data_mnar_10_temp <- unbalanced_panel_data_mnar_10[c("Year", "Education", "Age", "IndividualIncome")]
unbalanced_panel_data_mnar_10_mice_imp <- mice(unbalanced_panel_data_mnar_10_temp, m = 3, maxit = 500, method = 'pmm')
model <- with(unbalanced_panel_data_mnar_10_mice_imp, lm(IndividualIncome ~ Year + Education + Age))
unbalanced_panel_data_mnar_10_mice_imp_pooled_results <- pool(model)
summary(unbalanced_panel_data_mnar_10_mice_imp_pooled_results)

######################
## mitml package
######################

library(mitml)
library(dplyr)

# Balanced Panel

# Define a function to perform the imputation and model fitting
process_panel_data <- function(panel_data) {
  # Select relevant columns
  selected_data <- panel_data[c("ID", "Year", "Education", "Age", "IndividualIncome")]
  
  # Define the type vector and assign column names
  type <- c(0, -2, 2, 2, 1)
  names(type) <- colnames(selected_data)
  
  # Impute missing data
  imputed_data <- panImpute(selected_data, type = type, n.burn = 1000, n.iter = 100, m = 3)
  
  # Extract imputed datasets
  imputed_list <- mitmlComplete(imputed_data, print = "all")
  
  # Fit the model on each imputed dataset
  model_list <- lapply(imputed_list, function(x) {
    lm(IndividualIncome ~ Year + Education + Age, data = x)
  })
  
  # Pool the results
  pooled_results <- testEstimates(model_list)
  
  # Return the pooled results summary
  return(summary(pooled_results))
}

# Apply the function to each dataset in the list and store results
process_panel_data(balanced_panel_data_mcar_50)
process_panel_data(balanced_panel_data_mcar_30)
process_panel_data(balanced_panel_data_mcar_10)
process_panel_data(balanced_panel_data_mar_50)
process_panel_data(balanced_panel_data_mar_30)
process_panel_data(balanced_panel_data_mar_10)
process_panel_data(balanced_panel_data_mnar_50)
process_panel_data(balanced_panel_data_mnar_30)
process_panel_data(balanced_panel_data_mnar_10)

# Unbalanced Panel

# Define a function to perform the imputation and model fitting
process_panel_data <- function(panel_data) {
  
  # Ungroup the data
  panel_data <- panel_data %>% 
    ungroup()
  
  # Optionally convert to a standard data frame
  selected_data <- as.data.frame(panel_data[c("ID", "Year", "Education", "Age", "IndividualIncome")])
  
  # Define the type vector and assign column names
  type <- c(0, -2, 2, 2, 1)
  names(type) <- colnames(selected_data)
  
  # Impute missing data
  imputed_data <- panImpute(selected_data, type = type, n.burn = 1000, n.iter = 100, m = 3)
  
  # Extract imputed datasets
  imputed_list <- mitmlComplete(imputed_data, print = "all")
  
  # Fit the model on each imputed dataset
  model_list <- lapply(imputed_list, function(x) {
    lm(IndividualIncome ~ Year + Education + Age, data = x)
  })
  
  # Pool the results
  pooled_results <- testEstimates(model_list)
  
  # Return the pooled results summary
  return(summary(pooled_results))
}

# Apply the function to each dataset in the list and store results
process_panel_data(unbalanced_panel_data_mcar_50)
process_panel_data(unbalanced_panel_data_mcar_30)
process_panel_data(unbalanced_panel_data_mcar_10)
process_panel_data(unbalanced_panel_data_mar_50)
process_panel_data(unbalanced_panel_data_mar_30)
process_panel_data(unbalanced_panel_data_mar_10)
process_panel_data(unbalanced_panel_data_mnar_50)
process_panel_data(unbalanced_panel_data_mnar_30)
process_panel_data(unbalanced_panel_data_mnar_10)





############################
## LSTM Network
############################

























