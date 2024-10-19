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
library(ggplot2)
library(gridExtra)

# Load and clean the data
RawData <- readRDS("population.RDS")
RawData = data.frame(RawData)
data = RawData[c("id", "year","EF310", "EF44", "inc.ind")]
colnames(data) <- c("ID", "Year", "Education", "Age", "IndividualIncome")
summary(data)

# 'ID'
count(data, ID)

# 'Year'
count(data, Year)
data <- subset(data, Year >= 2013 & Year <= 2023) # Keeping the data from 2013 to 2023

# 'Education': Highest general school degree
count(data, Education)
data$Education[is.na(data$Education)] <- 7

# Age
count(data, Age)
summary(data$Age)

# Density curve for Age
AgeDist <- ggplot(data, aes(x = Age)) + 
  geom_density() +
  labs(title = "Density Curve of Age", x = "Age", y = "Density") +
  theme_minimal()
AgeDist

# Cleaning income column
summary(data$IndividualIncome)
count(data, IndividualIncome)
data$IndividualIncome[is.na(data$IndividualIncome)] <- 0
summary(data$IndividualIncome)

# Histogram for Individual Income
IndividualIncomeHist <- ggplot(data, aes(x = IndividualIncome)) + 
  geom_histogram(binwidth = 1000) +
  labs(title = "Individual Income", x = "Individual Income", y = "Frequency") +
  theme_minimal()
IndividualIncomeHist

# Apply log transformation to IndividualIncome (adding 1 to avoid log(0))
data$LogIndividualIncome <- log(data$IndividualIncome + 1)

IndividualIncomeLogHist <- ggplot(data, aes(x = LogIndividualIncome)) + 
  geom_histogram(binwidth = .10) +
  labs(title = "Logged Individual Income", x = "Logged Individual Income", y = "Frequency")+
  theme_minimal()
IndividualIncomeLogHist

DataTempAge <- data[data$Age >= 18 & data$Age <= 67, ]
AFilteredIncomeLogHist <- ggplot(DataTempAge, aes(x = LogIndividualIncome)) + 
  geom_histogram(binwidth = .10) +
  labs(title = "Logged Individual Income (Age: 18-67)", x = "Logged Individual Income", y = "Frequency")+
  theme_minimal()
AFilteredIncomeLogHist

DataTempIncome <- data[data$IndividualIncome != 0, ]
IFilteredIncomeLogHist <- ggplot(DataTempIncome, aes(x = LogIndividualIncome)) + 
  geom_histogram(binwidth = .10) +
  labs(title = "Logged Individual Income (Without 0 Income)", x = "Logged Individual Income", y = "Frequency")+
  theme_minimal()
IFilteredIncomeLogHist

grid.arrange(IndividualIncomeHist, IndividualIncomeLogHist, AFilteredIncomeLogHist, IFilteredIncomeLogHist, ncol = 2)

sum(is.na(data)) #Total number of missing values
data <- data[c("ID", "Year", "Education", "Age", "LogIndividualIncome")]
colnames(data)[colnames(data) == "LogIndividualIncome"] <- "Income"
#data <- data[c("ID", "Year", "Education", "Age", "IndividualIncome")]
#colnames(data)[colnames(data) == "IndividualIncome"] <- "Income"
summary(data)

########################
## Balanced Panel
########################

data$Year <- as.numeric(data$Year) # Convert Year column to make the calculation easy

# Find IDs that are present in all years between 2013 and 2023
years <- 2013:2023
common_ids <- Reduce(intersect, lapply(years, function(year) {
  unique(data$ID[data$Year == year])
}))

panel_data <- data[data$ID %in% common_ids, ] # Filter the data to keep only rows with common IDs

# Count the occurrence of each ID
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

is_balanced_panel <- all(id_year_count$Year == length(2013:2023)) # Check if every ID appears in all the years (2013 to 2023)

if (is_balanced_panel) {
  print("Balanced Panel Data")
} else {
  print("Unbalanced Panel Data")
}

# Print the IDs that do not appear in all years (optional task)
unbalanced_ids <- id_year_count$ID[id_year_count$Year != length(2013:2023)]
if (length(unbalanced_ids) > 0) {
  print("IDs that are not present in all years:")
  print(unbalanced_ids)
}

# Count the occurrence of each ID
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

is_balanced_panel <- all(id_year_count$Year == length(2013:2023)) # Check if every ID appears in all the years (2013 to 2023)


if (is_balanced_panel) {
  print("The data is a balanced panel.")
} else {
  print("The data is not a balanced panel.")
}

# Print the IDs that do not appear in all years
unbalanced_ids <- id_year_count$ID[id_year_count$Year != length(2013:2023)]
if (length(unbalanced_ids) > 0) {
  print("IDs that do not appear in all years:")
  print(unbalanced_ids)
}

print(unbalanced_panel_data[duplicated(unbalanced_panel_data), ]) # Check for duplicate rows in the data

count(unbalanced_panel_data, Year)
summary(unbalanced_panel_data)

########################
## Simulate Missingness in Balanced Panel
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

# Visualize the missing data pattern
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

#### 10% ####

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

mis_simulated_mar_50 <- mis_simulated_mar_50 < quantile(mis_simulated_mar_50, p_mis_50) # All below the 50% quantile are set to missing
mean(as.numeric(mis_simulated_mar_50))
balanced_panel_data_mar_50$Income[mis_simulated_mar_50] <- NA # Set values to NA in Income where missingness occurs
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

mis_simulated_mar_30 <- mis_simulated_mar_30 < quantile(mis_simulated_mar_30, p_mis_30) # All below the 50% quantile are set to missing
mean(as.numeric(mis_simulated_mar_30))
balanced_panel_data_mar_30$Income[mis_simulated_mar_30] <- NA # Set values to NA in IndividualIncome where missingness occurs
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

mis_simulated_mar_10 <- mis_simulated_mar_10 < quantile(mis_simulated_mar_10, p_mis_10) # All below the 50% quantile are set to missing
mean(as.numeric(mis_simulated_mar_10))
balanced_panel_data_mar_10$Income[mis_simulated_mar_10] <- NA # Set values to NA in Income where missingness occurs
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
# the missing of a value now also depends on Income itself
mis_simulated_mnar_50 <- 0.5 + .2 * balanced_panel_data_mnar_50$Education + 
  0.1 * balanced_panel_data_mnar_50$Age + 
  0.5 * balanced_panel_data_mnar_50$Income + rnorm(nrow(balanced_panel_data), 0, 3)
mis_simulated_mnar_50 <- mis_simulated_mnar_50 < quantile(mis_simulated_mnar_50, p_mis_50)
balanced_panel_data_mnar_50$Income[mis_simulated_mnar_50] <- NA
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
# the missing of a value now also depends on Income itself
mis_simulated_mnar_30 <- 0.7 + 0.1 * balanced_panel_data_mnar_30$Education + 
  0.2 * balanced_panel_data_mnar_30$Age + 
  0.5 * balanced_panel_data_mnar_30$Income + rnorm(nrow(balanced_panel_data), 0, 3)
mis_simulated_mnar_30 <- mis_simulated_mnar_30 < quantile(mis_simulated_mnar_30, p_mis_30)
balanced_panel_data_mnar_30$Income[mis_simulated_mnar_30] <- NA
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
# the missing of a value now also depends on Income itself
mis_simulated_mnar_10 <- 0.1 + 0.1 * balanced_panel_data_mnar_10$Education + 
  0.2 * balanced_panel_data_mnar_10$Age + 
  0.5 * balanced_panel_data_mnar_10$Income + rnorm(nrow(balanced_panel_data), 0, 3)
mis_simulated_mnar_10 <- mis_simulated_mnar_10 < quantile(mis_simulated_mnar_10, p_mis_10)
balanced_panel_data_mnar_10$Income[mis_simulated_mnar_10] <- NA
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

mis_simulated_mar_50 <- mis_simulated_mar_50 < quantile(mis_simulated_mar_50, p_mis_50) # All below the 50% quantile are set to missing
mean(as.numeric(mis_simulated_mar_50))

unbalanced_panel_data_mar_50$Income[mis_simulated_mar_50] <- NA # Set values to NA in Income where missingness occurs
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

mis_simulated_mar_30 <- mis_simulated_mar_30 < quantile(mis_simulated_mar_30, p_mis_30) # All below the 50% quantile are set to missing
mean(as.numeric(mis_simulated_mar_30))

unbalanced_panel_data_mar_30$Income[mis_simulated_mar_30] <- NA # Set values to NA in Income where missingness occurs
summary(unbalanced_panel_data_mar_30) # Summary of Income after introducing missingness

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

mis_simulated_mar_10 <- mis_simulated_mar_10 < quantile(mis_simulated_mar_10, p_mis_10) # All below the 50% quantile are set to missing
mean(as.numeric(mis_simulated_mar_10))

unbalanced_panel_data_mar_10$Income[mis_simulated_mar_10] <- NA # Set values to NA in Income where missingness occurs
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
  0.5 * unbalanced_panel_data_mnar_50$Income + rnorm(nrow(unbalanced_panel_data), 0, 3)
mis_simulated_mnar_50 <- mis_simulated_mnar_50 < quantile(mis_simulated_mnar_50, p_mis_50)
unbalanced_panel_data_mnar_50$Income[mis_simulated_mnar_50] <- NA
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
  0.5 * unbalanced_panel_data_mnar_30$Income + rnorm(nrow(unbalanced_panel_data), 0, 3)
mis_simulated_mnar_30 <- mis_simulated_mnar_30 < quantile(mis_simulated_mnar_30, p_mis_30)
unbalanced_panel_data_mnar_30$Income[mis_simulated_mnar_30] <- NA
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
  0.5 * unbalanced_panel_data_mnar_10$Income + rnorm(nrow(unbalanced_panel_data), 0, 3)
mis_simulated_mnar_10 <- mis_simulated_mnar_10 < quantile(mis_simulated_mnar_10, p_mis_10)
unbalanced_panel_data_mnar_10$Income[mis_simulated_mnar_10] <- NA
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

#####################################
### Data Sets Overview and Formating
####################################

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
packageVersion("mice")

StartTime_mice <- Sys.time()  # Starting time

Data_Imputation_mice <- function(data, m = 3, maxit = 20, method = 'pmm') {
  data_temp <- data[c("Year", "Education", "Age", "Income")]
  mice_imp <- mice(data_temp, method = method, m = m, maxit = maxit) # Perform MICE imputation
  return(mice_imp)
}

# Apply imputation
mice_bal_mcar_50 <- Data_Imputation_mice(balanced_panel_data_mcar_50)
mice_bal_mcar_30 <- Data_Imputation_mice(balanced_panel_data_mcar_30)
mice_bal_mcar_10 <- Data_Imputation_mice(balanced_panel_data_mcar_10)

mice_bal_mar_50 <- Data_Imputation_mice(balanced_panel_data_mar_50)
mice_bal_mar_30 <- Data_Imputation_mice(balanced_panel_data_mar_30)
mice_bal_mar_10 <- Data_Imputation_mice(balanced_panel_data_mar_10)

mice_bal_mnar_50 <- Data_Imputation_mice(balanced_panel_data_mnar_50)
mice_bal_mnar_30 <- Data_Imputation_mice(balanced_panel_data_mnar_30)
mice_bal_mnar_10 <- Data_Imputation_mice(balanced_panel_data_mnar_10)

mice_unbal_mcar_50 <- Data_Imputation_mice(unbalanced_panel_data_mcar_50)
mice_unbal_mcar_30 <- Data_Imputation_mice(unbalanced_panel_data_mcar_30)
mice_unbal_mcar_10 <- Data_Imputation_mice(unbalanced_panel_data_mcar_10)

mice_unbal_mar_50 <- Data_Imputation_mice(unbalanced_panel_data_mar_50)
mice_unbal_mar_30 <- Data_Imputation_mice(unbalanced_panel_data_mar_30)
mice_unbal_mar_10 <- Data_Imputation_mice(unbalanced_panel_data_mar_10)

mice_unbal_mnar_50 <- Data_Imputation_mice(unbalanced_panel_data_mnar_50)
mice_unbal_mnar_30 <- Data_Imputation_mice(unbalanced_panel_data_mnar_30)
mice_unbal_mnar_10 <- Data_Imputation_mice(unbalanced_panel_data_mnar_10)

Analyze_mice <- function(mice_imp) {
  model <- with(mice_imp, lm(Income ~ Year + Education + Age))   # Fit the linear model
  pooled_results <- pool(model) # Pool the results
  return(summary(pooled_results))
}

# Apply analysis
analyze_mice_bal_mcar_50 <- Analyze_mice(mice_bal_mcar_50)
analyze_mice_bal_mcar_30 <- Analyze_mice(mice_bal_mcar_30)
analyze_mice_bal_mcar_10 <- Analyze_mice(mice_bal_mcar_10)

analyze_mice_bal_mar_50 <- Analyze_mice(mice_bal_mar_50)
analyze_mice_bal_mar_30 <- Analyze_mice(mice_bal_mar_30)
analyze_mice_bal_mar_10 <- Analyze_mice(mice_bal_mar_10)

analyze_mice_bal_mnar_50 <- Analyze_mice(mice_bal_mnar_50)
analyze_mice_bal_mnar_30 <- Analyze_mice(mice_bal_mnar_30)
analyze_mice_bal_mnar_10 <- Analyze_mice(mice_bal_mnar_10)

analyze_mice_unbal_mcar_50 <- Analyze_mice(mice_unbal_mcar_50)
analyze_mice_unbal_mcar_30 <- Analyze_mice(mice_unbal_mcar_30)
analyze_mice_unbal_mcar_10 <- Analyze_mice(mice_unbal_mcar_10)

analyze_mice_unbal_mar_50 <- Analyze_mice(mice_unbal_mar_50)
analyze_mice_unbal_mar_30 <- Analyze_mice(mice_unbal_mar_30)
analyze_mice_unbal_mar_10 <- Analyze_mice(mice_unbal_mar_10)

analyze_mice_unbal_mnar_50 <- Analyze_mice(mice_unbal_mnar_50)
analyze_mice_unbal_mnar_30 <- Analyze_mice(mice_unbal_mnar_30)
analyze_mice_unbal_mnar_10 <- Analyze_mice(mice_unbal_mnar_10)

EndTime_mice <- Sys.time()  # Ending time

ExecutionTime_mice <- EndTime_mice - StartTime_mice
print(ExecutionTime_mice) # Time difference of 1.432353 mins

######################
## mitml package
######################

library(mitml)
library(dplyr)
library(plm)
library(lmtest)
packageVersion("mitml")

## Balanced Panel##
###################

pdata_bal <- pdata.frame(balanced_panel_data, index = c("ID", "Year")) # Convert the data frame to a panel data frame

# Estimate the models
fe_model <- plm(Income ~ Year + Education + Age, data = pdata_bal, model = "within")
re_model <- plm(Income ~ Year + Education + Age, data = pdata_bal, model = "random")

hausman_test <- phtest(fe_model, re_model) # Perform the Hausman test to compare the fixed and random effects models
print(hausman_test)
# p-value is 0.2185, which is > 0.05. S0 the null hypothesis cannot be rejected.
# This implies that the random effects in Education and Age is more appropriate.

StartTime_mitml <- Sys.time()  # Starting time

# Function to impute data
Data_Imputation_mitml_Bal <- function(panel_data) {
  
  selected_data <- panel_data[c("ID", "Year", "Education", "Age", "Income")]
  
  # Define the type vector and assign column names
  type <- c(0, -2, 3, 3, 1)
  names(type) <- colnames(selected_data)
  
  imputed_data <- panImpute(selected_data, type = type, n.burn = 1000, n.iter = 100, m = 3)   # Impute missing data
  imputed_list <- mitmlComplete(imputed_data, print = "all")
  return(imputed_list)
}


# Apply the function to each dataset
mitml_bal_mcar_50 <- Data_Imputation_mitml_Bal(balanced_panel_data_mcar_50)
mitml_bal_mcar_30 <- Data_Imputation_mitml_Bal(balanced_panel_data_mcar_30)
mitml_bal_mcar_10 <- Data_Imputation_mitml_Bal(balanced_panel_data_mcar_10)

mitml_bal_mar_50 <- Data_Imputation_mitml_Bal(balanced_panel_data_mar_50)
mitml_bal_mar_30 <- Data_Imputation_mitml_Bal(balanced_panel_data_mar_30)
mitml_bal_mar_10 <- Data_Imputation_mitml_Bal(balanced_panel_data_mar_10)

mitml_bal_mnar_50 <- Data_Imputation_mitml_Bal(balanced_panel_data_mnar_50)
mitml_bal_mnar_30 <- Data_Imputation_mitml_Bal(balanced_panel_data_mnar_30)
mitml_bal_mnar_10 <- Data_Imputation_mitml_Bal(balanced_panel_data_mnar_10)

# Function to extract coefficients
Analyze_mitml_Bal <- function(imputed_list) {
  # Step 1: Impute dataset
  
  # Step 2: Perform Breusch-Pagan test to check the panel effect
  breusch_pagan_results <- lapply(imputed_list, function(x) {
    pdata <- pdata.frame(x, index = c("ID", "Year"))
    bp_test <- plmtest(plm(Income ~ Education + Age, data = pdata, model = "pooling"), type = "bp")
    return(bp_test$p.value)
  })
  
  # Step 3: Based on Breusch-Pagan test, perform the appropriate regression
  model_list <- lapply(1:length(imputed_list), function(i) {
    pdata <- pdata.frame(imputed_list[[i]], index = c("ID", "Year"))
    if (breusch_pagan_results[[i]] > 0.05) {
      # No panel effect, proceed with Pooled OLS model
      return(plm(Income ~ Year + Education + Age, data = pdata, model = "pooling"))
    } else {
      # Panel effect exists, proceed to Hausman test
      random_model <- plm(Income ~ Year + Education + Age, data = pdata, model = "random")
      fixed_model <- plm(Income ~ Year + Education + Age, data = pdata, model = "within")
      
      
      hausman_test <- phtest(fixed_model, random_model) # Perform Hausman test
      
      if (hausman_test$p.value <= 0.05) {
        print("Fixed Effect Model:")
        return(fixed_model) # Use Fixed Effects model if correlation exists
      } else {
        print("Random Effect Model:")
        return(random_model) # Use Random Effect model if corellation does not exists
      }
    }
  })
  
  # Step 4: Pool the coefficients and return summary
  pooled_results <- testEstimates(model_list)
  return(pooled_results)
}

# Apply the function to each dataset and store results
analyze_mitml_bal_mcar_50 <- Analyze_mitml_Bal(mitml_bal_mcar_50)
analyze_mitml_bal_mcar_30 <- Analyze_mitml_Bal(mitml_bal_mcar_30)
analyze_mitml_bal_mcar_10 <- Analyze_mitml_Bal(mitml_bal_mcar_10)

analyze_mitml_bal_mar_50 <- Analyze_mitml_Bal(mitml_bal_mar_50)
analyze_mitml_bal_mar_30 <- Analyze_mitml_Bal(mitml_bal_mar_30)
analyze_mitml_bal_mar_10 <- Analyze_mitml_Bal(mitml_bal_mar_10)

analyze_mitml_bal_mnar_50 <- Analyze_mitml_Bal(mitml_bal_mnar_50)
analyze_mitml_bal_mnar_30 <- Analyze_mitml_Bal(mitml_bal_mnar_30)
analyze_mitml_bal_mnar_10 <- Analyze_mitml_Bal(mitml_bal_mnar_10)

## Unbalanced Panel ##
#####################

pdata_unbal <- pdata.frame(unbalanced_panel_data, index = c("ID", "Year")) # Convert the data frame to a panel data frame

# Estimate the models
fe_model <- plm(Income ~ Year + Education + Age, data = pdata_unbal, model = "within")
re_model <- plm(Income ~ Year + Education + Age, data = pdata_unbal, model = "random")

hausman_test <- phtest(fe_model, re_model) # Perform the Hausman test to compare the fixed and random effects models
print(hausman_test)
# p-value is 2.2e-16, which is < 0.05. So null hypothesis can be rejected.
# implying that the fixed effects in Education and Age is more appropriate.

# Function to impute data for unbalanced panels
Data_Imputation_mitml_Unbal <- function(panel_data) {
  # Prepare the data by ungrouping and selecting relevant columns
  panel_data <- panel_data %>% 
    ungroup()
  
  selected_data <- as.data.frame(panel_data[c("ID", "Year", "Education", "Age", "Income")])
  
  # Define the type vector and assign column names
  type <- c(0, -2, 2, 2, 1) 
  names(type) <- colnames(selected_data)
  
  imputed_data <- panImpute(selected_data, type = type, n.burn = 1000, n.iter = 100, m = 3)   # Impute missing data
  imputed_list <- mitmlComplete(imputed_data, print = "all")   # Extract imputed datasets
  return(imputed_list)
}

# Apply the function to each dataset
mitml_unbal_mcar_50 <- Data_Imputation_mitml_Unbal(unbalanced_panel_data_mcar_50)
mitml_unbal_mcar_30 <- Data_Imputation_mitml_Unbal(unbalanced_panel_data_mcar_30)
mitml_unbal_mcar_10 <- Data_Imputation_mitml_Unbal(unbalanced_panel_data_mcar_10)

mitml_unbal_mar_50 <- Data_Imputation_mitml_Unbal(unbalanced_panel_data_mar_50)
mitml_unbal_mar_30 <- Data_Imputation_mitml_Unbal(unbalanced_panel_data_mar_30)
mitml_unbal_mar_10 <- Data_Imputation_mitml_Unbal(unbalanced_panel_data_mar_10)

mitml_unbal_mnar_50 <- Data_Imputation_mitml_Unbal(unbalanced_panel_data_mnar_50)
mitml_unbal_mnar_30 <- Data_Imputation_mitml_Unbal(unbalanced_panel_data_mnar_30)
mitml_unbal_mnar_10 <- Data_Imputation_mitml_Unbal(unbalanced_panel_data_mnar_10)

# Function to analyze imputed data and extract coefficients for unbalanced panels
Analyze_mitml_Unbal <- function(imputed_list) {
  # Step 01: Impute the dataset
  
  # Step 2: Perform Breusch-Pagan test to check for a panel effect
  breusch_pagan_results <- lapply(imputed_list, function(x) {
    pdata <- pdata.frame(x, index = c("ID", "Year"))
    bp_test <- plmtest(plm(Income ~ Education + Age, data = pdata, model = "pooling"), type = "bp")
    return(bp_test$p.value)
  })
  
  # Step 3: Based on Breusch-Pagan test, perform the appropriate regression
  model_list <- lapply(1:length(imputed_list), function(i) {
    pdata <- pdata.frame(imputed_list[[i]], index = c("ID", "Year"))
    if (breusch_pagan_results[[i]] > 0.05) {
      # Pooled OLS for no panel effect
      return(plm(Income ~ Year + Education + Age, data = pdata, model = "pooling"))
    } else {
      # Hausman test if panel effect exists
      random_model <- plm(Income ~ Year + Education + Age, data = pdata, model = "random")
      fixed_model <- plm(Income ~ Year + Education + Age, data = pdata, model = "within")
      
      hausman_test <- phtest(fixed_model, random_model) # Hausman test
      
      if (hausman_test$p.value <= 0.05) {
        print("Fixed Effects")
        return(fixed_model) # Fixed Effect model if correlation exist
        
      } else {
        print("Random Effects")
        return(random_model) # Random Effect model if the correlation does not exists
      }
    }
  })
  
  # Step 4: Pool and the coefficients
  pooled_results <- testEstimates(model_list)
  
  # Return the pooled results summary
  return(pooled_results)
}

# Apply the analysis function to each set of imputed datasets
analyze_mitml_unbal_mcar_50 <- Analyze_mitml_Unbal(mitml_unbal_mcar_50)
analyze_mitml_unbal_mcar_30 <- Analyze_mitml_Unbal(mitml_unbal_mcar_30)
analyze_mitml_unbal_mcar_10 <- Analyze_mitml_Unbal(mitml_unbal_mcar_10)

analyze_mitml_unbal_mar_50 <- Analyze_mitml_Unbal(mitml_unbal_mar_50)
analyze_mitml_unbal_mar_30 <- Analyze_mitml_Unbal(mitml_unbal_mar_30)
analyze_mitml_unbal_mar_10 <- Analyze_mitml_Unbal(mitml_unbal_mar_10)

analyze_mitml_unbal_mnar_50 <- Analyze_mitml_Unbal(mitml_unbal_mnar_50)
analyze_mitml_unbal_mnar_30 <- Analyze_mitml_Unbal(mitml_unbal_mnar_30)
analyze_mitml_unbal_mnar_10 <- Analyze_mitml_Unbal(mitml_unbal_mnar_10)

EndTime_mitml <- Sys.time()  # Ending time

ExecutionTime_mitml <- EndTime_mitml - StartTime_mitml
print(ExecutionTime_mitml) # Time difference of 1.419402 mins

############################
## Amelia package
############################

library(Amelia)
packageVersion("Amelia")

# Function to impute data
Data_Imputation_Amelia <- function(data) {
  
  # Convert the data to panel data using plm package
  pdata <- pdata.frame(data, index = c("ID", "Year"))
  pdata = pdata[c("ID", "Year", "Education", "Age", "Income")]
  pdata$Year <- as.numeric(as.character(pdata$Year))
  
  
  # Perform the imputation using Amelia
  amelia_fit <- amelia(
    pdata,
    m = 3,
    ts = "Year",
    cs = "ID",
    noms = "Education"
  )
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


StartTime_amelia <- Sys.time()  # Starting time

# Function to generate coefficients and intercepts
Analyze_Amelia <- function(data) {
  
  # Step 1: Imputation and extraction of data
  amelia_fit <- Data_Imputation_Amelia(data)
  imputed_list <- amelia_fit$imputations
  
  # Step 2: Perform Breusch-Pagan test to check for a panel effect
  breusch_pagan_results <- lapply(imputed_list, function(x) {
    pdata <- pdata.frame(x, index = c("ID", "Year"))
    bp_test <- plmtest(plm(Income ~ Year + Education + Age, data = pdata, model = "pooling"), type = "bp")
    return(bp_test$p.value)
  })
  
  # Step 3: Based on Breusch-Pagan test, perform the appropriate regression
  model_list <- lapply(1:length(imputed_list), function(i) {
    pdata <- pdata.frame(imputed_list[[i]], index = c("ID", "Year"))
    if (breusch_pagan_results[[i]] > 0.05) {
      # Pooled OLS for no panel effect
      return(plm(Income ~ Year + Education + Age, data = pdata, model = "pooling"))
    } else {
      # Hausman test if panel effect exists
      random_model <- plm(Income ~ Year + Education + Age, data = pdata, model = "random")
      fixed_model <- plm(Income ~ Year + Education + Age, data = pdata, model = "within")
      
      hausman_test <- phtest(fixed_model, random_model) # Hausman test
      
      if (hausman_test$p.value <= 0.05) {
        print("Fixed Effects")
        return(fixed_model) # Fixed Effect model if correlation exists
      } else {
        print("Random Effects")
        return(random_model) # Random Effect model if correlation does not exists
      }
    }
  })
  
  # Step 4: Pool and return the coefficients
  pooled_results <- testEstimates(model_list)
  return(pooled_results)
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

EndTime_amelia <- Sys.time()  # Ending time

ExecutionTime_amelia <- EndTime_amelia - StartTime_amelia
print(ExecutionTime_amelia) # Time difference of 1.444227 mins

############################
## LSTM Network
############################

library(keras)
library(dplyr)
library(plm)
packageVersion("keras")


# Function to impute the datasets
Data_Imputation_LSTM <- function(data) {
  
  # Temporarily replace the missing values with mean
  DataTemp <- data %>%
    mutate(Income = ifelse(is.na(Income), mean(Income, na.rm = TRUE), Income))
  
  # Convert 'Education' to numeric to make it more compatible with the model and including other numeric columns
  DataEncoded <- DataTemp %>%
    mutate(Education = as.factor(Education)) %>%
    select(Age, Income, Education)
  
  DataEncoded <- as.data.frame(model.matrix(~ Education - 1, data = DataEncoded))
  
  InputData <- cbind(Age = DataTemp$Age, Income = DataTemp$Income, DataEncoded)
  
  CompleteCase <- InputData[!is.na(InputData$Income), ]
  
  TrainArray <- array(as.matrix(CompleteCase), dim = c(nrow(CompleteCase), 1, ncol(CompleteCase)))   # Because 3D array is more suitable for LSTM
  
  # define and compile the LSTM model
  model <- keras_model_sequential() %>%
    layer_lstm(units = 50, input_shape = c(1, ncol(CompleteCase)), return_sequences = FALSE) %>%
    layer_dense(units = 1)
  
  model %>% compile(
    loss = "mean_squared_error",
    optimizer = optimizer_adam()
  )
  
  # Train the model
  model %>% fit(
    x = TrainArray,
    y = CompleteCase$Income,  # Target is the Income column
    epochs = 50,
    batch_size = 32
  )
  
  # Predict and replacing the missing values
  MissingCase <- InputData[is.na(data$Income), ]
  if (nrow(MissingCase) > 0) {
    MissingArray <- array(as.matrix(MissingCase), dim = c(nrow(MissingCase), 1, ncol(MissingCase)))
    PredictedValue <- model %>% predict(MissingArray)
    data$Income[is.na(data$Income)] <- PredictedValue
  }
  
  return(data)
}

# Apply the function to the dataset
lstm_bal_mcar_50 <- Data_Imputation_LSTM(balanced_panel_data_mcar_50)
lstm_bal_mcar_30 <- Data_Imputation_LSTM(balanced_panel_data_mcar_30)
lstm_bal_mcar_10 <- Data_Imputation_LSTM(balanced_panel_data_mcar_10)

lstm_bal_mar_50 <- Data_Imputation_LSTM(balanced_panel_data_mar_50)
lstm_bal_mar_30 <- Data_Imputation_LSTM(balanced_panel_data_mar_30)
lstm_bal_mar_10 <- Data_Imputation_LSTM(balanced_panel_data_mar_10)

lstm_bal_mnar_50 <- Data_Imputation_LSTM(balanced_panel_data_mnar_50)
lstm_bal_mnar_30 <- Data_Imputation_LSTM(balanced_panel_data_mnar_30)
lstm_bal_mnar_10 <- Data_Imputation_LSTM(balanced_panel_data_mnar_10)

lstm_unbal_mcar_50 <- Data_Imputation_LSTM(unbalanced_panel_data_mcar_50)
lstm_unbal_mcar_30 <- Data_Imputation_LSTM(unbalanced_panel_data_mcar_30)
lstm_unbal_mcar_10 <- Data_Imputation_LSTM(unbalanced_panel_data_mcar_10)

lstm_unbal_mar_50 <- Data_Imputation_LSTM(unbalanced_panel_data_mar_50)
lstm_unbal_mar_30 <- Data_Imputation_LSTM(unbalanced_panel_data_mar_30)
lstm_unbal_mar_10 <- Data_Imputation_LSTM(unbalanced_panel_data_mar_10)

lstm_unbal_mnar_50 <- Data_Imputation_LSTM(unbalanced_panel_data_mnar_50)
lstm_unbal_mnar_30 <- Data_Imputation_LSTM(unbalanced_panel_data_mnar_30)
lstm_unbal_mnar_10 <- Data_Imputation_LSTM(unbalanced_panel_data_mnar_10)


StartTime_LSTM <- Sys.time()  # Starting time

# Function to generate coefficients and intercepts
Analyze_LSTM <- function(data) {
  
  # Step 1: Imputa the missing values and convert the data into panel data
  imputed_data <- Data_Imputation_LSTM(data)
  pdata <- pdata.frame(imputed_data, index = c("ID", "Year"))
  
  # Step 2: Perform Breusch-Pagan test to check the panel effects
  bp_test <- plmtest(plm(Income ~ Year + Education + Age, data = pdata, model = "pooling"), type = "bp")
  
  # Step 3: Based on the Breusch-Pagan test use appropriate models
  if (bp_test$p.value > 0.05) {
    # Pooled OLS for no panel effect
    model <- plm(Income ~ Year + Education + Age, data = pdata, model = "pooling")
  } else {
    # Hausman test if panel effect exist
    random_model <- plm(Income ~ Year + Education + Age, data = pdata, model = "random")
    fixed_model <- plm(Income ~ Year + Education + Age, data = pdata, model = "within")
    
    hausman_test <- phtest(fixed_model, random_model)
    
    if (hausman_test$p.value <= 0.05) {
      model <- fixed_model # Fixed effect model if correlation exists
    } else {
      model <- random_model # Random Effect model if correlation does not exist
    }
  }
  
  # Step 4: Return the model summary
  return(summary(model))
}

# Apply the function to each dataset and store results
analyze_lstm_bal_mcar_50 <- Analyze_LSTM(balanced_panel_data_mcar_50)
analyze_lstm_bal_mcar_30 <- Analyze_LSTM(balanced_panel_data_mcar_30)
analyze_lstm_bal_mcar_10 <- Analyze_LSTM(balanced_panel_data_mcar_10)

analyze_lstm_bal_mar_50 <- Analyze_LSTM(balanced_panel_data_mar_50)
analyze_lstm_bal_mar_30 <- Analyze_LSTM(balanced_panel_data_mar_30)
analyze_lstm_bal_mar_10 <- Analyze_LSTM(balanced_panel_data_mar_10)

analyze_lstm_bal_mnar_50 <- Analyze_LSTM(balanced_panel_data_mnar_50)
analyze_lstm_bal_mnar_30 <- Analyze_LSTM(balanced_panel_data_mnar_30)
analyze_lstm_bal_mnar_10 <- Analyze_LSTM(balanced_panel_data_mnar_10)

analyze_lstm_unbal_mcar_50 <- Analyze_LSTM(unbalanced_panel_data_mcar_50)
analyze_lstm_unbal_mcar_30 <- Analyze_LSTM(unbalanced_panel_data_mcar_30)
analyze_lstm_unbal_mcar_10 <- Analyze_LSTM(unbalanced_panel_data_mcar_10)

analyze_lstm_unbal_mar_50 <- Analyze_LSTM(unbalanced_panel_data_mar_50)
analyze_lstm_unbal_mar_30 <- Analyze_LSTM(unbalanced_panel_data_mar_30)
analyze_lstm_unbal_mar_10 <- Analyze_LSTM(unbalanced_panel_data_mar_10)

analyze_lstm_unbal_mnar_50 <- Analyze_LSTM(unbalanced_panel_data_mnar_50)
analyze_lstm_unbal_mnar_30 <- Analyze_LSTM(unbalanced_panel_data_mnar_30)
analyze_lstm_unbal_mnar_10 <- Analyze_LSTM(unbalanced_panel_data_mnar_10)

EndTime_LSTM <- Sys.time()  # Ending time

ExecutionTime_LSTM <- EndTime_LSTM - StartTime_LSTM
print(ExecutionTime_LSTM) # Time difference of 20.0069 mins

#############################
# Distribution Comparasion ##
#############################

##### mice #####

miceData <- function(data){
  miceImp <- complete(data, 1)
  return(miceImp)
}

# Extract imputed dataset
mice_imp_bal_mcar_50 <- miceData(mice_bal_mcar_50)
mice_imp_bal_mcar_30 <- miceData(mice_bal_mcar_30)
mice_imp_bal_mcar_10 <- miceData(mice_bal_mcar_10)

mice_imp_bal_mar_50 <- miceData(mice_bal_mar_50)
mice_imp_bal_mar_30 <- miceData(mice_bal_mar_30)
mice_imp_bal_mar_10 <- miceData(mice_bal_mar_10)

mice_imp_bal_mnar_50 <- miceData(mice_bal_mnar_50)
mice_imp_bal_mnar_30 <- miceData(mice_bal_mnar_30)
mice_imp_bal_mnar_10 <- miceData(mice_bal_mnar_10)

mice_imp_unbal_mcar_50 <- miceData(mice_unbal_mcar_50)
mice_imp_unbal_mcar_30 <- miceData(mice_unbal_mcar_30)
mice_imp_unbal_mcar_10 <- miceData(mice_unbal_mcar_10)

mice_imp_unbal_mar_50 <- miceData(mice_unbal_mar_50)
mice_imp_unbal_mar_30 <- miceData(mice_unbal_mar_30)
mice_imp_unbal_mar_10 <- miceData(mice_unbal_mar_10)

mice_imp_unbal_mnar_50 <- miceData(mice_unbal_mnar_50)
mice_imp_unbal_mnar_30 <- miceData(mice_unbal_mnar_30)
mice_imp_unbal_mnar_10 <- miceData(mice_unbal_mnar_10)

# plot distribution
IncDist_mice <- function(data, col){
  DataTemp_mice <- density(data$Income, na.rm = TRUE)
  lines(DataTemp_mice, col = col, lwd = 2)
}

DataTemp_mice <- density(balanced_panel_data$Income, na.rm = TRUE)
plot(DataTemp_mice, 
     main = "Income Distributions from mice - Balanced panel", 
     xlab = "Income", 
     ylab = "Density",
     lwd = 2, col = "black")

###balanced data

IncDist_mice(mice_imp_bal_mcar_50, "blue")
IncDist_mice(mice_imp_bal_mcar_30, "red")
IncDist_mice(mice_imp_bal_mcar_10, "pink")

IncDist_mice(mice_imp_bal_mar_50, "skyblue")
IncDist_mice(mice_imp_bal_mar_30, "violet")
IncDist_mice(mice_imp_bal_mar_10, "yellow")

IncDist_mice(mice_imp_bal_mnar_50, "orange")
IncDist_mice(mice_imp_bal_mnar_30, "green")
IncDist_mice(mice_imp_bal_mnar_10, "brown")

legend("topright", 
       legend = c("Initial Data", 
                  "bal_mcar_50", "bal_mcar_30", "bal_mcar_10", 
                  "bal_mar_50", "bal_mar_30", "bal_mar_10", 
                  "bal_mnar_50", "bal_mnar_30", "bal_mnar_10"),
       col = c("black", "blue", "red", "pink", "skyblue", "violet", "yellow", 
               "orange", "green", "brown"),
       lwd = 2)

###Unbalanced data

DataTemp_mice <- density(unbalanced_panel_data$Income, na.rm = TRUE)
plot(DataTemp_mice, 
     main = "Income Distributions from mice - Unbalanced panel", 
     xlab = "Income", 
     ylab = "Density",
     lwd = 2, col = "black")

IncDist_mice(mice_imp_unbal_mcar_50, "coral")
IncDist_mice(mice_imp_unbal_mcar_30, "salmon")
IncDist_mice(mice_imp_unbal_mcar_10, "lavender")

IncDist_mice(mice_imp_unbal_mar_50, "gray")
IncDist_mice(mice_imp_unbal_mar_30, "gold")
IncDist_mice(mice_imp_unbal_mar_10, "orchid")

IncDist_mice(mice_imp_unbal_mnar_50, "navy")
IncDist_mice(mice_imp_unbal_mnar_30, "darkgreen")
IncDist_mice(mice_imp_unbal_mnar_10, "steelblue")

legend("topright", 
       legend = c("Initial Data", 
                  "unbal_mcar_50", "unbal_mcar_30", "unbal_mcar_10", 
                  "unbal_mar_50", "unbal_mar_30", "unbal_mar_10", 
                  "unbal_mnar_50", "unbal_mnar_30", "unbal_mnar_10"),
       col = c("black", "coral", "salmon", "lavender", 
               "gray", "gold", "orchid", "navy", "darkgreen", "steelblue"),
       lwd = 2)

##### mitml #####

# Data Extraction
mitmlData <- function(data){
  mitmlImp <- data[[1]]
  return(mitmlImp)
}

mitml_imp_bal_mcar_50 <- mitmlData(mitml_bal_mcar_50)
mitml_imp_bal_mcar_30 <- mitmlData(mitml_bal_mcar_30)
mitml_imp_bal_mcar_10 <- mitmlData(mitml_bal_mcar_10)


mitml_imp_bal_mar_50 <- mitmlData(mitml_bal_mar_50)
mitml_imp_bal_mar_30 <- mitmlData(mitml_bal_mar_30)
mitml_imp_bal_mar_10 <- mitmlData(mitml_bal_mar_10)


mitml_imp_bal_mnar_50 <- mitmlData(mitml_bal_mnar_50)
mitml_imp_bal_mnar_30 <- mitmlData(mitml_bal_mnar_30)
mitml_imp_bal_mnar_10 <- mitmlData(mitml_bal_mnar_10)

mitml_imp_unbal_mcar_50 <- mitmlData(mitml_unbal_mcar_50)
mitml_imp_unbal_mcar_30 <- mitmlData(mitml_unbal_mcar_30)
mitml_imp_unbal_mcar_10 <- mitmlData(mitml_unbal_mcar_10)

mitml_imp_unbal_mar_50 <- mitmlData(mitml_unbal_mar_50)
mitml_imp_unbal_mar_30 <- mitmlData(mitml_unbal_mar_30)
mitml_imp_unbal_mar_10 <- mitmlData(mitml_unbal_mar_10)

mitml_imp_unbal_mnar_50 <- mitmlData(mitml_unbal_mnar_50)
mitml_imp_unbal_mnar_30 <- mitmlData(mitml_unbal_mnar_30)
mitml_imp_unbal_mnar_10 <- mitmlData(mitml_unbal_mnar_10)

### Plot Distribution
IncDist_mitml <- function(data, col) {
  DataTemp_mitml <- density(data$Income, na.rm = TRUE)
  lines(DataTemp_mitml, col = col, lwd = 2)
}

DataTemp_mitml <- density(balanced_panel_data$Income, na.rm = TRUE)
plot(DataTemp_mitml, 
     main = "Income Distributions from mitml - Balanced panel", 
     xlab = "Income", 
     ylab = "Density",
     lwd = 2, col = "black")

# Balanced Data
IncDist_mitml(mitml_imp_bal_mcar_50, "blue")
IncDist_mitml(mitml_imp_bal_mcar_30, "red")
IncDist_mitml(mitml_imp_bal_mcar_10, "pink")

IncDist_mitml(mitml_imp_bal_mar_50, "skyblue")
IncDist_mitml(mitml_imp_bal_mar_30, "violet")
IncDist_mitml(mitml_imp_bal_mar_10, "yellow")

IncDist_mitml(mitml_imp_bal_mnar_50, "orange")
IncDist_mitml(mitml_imp_bal_mnar_30, "green")
IncDist_mitml(mitml_imp_bal_mnar_10, "brown")

legend("topright", 
       legend = c("Initial Data", 
                  "bal_mcar_50", "bal_mcar_30", "bal_mcar_10", 
                  "bal_mar_50", "bal_mar_30", "bal_mar_10", 
                  "bal_mnar_50", "bal_mnar_30", "bal_mnar_10"),
       col = c("black", "blue", "red", "pink", "skyblue", "violet", "yellow", 
               "orange", "green", "brown"),
       lwd = 2)

### Unbalanced data

DataTemp_mitml <- density(unbalanced_panel_data$Income, na.rm = TRUE)
plot(DataTemp_mitml, 
     main = "Income Distributions from mitml - Unbalanced Panel", 
     xlab = "Income", 
     ylab = "Density",
     lwd = 2, col = "black")

IncDist_mitml(mitml_imp_unbal_mcar_50, "coral")
IncDist_mitml(mitml_imp_unbal_mcar_30, "salmon")
IncDist_mitml(mitml_imp_unbal_mcar_10, "lavender")

IncDist_mitml(mitml_imp_unbal_mar_50, "gray")
IncDist_mitml(mitml_imp_unbal_mar_30, "gold")
IncDist_mitml(mitml_imp_unbal_mar_10, "orchid")

IncDist_mitml(mitml_imp_unbal_mnar_50, "navy")
IncDist_mitml(mitml_imp_unbal_mnar_30, "darkgreen")
IncDist_mitml(mitml_imp_unbal_mnar_10, "steelblue")

legend("topright", 
       legend = c("Initial Data", 
                  "unbal_mcar_50", "unbal_mcar_30", "unbal_mcar_10", 
                  "unbal_mar_50", "unbal_mar_30", "unbal_mar_10", 
                  "unbal_mnar_50", "unbal_mnar_30", "unbal_mnar_10"),
       col = c("black", "coral", "salmon", "lavender", 
               "gray", "gold", "orchid", "navy", "darkgreen", "steelblue"),
       lwd = 2)

### amelia

# Data Extraction
ameliaData <- function(data){
  ameliaImp <- data$imputations[[1]]
  return(ameliaImp)
}

amelia_imp_bal_mcar_50 <- ameliaData(amelia_bal_mcar_50)
amelia_imp_bal_mcar_30 <- ameliaData(amelia_bal_mcar_30)
amelia_imp_bal_mcar_10 <- ameliaData(amelia_bal_mcar_10)

amelia_imp_bal_mar_50 <- ameliaData(amelia_bal_mar_50)
amelia_imp_bal_mar_30 <- ameliaData(amelia_bal_mar_30)
amelia_imp_bal_mar_10 <- ameliaData(amelia_bal_mar_10)

amelia_imp_bal_mnar_50 <- ameliaData(amelia_bal_mnar_50)
amelia_imp_bal_mnar_30 <- ameliaData(amelia_bal_mnar_30)
amelia_imp_bal_mnar_10 <- ameliaData(amelia_bal_mnar_10)

amelia_imp_unbal_mcar_50 <- ameliaData(amelia_unbal_mcar_50)
amelia_imp_unbal_mcar_30 <- ameliaData(amelia_unbal_mcar_30)
amelia_imp_unbal_mcar_10 <- ameliaData(amelia_unbal_mcar_10)

amelia_imp_unbal_mar_50 <- ameliaData(amelia_unbal_mar_50)
amelia_imp_unbal_mar_30 <- ameliaData(amelia_unbal_mar_30)
amelia_imp_unbal_mar_10 <- ameliaData(amelia_unbal_mar_10)

amelia_imp_unbal_mnar_50 <- ameliaData(amelia_unbal_mnar_50)
amelia_imp_unbal_mnar_30 <- ameliaData(amelia_unbal_mnar_30)
amelia_imp_unbal_mnar_10 <- ameliaData(amelia_unbal_mnar_10)

# Plot Distribution
IncDist_amelia <- function(data, col) {
  DataTemp_amelia <- density(data$Income, na.rm = TRUE)
  lines(DataTemp_amelia, col = col, lwd = 2)
}

# Balanced Panel
DataTemp_amelia <- density(balanced_panel_data$Income, na.rm = TRUE)
plot(DataTemp_amelia, 
     main = "Income Distributions from Amelia - Balanced Panel", 
     xlab = "Income", 
     ylab = "Density",
     lwd = 2, col = "black")

IncDist_amelia(amelia_imp_bal_mcar_50, "blue")
IncDist_amelia(amelia_imp_bal_mcar_30, "red")
IncDist_amelia(amelia_imp_bal_mcar_10, "pink")

IncDist_amelia(amelia_imp_bal_mar_50, "skyblue")
IncDist_amelia(amelia_imp_bal_mar_30, "violet")
IncDist_amelia(amelia_imp_bal_mar_10, "yellow")

IncDist_amelia(amelia_imp_bal_mnar_50, "orange")
IncDist_amelia(amelia_imp_bal_mnar_30, "green")
IncDist_amelia(amelia_imp_bal_mnar_10, "brown")

legend("topright", 
       legend = c("Initial Data", 
                  "bal_mcar_50", "bal_mcar_30", "bal_mcar_10", 
                  "bal_mar_50", "bal_mar_30", "bal_mar_10", 
                  "bal_mnar_50", "bal_mnar_30", "bal_mnar_10"),
       col = c("black", "blue", "red", "pink", "skyblue", "violet", "yellow", 
               "orange", "green", "brown"),
       lwd = 2)

# Unbalanced Panel
DataTemp_amelia <- density(unbalanced_panel_data$Income, na.rm = TRUE)
plot(DataTemp_amelia, 
     main = "Income Distributions from Amelia - Unbalanced Panel", 
     xlab = "Income", 
     ylab = "Density",
     lwd = 2, col = "black")

IncDist_amelia(amelia_imp_unbal_mcar_50, "coral")
IncDist_amelia(amelia_imp_unbal_mcar_30, "salmon")
IncDist_amelia(amelia_imp_unbal_mcar_10, "lavender")

IncDist_amelia(amelia_imp_unbal_mar_50, "gray")
IncDist_amelia(amelia_imp_unbal_mar_30, "gold")
IncDist_amelia(amelia_imp_unbal_mar_10, "orchid")

IncDist_amelia(amelia_imp_unbal_mnar_50, "navy")
IncDist_amelia(amelia_imp_unbal_mnar_30, "darkgreen")
IncDist_amelia(amelia_imp_unbal_mnar_10, "steelblue")


legend("topright", 
       legend = c("Initial Data", 
                  "unbal_mcar_50", "unbal_mcar_30", "unbal_mcar_10", 
                  "unbal_mar_50", "unbal_mar_30", "unbal_mar_10", 
                  "unbal_mnar_50", "unbal_mnar_30", "unbal_mnar_10"),
       col = c("black", "coral", "salmon", "lavender", 
               "gray", "gold", "orchid", "navy", "darkgreen", "steelblue"),
       lwd = 2)

### LSTM-Network

# Plot Distribution
IncDist_LSTM <- function(data, col) {
  DataTemp_LSTM <- density(data$Income, na.rm = TRUE)
  lines(DataTemp_LSTM, col = col, lwd = 2)
}

# Balanced Panel
DataTemp_LSTM <- density(balanced_panel_data$Income, na.rm = TRUE)
plot(DataTemp_LSTM, 
     main = "Income Distributions from LSTM - Balanced Panel", 
     xlab = "Income", 
     ylab = "Density",
     lwd = 2, col = "black")

IncDist_LSTM(lstm_bal_mcar_50, "blue")
IncDist_LSTM(lstm_bal_mcar_30, "red")
IncDist_LSTM(lstm_bal_mcar_10, "pink")

IncDist_LSTM(lstm_bal_mar_50, "skyblue")
IncDist_LSTM(lstm_bal_mar_30, "violet")
IncDist_LSTM(lstm_bal_mar_10, "yellow")

IncDist_LSTM(lstm_bal_mnar_50, "orange")
IncDist_LSTM(lstm_bal_mnar_30, "green")
IncDist_LSTM(lstm_bal_mnar_10, "brown")

legend("topright", 
       legend = c("Initial Data", 
                  "bal_mcar_50", "bal_mcar_30", "bal_mcar_10", 
                  "bal_mar_50", "bal_mar_30", "bal_mar_10", 
                  "bal_mnar_50", "bal_mnar_30", "bal_mnar_10"),
       col = c("black", "blue", "red", "pink", "skyblue", "violet", "yellow", 
               "orange", "green", "brown"),
       lwd = 2)

# Unbalanced Panel
DataTemp_LSTM <- density(unbalanced_panel_data$Income, na.rm = TRUE)
plot(DataTemp_LSTM, 
     main = "Income Distributions from LSTM - Unbalanced Panel", 
     xlab = "Income", 
     ylab = "Density",
     lwd = 2, col = "black")

IncDist_LSTM(lstm_unbal_mcar_50, "coral")
IncDist_LSTM(lstm_unbal_mcar_30, "salmon")
IncDist_LSTM(lstm_unbal_mcar_10, "lavender")

IncDist_LSTM(lstm_unbal_mar_50, "gray")
IncDist_LSTM(lstm_unbal_mar_30, "gold")
IncDist_LSTM(lstm_unbal_mar_10, "orchid")

IncDist_LSTM(lstm_unbal_mnar_50, "navy")
IncDist_LSTM(lstm_unbal_mnar_30, "darkgreen")
IncDist_LSTM(lstm_unbal_mnar_10, "steelblue")


legend("topright", 
       legend = c("Initial Data", 
                  "unbal_mcar_50", "unbal_mcar_30", "unbal_mcar_10", 
                  "unbal_mar_50", "unbal_mar_30", "unbal_mar_10", 
                  "unbal_mnar_50", "unbal_mnar_30", "unbal_mnar_10"),
       col = c("black", "coral", "salmon", "lavender", 
               "gray", "gold", "orchid", "navy", "darkgreen", "steelblue"),
       lwd = 2)
