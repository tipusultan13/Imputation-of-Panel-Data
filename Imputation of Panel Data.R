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
library(gridExtra)
library(VIM)
library(mice)
library(broom)
library(mitml)
library(plm)
library(lmtest)
library(Amelia)
library(keras)
library(plm)

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

#########################
## Balanced Panel
########################

data$Year <- as.numeric(data$Year) # Convert Year column to make the calculation easy

# Find IDs that are present in all years between 2013 and 2023
years <- 2013:2023
CommonIDS <- Reduce(intersect, lapply(years, function(year) {
  unique(data$ID[data$Year == year])
}))

PanelData <- data[data$ID %in% CommonIDS, ] # Filter the data to keep only rows with common IDs

# Count the occurrence of each ID
IDCounts <- PanelData %>%
  count(ID)

# Extract IDs with exactly 11 occurrences
ids_with_11_occurrences <- IDCounts %>%
  filter(n == 11) %>%
  pull(ID)

# Filter the original dataset to include only rows with these IDs
balanced_panel_data <- PanelData %>%
  filter(ID %in% ids_with_11_occurrences)

# Count the number of unique years each ID appears in
IDYearCounts <- aggregate(Year ~ ID, data = balanced_panel_data, 
                          FUN = function(x) length(unique(x)))

is_balanced_panel <- all(IDYearCounts$Year == length(2013:2023)) # Check if every ID appears in all the years (2013 to 2023)

if (is_balanced_panel) {
  print("Balanced Panel Data")
} else {
  print("Unbalanced Panel Data")
}

# Print the IDs that do not appear in all years (optional task)
UnbalancedIDS <- IDYearCounts$ID[IDYearCounts$Year != length(2013:2023)]
if (length(UnbalancedIDS) > 0) {
  print("IDs that are not present in all years:")
  print(UnbalancedIDS)
}

# Count the occurrence of each ID
IDCounts <- balanced_panel_data %>%
  count(ID)

# Filter IDs that appear more than 11 times
more_than_11 <- IDCounts %>%
  filter(n > 11)
num_more_than_11 <- nrow(more_than_11)

# Filter IDs that appear less than 11 times
less_than_11 <- IDCounts %>%
  filter(n < 11)
num_less_than_11 <- nrow(less_than_11)

print(paste("Number of IDs appearing more than 11 times:", num_more_than_11))
print(paste("Number of IDs appearing less than 11 times:", num_less_than_11))

count(balanced_panel_data, Year)
summary(balanced_panel_data)

########################
## Unalanced Panel
########################

SampleData <- data.frame()

# Loop through each year to ensure 3455 unique ID-Year combinations
for (yr in unique(data$Year)) {
  # Filter the data for the current year
  YearData <- data %>% filter(Year == yr)
  
  # Sample 3455 unique ID-Year combinations for this year
  SampleTemp <- YearData %>%
    distinct(ID, Year, .keep_all = TRUE) %>%
    slice_sample(n = 3455)
  
  # Append the sampled data to the main data frame
  SampleData <- bind_rows(SampleData, SampleTemp)
}

unbalanced_panel_data <- SampleData

# Count the number of unique years each ID appears in
IDYearCounts <- aggregate(Year ~ ID, data = unbalanced_panel_data, 
                          FUN = function(x) length(unique(x)))

is_balanced_panel <- all(IDYearCounts$Year == length(2013:2023)) # Check if every ID appears in all the years (2013 to 2023)


if (is_balanced_panel) {
  print("The data is a balanced panel.")
} else {
  print("The data is not a balanced panel.")
}

# Print the IDs that do not appear in all years
UnbalancedIDS <- IDYearCounts$ID[IDYearCounts$Year != length(2013:2023)]
if (length(UnbalancedIDS) > 0) {
  print("IDs that do not appear in all years:")
  print(UnbalancedIDS)
}

print(unbalanced_panel_data[duplicated(unbalanced_panel_data), ]) # Check for duplicate rows in the data

count(unbalanced_panel_data, Year)
summary(unbalanced_panel_data)

########################
## Simulate Missingness in Balanced Panel
########################

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

#################################
## Missingness in Unalanced Panel
#################################

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
balanced_panel_data <- convert_to_factors(balanced_panel_data)
unbalanced_panel_data <- convert_to_factors(unbalanced_panel_data)

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

packageVersion("mice")

StartTime_mice <- Sys.time()  # Starting time

Data_Imputation_mice <- function(data, m = 3, maxit = 20, method = 'pmm') {

  ID <- data$ID
  DataTemp <- data[c("Year", "Education", "Age", "Income")] # Dataset without ID column.
  miceImp <- mice(DataTemp, method = method, m = m, maxit = maxit) # Perform MICE imputation
  
  # Readd the ID column
  CompleteDataset <- lapply(1:m, function(i) {
    CompleteData <- complete(miceImp, action = i)
    CompleteData <- cbind(ID = ID, CompleteData)
    return(CompleteData)
  })
  
  return(CompleteDataset)  # Return a list of completed datasets
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

EndTime_mice <- Sys.time()  # Ending time
ExecutionTime_mice <- EndTime_mice - StartTime_mice
print(ExecutionTime_mice) # Time difference of 1.326622 mins

######################
## mitml package
######################

packageVersion("mitml")

## Imputation ##

## Balanced Panel##

StartTime_mitml <- Sys.time()  # Starting time

pdata_bal <- pdata.frame(balanced_panel_data, index = c("ID", "Year")) # Convert the data frame to a panel data frame

# Estimate the models
FE_Model <- plm(Income ~ Year + Education + Age, data = pdata_bal, model = "within")
RE_Model <- plm(Income ~ Year + Education + Age, data = pdata_bal, model = "random")

HausmanTest <- phtest(FE_Model, RE_Model) # Perform the Hausman test to compare the fixed and random effects models
print(HausmanTest)
# p-value is 0.2185, which is > 0.05. S0 the null hypothesis cannot be rejected.
# This implies that the random effects in Education and Age is more appropriate.

# Function to impute data
Data_Imputation_mitml_Bal <- function(panel_data) {
  
  SelectedData <- panel_data[c("ID", "Year", "Education", "Age", "Income")]
  
  # Define the type vector and assign column names
  type <- c(0, -2, 3, 3, 1)
  names(type) <- colnames(SelectedData)
  
  ImputedData <- panImpute(SelectedData, type = type, n.burn = 1000, n.iter = 100, m = 3)   # Impute missing data
  ImputedList <- mitmlComplete(ImputedData, print = "all")
  return(ImputedList)
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

## Unbalanced ##

pdata_unbal <- pdata.frame(unbalanced_panel_data, index = c("ID", "Year")) # Convert the data frame to a panel data frame

# Estimate the models
FE_Model <- plm(Income ~ Year + Education + Age, data = pdata_unbal, model = "within")
RE_Model <- plm(Income ~ Year + Education + Age, data = pdata_unbal, model = "random")

HausmanTest <- phtest(FE_Model, RE_Model) # Perform the Hausman test to compare the fixed and random effects models
print(HausmanTest)
# p-value is 2.2e-16, which is < 0.05. So null hypothesis can be rejected.
# implying that the fixed effects in Education and Age is more appropriate.

# Function to impute data for unbalanced panels
Data_Imputation_mitml_Unbal <- function(panel_data) {
  # Prepare the data by ungrouping and selecting relevant columns
  panel_data <- panel_data %>% 
    ungroup()
  
  SelectedData <- as.data.frame(panel_data[c("ID", "Year", "Education", "Age", "Income")])
  
  # Define the type vector and assign column names
  type <- c(0, -2, 2, 2, 1) 
  names(type) <- colnames(SelectedData)
  
  ImputedData <- panImpute(SelectedData, type = type, n.burn = 1000, n.iter = 100, m = 3)   # Impute missing data
  ImputedList <- mitmlComplete(ImputedData, print = "all")   # Extract imputed datasets
  return(ImputedList)
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

EndTime_mitml <- Sys.time()  # Ending time
ExecutionTime_mitml <- EndTime_mitml - StartTime_mitml
print(ExecutionTime_mitml) # Time difference of 52.44876 secs

############################
## Amelia package
############################

packageVersion("Amelia")

StartTime_amelia <- Sys.time()  # Starting time

# Function to impute data
Data_Imputation_Amelia <- function(data) {
  
  # Convert the data to panel data using plm package
  pdata <- pdata.frame(data, index = c("ID", "Year"))
  pdata = pdata[c("ID", "Year", "Education", "Age", "Income")]
  pdata$Year <- as.numeric(as.character(pdata$Year))
  
  
  # Perform the imputation using Amelia
  ImputedData <- amelia(
    pdata,
    m = 3,
    ts = "Year",
    cs = "ID",
    noms = "Education"
  )
  return(ImputedData)
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

EndTime_amelia <- Sys.time()  # Ending time
ExecutionTime_amelia <- EndTime_amelia - StartTime_amelia
print(ExecutionTime_amelia) # Time difference of 51.04909 secs

############################
## LSTM Network
############################

packageVersion("keras")

StartTime_LSTM <- Sys.time()  # Starting time

Data_Imputation_LSTM <- function(data) {
  
  # Remove the rows of the cells that include missing values
  MissingRows <- which(is.na(data$Income))
  CompleteData <- data[!is.na(data$Income), ]
  
  # Normalize Age and Income columns to train the LSTM model
  AgeM <- mean(CompleteData$Age, na.rm = TRUE)
  AgeSD <- sd(CompleteData$Age, na.rm = TRUE)
  
  IncomeM <- mean(CompleteData$Income, na.rm = TRUE)
  IncomeSD <- sd(CompleteData$Income, na.rm = TRUE)
  
  CompleteData$Age <- (CompleteData$Age - AgeM) / AgeSD
  CompleteData$Income <- (CompleteData$Income - IncomeM) / IncomeSD
  
  # Convert categorical columns to numeric encoding
  CompleteData <- CompleteData %>%
    mutate(Year = as.numeric(factor(Year)),
           Education = as.numeric(factor(Education)))
  
  # Prepare data to train the model, excluding ID
  TrainX <- as.matrix(CompleteData %>% select(Year, Education, Age))
  TrainY <- CompleteData$Income
  
  # Reshape the data into 3D to make LSTM calculation easier (samples, timesteps=1, features=3)
  TrainX <- array(TrainX, dim = c(nrow(TrainX), 1, ncol(TrainX)))
  
  # Build the model
  model <- keras_model_sequential() %>%
    layer_lstm(units = 50, input_shape = c(1, 3), return_sequences = FALSE) %>%
    layer_dense(units = 1)
  
  model %>% compile(
    loss = 'mean_squared_error',
    optimizer = optimizer_adam()
  )
  
  # Train the model
  model %>% fit(TrainX, TrainY, epochs = 50, batch_size = 32, verbose = 1)
  
  # Refill the missing values
  MissingValues <- data[MissingRows, ]
  
  MissingValues <- MissingValues %>%
    mutate(Year = as.numeric(factor(Year)),
           Education = as.numeric(factor(Education)),
           Age = (Age - AgeM) / AgeSD)  # Normalize Age for missing data
  
  XMissing <- as.matrix(MissingValues %>% select(Year, Education, Age))
  XMissing <- array(XMissing, dim = c(nrow(XMissing), 1, ncol(XMissing)))
  
  PredictedIncome <- model %>% predict(XMissing) # Predict the missing values
  PredictedIncome <- PredictedIncome * IncomeSD + IncomeM # De-normalize the predicted income
  data$Income[MissingRows] <- PredictedIncome # Fill the missing values
  
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

EndTime_LSTM <- Sys.time()  # Ending time
ExecutionTime_LSTM <- EndTime_LSTM - StartTime_LSTM
print(ExecutionTime_LSTM) # Time difference of 11.66831 mins


#############################
# Distribution Comparason #
#############################

##### mice #####

miceData <- function(data){
  miceImp <- data[[1]]
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
par(mfrow = c(2, 3))
plot(DataTemp_mice, 
     main = "Income Distributions from mice - Balanced panel (MNAR)", 
     xlab = "Income", 
     ylab = "Density",
     lwd = 2, col = "black")

### Balanced data

IncDist_mice(mice_imp_bal_mcar_50, "blue")
IncDist_mice(mice_imp_bal_mcar_30, "red")
IncDist_mice(mice_imp_bal_mcar_10, "pink")
legend("topright", 
       legend = c("Initial Data", "bal_mcar_50", "bal_mcar_30", "bal_mcar_10"),
       col = c("black", "blue", "red", "pink"),
       lwd = 2,
       cex = 0.4)

IncDist_mice(mice_imp_bal_mar_50, "skyblue")
IncDist_mice(mice_imp_bal_mar_30, "violet")
IncDist_mice(mice_imp_bal_mar_10, "yellow")
legend("topright", 
       legend = c("Initial Data", "bal_mar_50", "bal_mar_30", "bal_mar_10"),
       col = c("black", "skyblue", "violet", "yellow"),
       lwd = 2,
       cex = 0.4)

IncDist_mice(mice_imp_bal_mnar_50, "orange")
IncDist_mice(mice_imp_bal_mnar_30, "green")
IncDist_mice(mice_imp_bal_mnar_10, "brown")
legend("topright", 
       legend = c("Initial Data", "bal_mnar_50", "bal_mnar_30", "bal_mnar_10"),
       col = c("black", "orange", "green", "brown"),
       lwd = 2,
       cex = 0.4)

### Unbalanced data

DataTemp_mice <- density(unbalanced_panel_data$Income, na.rm = TRUE)
plot(DataTemp_mice, 
     main = "Income Distributions from mice - Unbalanced panel (MNAR)", 
     xlab = "Income", 
     ylab = "Density",
     lwd = 2, col = "black")

IncDist_mice(mice_imp_unbal_mcar_50, "coral")
IncDist_mice(mice_imp_unbal_mcar_30, "salmon")
IncDist_mice(mice_imp_unbal_mcar_10, "lavender")
legend("topright", 
       legend = c("Initial Data", "unbal_mcar_50", "unbal_mcar_30", "unbal_mcar_10"),
       col = c("black", "coral", "salmon", "lavender"),
       lwd = 2,
       cex = 0.4)

IncDist_mice(mice_imp_unbal_mar_50, "gray")
IncDist_mice(mice_imp_unbal_mar_30, "gold")
IncDist_mice(mice_imp_unbal_mar_10, "orchid")
legend("topright", 
       legend = c("Initial Data","unbal_mar_50", "unbal_mar_30", "unbal_mar_10"),
       col = c("black", "gray", "gold", "orchid"),
       lwd = 2,
       cex = 0.4)

IncDist_mice(mice_imp_unbal_mnar_50, "navy")
IncDist_mice(mice_imp_unbal_mnar_30, "darkgreen")
IncDist_mice(mice_imp_unbal_mnar_10, "steelblue")
legend("topright", 
       legend = c("Initial Data", "unbal_mnar_50", "unbal_mnar_30", "unbal_mnar_10"),
       col = c("black", "navy", "darkgreen", "steelblue"),
       lwd = 2,
       cex = 0.4)

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
     main = "Income Distributions from mitml - Balanced panel (MNAR)", 
     xlab = "Income", 
     ylab = "Density",
     lwd = 2, col = "black")

# Balanced Data
IncDist_mitml(mitml_imp_bal_mcar_50, "blue")
IncDist_mitml(mitml_imp_bal_mcar_30, "red")
IncDist_mitml(mitml_imp_bal_mcar_10, "pink")
legend("topright", 
       legend = c("Initial Data", "bal_mcar_50", "bal_mcar_30", "bal_mcar_10"),
       col = c("black", "blue", "red", "pink"),
       lwd = 2,
       cex = 0.8)

IncDist_mitml(mitml_imp_bal_mar_50, "skyblue")
IncDist_mitml(mitml_imp_bal_mar_30, "violet")
IncDist_mitml(mitml_imp_bal_mar_10, "yellow")
legend("topright", 
       legend = c("Initial Data", "bal_mar_50", "bal_mar_30", "bal_mar_10"),
       col = c("black","skyblue", "violet", "yellow"),
       lwd = 2,
       cex = 0.8)

IncDist_mitml(mitml_imp_bal_mnar_50, "orange")
IncDist_mitml(mitml_imp_bal_mnar_30, "green")
IncDist_mitml(mitml_imp_bal_mnar_10, "brown")
legend("topright", 
       legend = c("Initial Data", "bal_mnar_50", "bal_mnar_30", "bal_mnar_10"),
       col = c("black", "orange", "green", "brown"),
       lwd = 2,
       cex = 0.8)

### Unbalanced data

DataTemp_mitml <- density(unbalanced_panel_data$Income, na.rm = TRUE)
plot(DataTemp_mitml, 
     main = "Income Distributions from mitml - Unbalanced Panel (MNAR)", 
     xlab = "Income", 
     ylab = "Density",
     lwd = 2, col = "black")

IncDist_mitml(mitml_imp_unbal_mcar_50, "coral")
IncDist_mitml(mitml_imp_unbal_mcar_30, "salmon")
IncDist_mitml(mitml_imp_unbal_mcar_10, "lavender")
legend("topright", 
       legend = c("Initial Data", "unbal_mcar_50", "unbal_mcar_30", "unbal_mcar_10"),
       col = c("black", "coral", "salmon", "lavender"),
       lwd = 2,
       cex = 0.8)

IncDist_mitml(mitml_imp_unbal_mar_50, "gray")
IncDist_mitml(mitml_imp_unbal_mar_30, "gold")
IncDist_mitml(mitml_imp_unbal_mar_10, "orchid")
legend("topright", 
       legend = c("Initial Data", "unbal_mar_50", "unbal_mar_30", "unbal_mar_10"),
       col = c("black", "gray", "gold", "orchid"),
       lwd = 2,
       cex = 0.8)

IncDist_mitml(mitml_imp_unbal_mnar_50, "navy")
IncDist_mitml(mitml_imp_unbal_mnar_30, "darkgreen")
IncDist_mitml(mitml_imp_unbal_mnar_10, "steelblue")
legend("topright", 
       legend = c("Initial Data", "unbal_mnar_50", "unbal_mnar_30", "unbal_mnar_10"),
       col = c("black", "navy", "darkgreen", "steelblue"),
       lwd = 2,
       cex = 0.8)

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
     main = "Income Distributions from Amelia - Balanced Panel (MNAR)", 
     xlab = "Income", 
     ylab = "Density",
     lwd = 2, col = "black")

IncDist_amelia(amelia_imp_bal_mcar_50, "blue")
IncDist_amelia(amelia_imp_bal_mcar_30, "red")
IncDist_amelia(amelia_imp_bal_mcar_10, "pink")
legend("topright", 
       legend = c("Initial Data", "bal_mcar_50", "bal_mcar_30", "bal_mcar_10"),
       col = c("black", "blue", "red", "pink"),
       lwd = 2,
       cex = 0.8)

IncDist_amelia(amelia_imp_bal_mar_50, "skyblue")
IncDist_amelia(amelia_imp_bal_mar_30, "violet")
IncDist_amelia(amelia_imp_bal_mar_10, "yellow")
legend("topright", 
       legend = c("Initial Data","bal_mar_50", "bal_mar_30", "bal_mar_10"),
       col = c("black", "skyblue", "violet", "yellow"),
       lwd = 2,
       cex = 0.8)

IncDist_amelia(amelia_imp_bal_mnar_50, "orange")
IncDist_amelia(amelia_imp_bal_mnar_30, "green")
IncDist_amelia(amelia_imp_bal_mnar_10, "brown")
legend("topright", 
       legend = c("Initial Data", "bal_mnar_50", "bal_mnar_30", "bal_mnar_10"),
       col = c("black", "orange", "green", "brown"),
       lwd = 2,
       cex = 0.8)

# Unbalanced Panel
DataTemp_amelia <- density(unbalanced_panel_data$Income, na.rm = TRUE)
plot(DataTemp_amelia, 
     main = "Income Distributions from Amelia - Unbalanced Panel (MNAR)", 
     xlab = "Income", 
     ylab = "Density",
     lwd = 2, col = "black")

IncDist_amelia(amelia_imp_unbal_mcar_50, "coral")
IncDist_amelia(amelia_imp_unbal_mcar_30, "salmon")
IncDist_amelia(amelia_imp_unbal_mcar_10, "lavender")
legend("topright", 
       legend = c("Initial Data", "unbal_mcar_50", "unbal_mcar_30", "unbal_mcar_10"),
       col = c("black", "coral", "salmon", "lavender"),
       lwd = 2,
       cex = 0.8)

IncDist_amelia(amelia_imp_unbal_mar_50, "gray")
IncDist_amelia(amelia_imp_unbal_mar_30, "gold")
IncDist_amelia(amelia_imp_unbal_mar_10, "orchid")
legend("topright", 
       legend = c("Initial Data", "unbal_mar_50", "unbal_mar_30", "unbal_mar_10"),
       col = c("black", "gray", "gold", "orchid"),
       lwd = 2,
       cex = 0.8)

IncDist_amelia(amelia_imp_unbal_mnar_50, "navy")
IncDist_amelia(amelia_imp_unbal_mnar_30, "darkgreen")
IncDist_amelia(amelia_imp_unbal_mnar_10, "steelblue")
legend("topright", 
       legend = c("Initial Data", "unbal_mnar_50", "unbal_mnar_30", "unbal_mnar_10"),
       col = c("black", "navy", "darkgreen", "steelblue"),
       lwd = 2,
       cex = 0.8)

### LSTM-Network

# Plot Distribution
IncDist_LSTM <- function(data, col) {
  DataTemp_LSTM <- density(data$Income, na.rm = TRUE)
  lines(DataTemp_LSTM, col = col, lwd = 2)
}

# Balanced Panel
DataTemp_LSTM <- density(balanced_panel_data$Income, na.rm = TRUE)
plot(DataTemp_LSTM, 
     main = "Income Distributions from LSTM - Balanced Panel (MNAR)", 
     xlab = "Income", 
     ylab = "Density",
     lwd = 2, col = "black")

IncDist_LSTM(lstm_bal_mcar_50, "blue")
IncDist_LSTM(lstm_bal_mcar_30, "red")
IncDist_LSTM(lstm_bal_mcar_10, "pink")
legend("topright", 
       legend = c("Initial Data", "bal_mcar_50", "bal_mcar_30", "bal_mcar_10"),
       col = c("black", "blue", "red", "pink"),
       lwd = 2,
       cex = 0.6)

IncDist_LSTM(lstm_bal_mar_50, "skyblue")
IncDist_LSTM(lstm_bal_mar_30, "violet")
IncDist_LSTM(lstm_bal_mar_10, "yellow")
legend("topright", 
       legend = c("Initial Data", "bal_mar_50", "bal_mar_30", "bal_mar_10"),
       col = c("black", "skyblue", "violet", "yellow"),
       lwd = 2,
       cex = 0.6)

IncDist_LSTM(lstm_bal_mnar_50, "orange")
IncDist_LSTM(lstm_bal_mnar_30, "green")
IncDist_LSTM(lstm_bal_mnar_10, "brown")
legend("topright", 
       legend = c("Initial Data", "bal_mnar_50", "bal_mnar_30", "bal_mnar_10"),
       col = c("black", "orange", "green", "brown"),
       lwd = 2,
       cex = 0.6)

# Unbalanced Panel
DataTemp_LSTM <- density(unbalanced_panel_data$Income, na.rm = TRUE)
plot(DataTemp_LSTM, 
     main = "Income Distributions from LSTM - Unbalanced Panel (MNAR)", 
     xlab = "Income", 
     ylab = "Density",
     lwd = 2, col = "black")

IncDist_LSTM(lstm_unbal_mcar_50, "coral")
IncDist_LSTM(lstm_unbal_mcar_30, "salmon")
IncDist_LSTM(lstm_unbal_mcar_10, "lavender")
legend("topright", 
       legend = c("Initial Data", "unbal_mcar_50", "unbal_mcar_30", "unbal_mcar_10"),
       col = c("black", "coral", "salmon", "lavender"),
       lwd = 2,
       cex = 0.6)

IncDist_LSTM(lstm_unbal_mar_50, "gray")
IncDist_LSTM(lstm_unbal_mar_30, "gold")
IncDist_LSTM(lstm_unbal_mar_10, "orchid")
legend("topright", 
       legend = c("Initial Data", "unbal_mar_50", "unbal_mar_30", "unbal_mar_10"),
       col = c("black", "gray", "gold", "orchid"),
       lwd = 2,
       cex = 0.6)

IncDist_LSTM(lstm_unbal_mnar_50, "navy")
IncDist_LSTM(lstm_unbal_mnar_30, "darkgreen")
IncDist_LSTM(lstm_unbal_mnar_10, "steelblue")
legend("topright", 
       legend = c("Initial Data", "unbal_mnar_50", "unbal_mnar_30", "unbal_mnar_10"),
       col = c("black", "navy", "darkgreen", "steelblue"),
       lwd = 2,
       cex = 0.6)

par(mfrow = c(1, 1))

###########################
### Coefficients Comparison
##########################

CoefficientsExtraction <- function(data) {
  
  pdata <- pdata.frame(data, index = c("ID", "Year")) # Converting into the panel format
  
  # Breusch-Pagan test
  BPTest <- plmtest(plm(Income ~ Education + Age, data = pdata, model = "pooling"), type = "bp")
  
  if (BPTest$p.value > 0.05) {
    model <- plm(Income ~ Year + Education + Age, data = pdata, model = "pooling") # Pooled OLS model for no panel effect
    print("Model: Pooled OLS")
  } else {
    # Hausman test to check the type of panel effect
    RandomEffectModel <- plm(Income ~ Year + Education + Age, data = pdata, model = "random")
    FixedEffectModel <- plm(Income ~ Year + Education + Age, data = pdata, model = "within")

    HausmanTest <- phtest(FixedEffectModel, RandomEffectModel)
    
    if (HausmanTest$p.value <= 0.05) {
      model <- FixedEffectModel # Fixed Effects model if correlation exists
      print("Model: Fixed Effect")
    } else {
      model <- RandomEffectModel # Random Effects model if correlation exists
      print("Model: Random Effect")
    }
  }
  
  
  Coefficients <- coef(model) # Coefficients extraction
  Coefficients <- as.data.frame(Coefficients)
  return(Coefficients)
}

balanced_panel_data_coef <- CoefficientsExtraction(balanced_panel_data)
unbalanced_panel_data_coef <- CoefficientsExtraction(unbalanced_panel_data)

### mice ###

# Balanced Panel
mice_Coff_bal <- function(mice_imp) {
  
  CoList <- list() # Empty list to store the coefficients from individual dataset
  
  for (i in 1:length(mice_imp)) {
    pdata <- pdata.frame(mice_imp[[i]], index = c("ID", "Year")) # Convert to panel data format
    RandomEffectModel <- plm(Income ~ Year + Education + Age, data = pdata, model = "random")
    CoList[[i]] <- coef(RandomEffectModel) # Extract coefficients
  }
  
  # Apply Rubin's Rules to generate the pooled coefficients
  CoefficientsList <- do.call(rbind, CoList)
  PooledCoefficient <- colMeans(CoefficientsList)
  CoDf <- data.frame(
    Coefficient = PooledCoefficient
  )
  
  return(CoDf)
}

# Apply analysis
Coeff_mice_bal_mcar_50 <- mice_Coff_bal(mice_bal_mcar_50)
Coeff_mice_bal_mcar_30 <- mice_Coff_bal(mice_bal_mcar_30)
Coeff_mice_bal_mcar_10 <- mice_Coff_bal(mice_bal_mcar_10)

Coeff_mice_bal_mar_50 <- mice_Coff_bal(mice_bal_mar_50)
Coeff_mice_bal_mar_30 <- mice_Coff_bal(mice_bal_mar_30)
Coeff_mice_bal_mar_10 <- mice_Coff_bal(mice_bal_mar_10)

Coeff_mice_bal_mnar_50 <- mice_Coff_bal(mice_bal_mnar_50)
Coeff_mice_bal_mnar_30 <- mice_Coff_bal(mice_bal_mnar_30)
Coeff_mice_bal_mnar_10 <- mice_Coff_bal(mice_bal_mnar_10)

# Unbalanced Panel
mice_Coff_unbal <- function(mice_imp) {
  
  CoList <- list() # Empty list to store the coefficients from individual dataset
  
  for (i in 1:length(mice_imp)) {
    pdata <- pdata.frame(mice_imp[[i]], index = c("ID", "Year")) # Convert to panel data format
    FixedEffectModel <- plm(Income ~ Year + Education + Age, data = pdata, model = "within")
    CoList[[i]] <- coef(FixedEffectModel) # Extract coefficients
  }
  
  # Apply Rubin's Rules to generate the pooled coefficients
  CoefficientsList <- do.call(rbind, CoList)
  PooledCoefficient <- colMeans(CoefficientsList)
  CoDf <- data.frame(
    Coefficient = PooledCoefficient
  )
  
  return(CoDf)
}

Coeff_mice_unbal_mcar_50 <- mice_Coff_unbal(mice_unbal_mcar_50)
Coeff_mice_unbal_mcar_30 <- mice_Coff_unbal(mice_unbal_mcar_30)
Coeff_mice_unbal_mcar_10 <- mice_Coff_unbal(mice_unbal_mcar_10)

Coeff_mice_unbal_mar_50 <- mice_Coff_unbal(mice_unbal_mar_50)
Coeff_mice_unbal_mar_30 <- mice_Coff_unbal(mice_unbal_mar_30)
Coeff_mice_unbal_mar_10 <- mice_Coff_unbal(mice_unbal_mar_10)

Coeff_mice_unbal_mnar_50 <- mice_Coff_unbal(mice_unbal_mnar_50)
Coeff_mice_unbal_mnar_30 <- mice_Coff_unbal(mice_unbal_mnar_30)
Coeff_mice_unbal_mnar_10 <- mice_Coff_unbal(mice_unbal_mnar_10)

### mitml ###

## Balanced Panel
mitml_Coff_bal <- function(mitml_imp) {
  
  CoList <- list() # Empty list to store the coefficients from individual dataset
  
  for (i in 1:length(mitml_imp)) {
    pdata <- pdata.frame(mitml_imp[[i]], index = c("ID", "Year")) # Convert into panel data format
    RandomEffectModel <- plm(Income ~ Year + Education + Age, data = pdata, model = "random")
    CoList[[i]] <- coef(RandomEffectModel) # Extract coefficients
  }
  
  # Apply Rubin's Rules to generate the pooled coefficients
  CoefficientsList <- do.call(rbind, CoList)
  PooledCoefficient <- colMeans(CoefficientsList, na.rm = TRUE)
  
  CoDf <- data.frame(
    Coefficient = PooledCoefficient
  )
  
  return(CoDf)
}

# Apply the function to each dataset and store results
Coeff_mitml_bal_mcar_50 <- mitml_Coff_bal(mitml_bal_mcar_50)
Coeff_mitml_bal_mcar_30 <- mitml_Coff_bal(mitml_bal_mcar_30)
Coeff_mitml_bal_mcar_10 <- mitml_Coff_bal(mitml_bal_mcar_10)

Coeff_mitml_bal_mar_50 <- mitml_Coff_bal(mitml_bal_mar_50)
Coeff_mitml_bal_mar_30 <- mitml_Coff_bal(mitml_bal_mar_30)
Coeff_mitml_bal_mar_10 <- mitml_Coff_bal(mitml_bal_mar_10)

Coeff_mitml_bal_mnar_50 <- mitml_Coff_bal(mitml_bal_mnar_50)
Coeff_mitml_bal_mnar_30 <- mitml_Coff_bal(mitml_bal_mnar_30)
Coeff_mitml_bal_mnar_10 <- mitml_Coff_bal(mitml_bal_mnar_10)

## Unbalanced Panel
mitml_Coff_unbal <- function(mitml_imp) {
  
  CoList <- list() # Empty list to store the coefficients from individual dataset
  
  for (i in 1:length(mitml_imp)) {
    pdata <- pdata.frame(mitml_imp[[i]], index = c("ID", "Year")) # Convert into panel data format
    FixedEffectModel <- plm(Income ~ Year + Education + Age, data = pdata, model = "within")
    CoList[[i]] <- coef(FixedEffectModel) # Extract coefficients
  }
  
  # Apply Rubin's Rules to generate the pooled coefficients
  CoefficientsList <- do.call(rbind, CoList)
  PooledCoefficient <- colMeans(CoefficientsList, na.rm = TRUE)
  
  CoDf <- data.frame(
    Coefficient = PooledCoefficient
  )
  
  return(CoDf)
}

# Apply the analysis function to each set of imputed datasets
Coeff_mitml_unbal_mcar_50 <- mitml_Coff_unbal(mitml_unbal_mcar_50)
Coeff_mitml_unbal_mcar_30 <- mitml_Coff_unbal(mitml_unbal_mcar_30)
Coeff_mitml_unbal_mcar_10 <- mitml_Coff_unbal(mitml_unbal_mcar_10)

Coeff_mitml_unbal_mar_50 <- mitml_Coff_unbal(mitml_unbal_mar_50)
Coeff_mitml_unbal_mar_30 <- mitml_Coff_unbal(mitml_unbal_mar_30)
Coeff_mitml_unbal_mar_10 <- mitml_Coff_unbal(mitml_unbal_mar_10)

Coeff_mitml_unbal_mnar_50 <- mitml_Coff_unbal(mitml_unbal_mnar_50)
Coeff_mitml_unbal_mnar_30 <- mitml_Coff_unbal(mitml_unbal_mnar_30)
Coeff_mitml_unbal_mnar_10 <- mitml_Coff_unbal(mitml_unbal_mnar_10)

### amelia ###
amelia_Coff_bal <- function(amelia_imp) {
  
  CoList <- list() # Empty list to store the coefficients from individual dataset
  amelia_imp <- amelia_imp$imputations
  for (i in 1:length(amelia_imp)) {
    pdata <- pdata.frame(amelia_imp[[i]], index = c("ID", "Year")) # Convert into panel data format
    RandomEffectModel <- plm(Income ~ Year + Education + Age, data = pdata, model = "random")
    CoList[[i]] <- coef(RandomEffectModel) # Extract coefficients
  }
  
  # Apply Rubin's Rules to generate the pooled coefficients
  CoefficientsList <- do.call(rbind, CoList)
  PooledCoefficient <- colMeans(CoefficientsList, na.rm = TRUE)
  
  CoDf <- data.frame(
    Coefficient = PooledCoefficient
  )
  
  return(CoDf)
}

# Apply the function to each dataset and store results
Coeff_amelia_bal_mcar_50 <- amelia_Coff_bal(amelia_bal_mcar_50)
Coeff_amelia_bal_mcar_30 <- amelia_Coff_bal(amelia_bal_mcar_30)
Coeff_amelia_bal_mcar_10 <- amelia_Coff_bal(amelia_bal_mcar_10)

Coeff_amelia_bal_mar_50 <- amelia_Coff_bal(amelia_bal_mar_50)
Coeff_amelia_bal_mar_30 <- amelia_Coff_bal(amelia_bal_mar_30)
Coeff_amelia_bal_mar_10 <- amelia_Coff_bal(amelia_bal_mar_10)

Coeff_amelia_bal_mnar_50 <- amelia_Coff_bal(amelia_bal_mnar_50)
Coeff_amelia_bal_mnar_30 <- amelia_Coff_bal(amelia_bal_mnar_30)
Coeff_amelia_bal_mnar_10 <- amelia_Coff_bal(amelia_bal_mnar_10)

### Unbalanced Panel
amelia_Coff_unbal <- function(amelia_imp) {
  
  CoList <- list() # Empty list to store the coefficients from individual dataset
  amelia_imp <- amelia_imp$imputations
  for (i in 1:length(amelia_imp)) {
    pdata <- pdata.frame(amelia_imp[[i]], index = c("ID", "Year")) # Convert into panel data format
    FixedEffectModel <- plm(Income ~ Year + Education + Age, data = pdata, model = "within")
    CoList[[i]] <- coef(FixedEffectModel) # Extract coefficients
  }
  
  # Apply Rubin's Rules to generate the pooled coefficients
  CoefficientsList <- do.call(rbind, CoList)
  PooledCoefficient <- colMeans(CoefficientsList, na.rm = TRUE)
  
  CoDf <- data.frame(
    Coefficient = PooledCoefficient
  )
  
  return(CoDf)
}

Coeff_amelia_unbal_mcar_50 <- amelia_Coff_unbal(amelia_unbal_mcar_50)
Coeff_amelia_unbal_mcar_30 <- amelia_Coff_unbal(amelia_unbal_mcar_30)
Coeff_amelia_unbal_mcar_10 <- amelia_Coff_unbal(amelia_unbal_mcar_10)

Coeff_amelia_unbal_mar_50 <- amelia_Coff_unbal(amelia_unbal_mar_50)
Coeff_amelia_unbal_mar_30 <- amelia_Coff_unbal(amelia_unbal_mar_30)
Coeff_amelia_unbal_mar_10 <- amelia_Coff_unbal(amelia_unbal_mar_10)

Coeff_amelia_unbal_mnar_50 <- amelia_Coff_unbal(amelia_unbal_mnar_50)
Coeff_amelia_unbal_mnar_30 <- amelia_Coff_unbal(amelia_unbal_mnar_30)
Coeff_amelia_unbal_mnar_10 <- amelia_Coff_unbal(amelia_unbal_mnar_10)

### LSTM ###
LSTM_Coff_bal <- function(data) {
  
  pdata <- pdata.frame(data, index = c("ID", "Year"))
  
  RandomEffectModel <- plm(Income ~ Year + Education + Age, data = pdata, model = "random")
  Coefficients <- coef(RandomEffectModel) # Coefficients extraction
  Coefficients <- as.data.frame(Coefficients)
  return(Coefficients)
}

# Apply the function to each dataset and store results
Coeff_lstm_bal_mcar_50 <- LSTM_Coff_bal(lstm_bal_mcar_50)
Coeff_lstm_bal_mcar_30 <- LSTM_Coff_bal(lstm_bal_mcar_30)
Coeff_lstm_bal_mcar_10 <- LSTM_Coff_bal(lstm_bal_mcar_10)

Coeff_lstm_bal_mar_50 <- LSTM_Coff_bal(lstm_bal_mar_50)
Coeff_lstm_bal_mar_30 <- LSTM_Coff_bal(lstm_bal_mar_30)
Coeff_lstm_bal_mar_10 <- LSTM_Coff_bal(lstm_bal_mar_10)

Coeff_lstm_bal_mnar_50 <- LSTM_Coff_bal(lstm_bal_mnar_50)
Coeff_lstm_bal_mnar_30 <- LSTM_Coff_bal(lstm_bal_mnar_30)
Coeff_lstm_bal_mnar_10 <- LSTM_Coff_bal(lstm_bal_mnar_10)

### Unbalanced Panel

LSTM_Coff_unbal <- function(data) {
  
  pdata <- pdata.frame(data, index = c("ID", "Year"))
  
  FixedEffectModel <- plm(Income ~ Year + Education + Age, data = pdata, model = "within")
  Coefficients <- coef(FixedEffectModel) # Coefficients extraction
  Coefficients <- as.data.frame(Coefficients)
  return(Coefficients)
}

Coeff_lstm_unbal_mcar_50 <- LSTM_Coff_unbal(lstm_unbal_mcar_50)
Coeff_lstm_unbal_mcar_30 <- LSTM_Coff_unbal(lstm_unbal_mcar_30)
Coeff_lstm_unbal_mcar_10 <- LSTM_Coff_unbal(lstm_unbal_mcar_10)

Coeff_lstm_unbal_mar_50 <- LSTM_Coff_unbal(lstm_unbal_mar_50)
Coeff_lstm_unbal_mar_30 <- LSTM_Coff_unbal(lstm_unbal_mar_30)
Coeff_lstm_unbal_mar_10 <- LSTM_Coff_unbal(lstm_unbal_mar_10)

Coeff_lstm_unbal_mnar_50 <- LSTM_Coff_unbal(lstm_unbal_mnar_50)
Coeff_lstm_unbal_mnar_30 <- LSTM_Coff_unbal(lstm_unbal_mnar_30)
Coeff_lstm_unbal_mnar_10 <- LSTM_Coff_unbal(lstm_unbal_mnar_10)
