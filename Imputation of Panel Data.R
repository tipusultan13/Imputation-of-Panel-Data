###################################
## Author: Tipu Sultan
###################################

########################
## Data Prepocessing
########################

# Set directory and load libraries
setwd("/Users/tipusultan/Documents/GitHub/Imputation-of-Panel-Data")

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
library(tidyr)

# Load and clean the data
RawData <- readRDS("population.RDS")
RawData = data.frame(RawData)
data = RawData[c("PID", "year", "EF44", "erwerbstyp", "inc.ind", "EF49", 
                 "vollzeit", "EF310", "EF46")]
colnames(data) <- c("ID", "Year", "Age", "EmploymentTypes", "IndividualIncome", 
                    "MaritalStatus", "EmploymentHours", "Education", "Sex")
summary(data)

# 'ID'
count(data, ID)

# 'Year'
count(data, Year)
data <- subset(data, Year >= 2013 & Year <= 2023) # Keeping the data from 2013 to 2023

# Age
count(data, Age)
summary(data$Age)

# Density curve for Age
AgeDist <- ggplot(data, aes(x = Age)) + 
  geom_density() +
  labs(title = "Density Curve of Age", x = "Age", y = "Density") +
  theme_minimal()
AgeDist

# 'EmploymentTypes': employed (0), unemployed (1), and not in workforce (2).
count(data, EmploymentTypes)

# Cleaning income column
summary(data$IndividualIncome)
count(data, IndividualIncome)
data$IndividualIncome[is.na(data$IndividualIncome)] <- 0
summary(data$IndividualIncome)

# Histogram for Individual Income
IndividualIncomeHist <- ggplot(data, aes(x = IndividualIncome)) + 
  geom_histogram(binwidth = 1000) +
  labs(title = "Income", x = "Income", y = "Frequency") +
  theme_minimal()
IndividualIncomeHist

# Apply log transformation to IndividualIncome (adding 1 to avoid log(0))
data$LogIndividualIncome <- log(data$IndividualIncome + 1)

IndividualIncomeLogHist <- ggplot(data, aes(x = LogIndividualIncome)) + 
  geom_histogram(binwidth = .10) +
  labs(title = "Logged Income", x = "Income", y = "Frequency")+
  theme_minimal()
IndividualIncomeLogHist

DataTempAge <- data[data$Age >= 18 & data$Age <= 67, ]
AFilteredIncomeLogHist <- ggplot(DataTempAge, aes(x = LogIndividualIncome)) + 
  geom_histogram(binwidth = .10) +
  labs(title = "Logged Income (Age: 18-67)", x = "Income", y = "Frequency")+
  theme_minimal()
AFilteredIncomeLogHist

DataTempIncome <- data[data$IndividualIncome != 0, ]
IFilteredIncomeLogHist <- ggplot(DataTempIncome, aes(x = LogIndividualIncome)) + 
  geom_histogram(binwidth = .10) +
  labs(title = "Logged Income (Without 0)", x = "Income", y = "Frequency")+
  theme_minimal()
IFilteredIncomeLogHist

grid.arrange(IndividualIncomeHist, IndividualIncomeLogHist, AFilteredIncomeLogHist, IFilteredIncomeLogHist, ncol = 2)

# MaritalStatus: Single (1), Married (2), Widowed (3), Divorced (4)
count(data, MaritalStatus)
summary(data$MaritalStatus)

# EmploymentHours:Part-time (0), Full-time (1), Not working (NA)
count(data, EmploymentHours)
data$EmploymentHours[is.na(data$EmploymentHours)] <- 2
summary(data$EmploymentHours)

# Education
count(data, Education)
data$Education[is.na(data$Education)] <- 7
summary(data$Education)

# Sex
count(data, Sex)
summary(data$Sex)

sum(is.na(data)) #Total number of missing values
colnames(data)[colnames(data) == "LogIndividualIncome"] <- "Income"
data <- data[c("ID", "Year", "Age", "Income", "MaritalStatus", "EmploymentTypes", "EmploymentHours", "Education", "Sex")]
summary(data)

###########################################
## Simulating Balanced and Unbalanced Panel
###########################################

## Balanced Panel ##
####################

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

## Unalanced Panel ##
#####################

SampleData <- data.frame()

# Loop through each year to ensure 50483 unique ID-Year combinations
for (yr in unique(data$Year)) {
  # Filter the data for the current year
  YearData <- data %>% filter(Year == yr)
  
  # Sample 50483 unique ID-Year combinations for this year
  SampleTemp <- YearData %>%
    distinct(ID, Year, .keep_all = TRUE) %>%
    slice_sample(n = 50483)
  
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

#########################################
## Simulate Missingness in Balanced Panel
#########################################

#### MCAR ####
##############

#### 50% ####

p_mis_50 <- 0.50
num_rows <- nrow(balanced_panel_data)  # Get the number of rows in balanced_panel_data
mis_simulated_mcar_50 <- sample(1:num_rows, p_mis_50 * num_rows, replace = FALSE)    
balanced_panel_data_mcar_50 <- balanced_panel_data
balanced_panel_data_mcar_50[mis_simulated_mcar_50, 4] <- NA
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
balanced_panel_data_mcar_30[mis_simulated_mcar_30, 4] <- NA
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
balanced_panel_data_mcar_10[mis_simulated_mcar_10, 4] <- NA
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

# Depending on EmploymentTypes, Age, and random error
mis_simulated_mar_50 <- 0.5 + 0.1 * balanced_panel_data_mar_50$EmploymentTypes + 
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

# Depending on EmploymentTypes, Age, and random error
mis_simulated_mar_30 <- 0.7 + 0.1 * balanced_panel_data_mar_30$EmploymentTypes + 
  0.2 * balanced_panel_data_mar_30$Age + 
  rnorm(nrow(balanced_panel_data_mar_30), 0, 3)

mis_simulated_mar_30 <- mis_simulated_mar_30 < quantile(mis_simulated_mar_30, p_mis_30) # All below the 50% quantile are set to missing
mean(as.numeric(mis_simulated_mar_30))
balanced_panel_data_mar_30$Income[mis_simulated_mar_30] <- NA # Set values to NA in Income where missingness occurs
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

# Depending on EmploymentTypes, Age, and random error
mis_simulated_mar_10 <- 0.1 + 0.1 * balanced_panel_data_mar_10$EmploymentTypes + 
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
mis_simulated_mnar_50 <- 0.5 + .2 * balanced_panel_data_mnar_50$EmploymentTypes + 
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
mis_simulated_mnar_30 <- 0.7 + 0.1 * balanced_panel_data_mnar_30$EmploymentTypes + 
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
mis_simulated_mnar_10 <- 0.1 + 0.1 * balanced_panel_data_mnar_10$EmploymentTypes + 
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

###########################################
## Simulating Missingness in Unalanced Panel
###########################################

#### MCAR ####
##############

#### 50% ####

p_mis_50 <- 0.50
num_rows <- nrow(unbalanced_panel_data)  # Get the number of rows in balanced_panel_data
mis_simulated_mcar_50 <- sample(1:num_rows, p_mis_50 * num_rows, replace = FALSE)    
unbalanced_panel_data_mcar_50 <- unbalanced_panel_data
unbalanced_panel_data_mcar_50[mis_simulated_mcar_50, 4] <- NA
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
unbalanced_panel_data_mcar_30[mis_simulated_mcar_30, 4] <- NA
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
unbalanced_panel_data_mcar_10[mis_simulated_mcar_10, 4] <- NA
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

# Depending on EmploymentTypes, Age, and random error
mis_simulated_mar_50 <- 0.5 + 0.1 * unbalanced_panel_data_mar_50$EmploymentTypes + 
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

# Depending on EmploymentTypes, Age, and random error
mis_simulated_mar_30 <- 0.7 + 0.1 * unbalanced_panel_data_mar_30$EmploymentTypes + 
  0.2 * unbalanced_panel_data_mar_30$Age + 
  rnorm(nrow(unbalanced_panel_data_mar_30), 0, 3)

mis_simulated_mar_30 <- mis_simulated_mar_30 < quantile(mis_simulated_mar_30, p_mis_30) # All below the 50% quantile are set to missing
mean(as.numeric(mis_simulated_mar_30))

unbalanced_panel_data_mar_30$Income[mis_simulated_mar_30] <- NA # Set values to NA in Income where missingness occurs
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

# Depending on EmploymentTypes, Age, and random error
mis_simulated_mar_10 <- 0.1 + 0.1 * unbalanced_panel_data_mar_10$EmploymentTypes + 
  0.2 * unbalanced_panel_data_mar_10$Age + 
  rnorm(nrow(unbalanced_panel_data_mar_10), 0, 3)

mis_simulated_mar_10 <- mis_simulated_mar_10 < quantile(mis_simulated_mar_10, p_mis_10) # All below the 50% quantile are set to missing
mean(as.numeric(mis_simulated_mar_10))

unbalanced_panel_data_mar_10$Income[mis_simulated_mar_10] <- NA # Set values to NA in Income where missingness occurs
summary(unbalanced_panel_data_mar_10)

# Visualize the missing data pattern
aggr(unbalanced_panel_data_mar_10, 
     col = c('navyblue', 'red'), 
     numbers = TRUE, 
     sortVars = TRUE, 
     labels = names(unbalanced_panel_data_mar_10), 
     cex.axis = .7, 
     gap = 3, 
     ylab = c("Missing data", "Pattern"))

#### MNAR ####
##############

#### Probabilistic, Linear Regression model ####
#### 50% ####

p_mis_50 <- .50
unbalanced_panel_data_mnar_50 <- unbalanced_panel_data
# the missing of a value now also depends on Income itself
mis_simulated_mnar_50 <- 0.5 + 0.1 * unbalanced_panel_data_mnar_50$EmploymentTypes + 
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
# the missing of a value now also depends on Income itself
mis_simulated_mnar_30 <-0.7 + 0.1 * unbalanced_panel_data_mnar_30$EmploymentTypes + 
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
# the missing of a value now also depends on Income itself
mis_simulated_mnar_10 <- 0.1 + 0.1 * unbalanced_panel_data_mnar_10$EmploymentTypes + 
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
### Datasets Overview and Formating
####################################

## Format Vriable to Their Originl Format
convert_DataTypes <- function(df) {
  df$ID <- as.factor(df$ID)
  df$Year <- as.factor(df$Year)
  df$age <- as.integer(df$Age)
  df$EmploymentTypes <- as.factor(df$EmploymentTypes)
  df$Income <- as.numeric(df$Income)
  df$MaritalStatus <- as.factor(df$MaritalStatus)
  df$EmploymentHours <- as.factor(df$EmploymentHours)
  df$Education <- as.factor(df$Education)
  df$Sex <- as.factor(df$Sex)
  df <- pdata.frame(df, index = c("ID", "Year")) # Convert the data frame to a panel data frame
  df <- df[c("ID", "Year", "Age", "EmploymentTypes", "Income", "MaritalStatus", "EmploymentHours", "Education", "Sex")]
  return(df)
}

balanced_panel_data <- convert_DataTypes(balanced_panel_data)
unbalanced_panel_data <- convert_DataTypes(unbalanced_panel_data)

balanced_panel_data_mcar_50 <- convert_DataTypes(balanced_panel_data_mcar_50)
balanced_panel_data_mcar_30 <- convert_DataTypes(balanced_panel_data_mcar_30)
balanced_panel_data_mcar_10 <- convert_DataTypes(balanced_panel_data_mcar_10)

balanced_panel_data_mar_50 <- convert_DataTypes(balanced_panel_data_mar_50)
balanced_panel_data_mar_30 <- convert_DataTypes(balanced_panel_data_mar_30)
balanced_panel_data_mar_10 <- convert_DataTypes(balanced_panel_data_mar_10)

balanced_panel_data_mnar_50 <- convert_DataTypes(balanced_panel_data_mnar_50)
balanced_panel_data_mnar_30 <- convert_DataTypes(balanced_panel_data_mnar_30)
balanced_panel_data_mnar_10 <- convert_DataTypes(balanced_panel_data_mnar_10)

unbalanced_panel_data_mcar_50 <- convert_DataTypes(unbalanced_panel_data_mcar_50)
unbalanced_panel_data_mcar_30 <- convert_DataTypes(unbalanced_panel_data_mcar_30)
unbalanced_panel_data_mcar_10 <- convert_DataTypes(unbalanced_panel_data_mcar_10)

unbalanced_panel_data_mar_50 <- convert_DataTypes(unbalanced_panel_data_mar_50)
unbalanced_panel_data_mar_30 <- convert_DataTypes(unbalanced_panel_data_mar_30)
unbalanced_panel_data_mar_10 <- convert_DataTypes(unbalanced_panel_data_mar_10)

unbalanced_panel_data_mnar_50 <- convert_DataTypes(unbalanced_panel_data_mnar_50)
unbalanced_panel_data_mnar_30 <- convert_DataTypes(unbalanced_panel_data_mnar_30)
unbalanced_panel_data_mnar_10 <- convert_DataTypes(unbalanced_panel_data_mnar_10)

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
## Data Imputation
######################

## mice ##
##########

packageVersion("mice")

StartTime_mice <- Sys.time()  # Starting time

Data_Imputation_mice <- function(data, IDColumn = "ID", TargetColumn = "Income", 
                                 LaggedColumns = c("EmploymentTypes", "MaritalStatus", "Sex", "Income"), 
                                 m = 5, maxit = 100, method = "pmm") {
  
  # Making ID factor and year numeric for proper lagging
  data[[IDColumn]] <- as.factor(data[[IDColumn]])
  data$Year <- as.numeric(data$Year)
  
  # Generate lagging variables
  for (col in LaggedColumns) {
    LaggedNames <- paste0("lag_", col)
    data <- data %>%
      group_by(!!sym(IDColumn)) %>%
      arrange(Year, .by_group = TRUE) %>%
      mutate(!!sym(LaggedNames) := lag(!!sym(col))) %>%
      ungroup()
  }
  
  # Remove the rows due to existing missing values in other columns. For example: first year for each ID
  LaggedVars <- paste0("lag_", LaggedColumns)
  data <- data %>% filter(rowSums(is.na(select(., all_of(LaggedVars)))) == 0)
  
  PredMat <- make.predictorMatrix(data) # matrix of predictors
  PredMat[, IDColumn] <- 0 # Remove ID from the list of predictors
  PredMat[, ] <- 0
  PredMat[colnames(PredMat) %in% c("lag_EmploymentTypes", "lag_MaritalStatus", 
                                           "lag_Sex", "lag_Income", "Age"), TargetColumn] <- 1
  
  ImputedData <- mice(data, m = m, maxit = maxit, method = method, predictorMatrix = PredMat) # Implement mice
  
  # Get the original rows and the imputed Income
  CompletedData <- lapply(1:m, function(i) complete(ImputedData, action = i))
  OriginalColumns <- c("ID", "Year", "Age", "EmploymentTypes", "Income", "MaritalStatus", 
                     "EmploymentHours", "Education", "Sex")
  CompletedData <- lapply(CompletedData, function(df) df[, OriginalColumns])
  return(CompletedData)
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
print(ExecutionTime_mice) # Time difference of 13.11498 mins

## mitml ##
###########

packageVersion("mitml")

## Balanced ##

StartTime_mitml <- Sys.time()  # Starting time

# Estimate the models
FE_Model <- plm(Income ~ Age + EmploymentTypes + MaritalStatus + Sex, data = balanced_panel_data, model = "within")
RE_Model <- plm(Income ~ Age + EmploymentTypes + MaritalStatus + Sex, data = balanced_panel_data, model = "random")

HausmanTest <- phtest(FE_Model, RE_Model) # Perform the Hausman test to compare the fixed and random effects models
print(HausmanTest)
# p-value is less than 2.2e-16, which is < 0.05. So null hypothesis can be rejected.
# implying that the fixed effects in Age, EmploymentTypes, MaritulStatus, and Sex is more appropriate.

# Function to impute data
Data_Imputation_mitml_Bal <- function(panel_data) {
  
  SelectedData <- panel_data[c("ID", "Year", "Age", "EmploymentTypes", "Income", "MaritalStatus", "EmploymentHours", "Education", "Sex")]
  
  # Define the type vector and assign column names
  type <- c(-2, -1, 2, 2, 1, 2, 0, 0, 2)
  names(type) <- colnames(SelectedData)
  
  ImputedData <- panImpute(panel_data, type = type, n.burn = 1000, n.iter = 100, m = 5)   # Impute missing data
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

# Estimate the models
FE_Model <- plm(Income ~ Age + EmploymentTypes + MaritalStatus + Sex, data = unbalanced_panel_data, model = "within")
RE_Model <- plm(Income ~ Age + EmploymentTypes + MaritalStatus + Sex, data = unbalanced_panel_data, model = "random")

HausmanTest <- phtest(FE_Model, RE_Model) # Perform the Hausman test to compare the fixed and random effects models
print(HausmanTest)
# p-value is less than 2.2e-16, which is < 0.05. So null hypothesis can be rejected.
# implying that the fixed effects in Age, EmploymentTypes, MaritulStatus, and Sex is more appropriate.

# Function to impute data for unbalanced panels
Data_Imputation_mitml_Unbal <- function(panel_data) {
  
  SelectedData <- panel_data[c("ID", "Year", "Age", "EmploymentTypes", "Income", "MaritalStatus", "EmploymentHours", "Education", "Sex")]
  
  # Define the type vector and assign column names
  type <- c(-2, -1, 2, 2, 1, 2, 0, 0, 2)
  names(type) <- colnames(SelectedData)
  
  ImputedData <- panImpute(SelectedData, type = type, n.burn = 1000, n.iter = 100, m = 5)   # Impute missing data
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
print(ExecutionTime_mitml) # Time difference of 25.23591 mins


## amelia
##########

packageVersion("Amelia")

StartTime_amelia <- Sys.time()  # Starting time

# Function to impute data
Data_Imputation_Amelia <- function(data) {
  
  pdata = data[c("ID", "Year", "Age", "EmploymentTypes", "Income", "MaritalStatus", "Sex")]
  pdata$Year <- as.numeric(as.character(pdata$Year))
  
  # Perform the imputation using Amelia
  ImputedData <- amelia(
    pdata,
    m = 5,
    ts = "Year",
    cs = "ID",
    noms = c("EmploymentTypes", "MaritalStatus", "Sex")
  )
  # Add exogenous variables (EmploymentHours and Education) back to each imputed dataset and restore original order
  ImputedData <- lapply(ImputedData$imputations, function(df) {
    df <- cbind(df, data[c("Education", "EmploymentHours")])
    df <- df[c("ID", "Year", "Age", "EmploymentTypes", "Income", "MaritalStatus", 
               "EmploymentHours", "Education", "Sex")]
    return(df)
  })
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
print(ExecutionTime_amelia) # Time difference of 2.38555 hours

## LSTM Network ##
################

packageVersion("keras")

StartTime_LSTM <- Sys.time()  # Starting time

Data_Imputation_LSTM <- function(Data) {
  
  # Removing the exogenious variable to keep their effect
  data <- Data[c("ID", "Year", "Age", "EmploymentTypes", "Income", "MaritalStatus", "Sex")]
  
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
           EmploymentTypes = as.numeric(factor(EmploymentTypes)),
           MaritalStatus = as.numeric(factor(MaritalStatus)),
           Sex = as.numeric(factor(Sex)))
  
  # Prepare data to train the model, excluding ID
  TrainX <- as.matrix(CompleteData %>% select(Year, EmploymentTypes, Age, MaritalStatus, Sex))
  TrainY <- CompleteData$Income
  
  # Reshape the data into 3D to make LSTM calculation easier (samples, timesteps=1, features=5)
  TrainX <- array(TrainX, dim = c(nrow(TrainX), 1, ncol(TrainX)))
  
  # Build the model
  model <- keras_model_sequential() %>%
    layer_lstm(units = 50, input_shape = c(1, 5), return_sequences = FALSE) %>%
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
           EmploymentTypes = as.numeric(factor(EmploymentTypes)),
           MaritalStatus = as.numeric(factor(MaritalStatus)),
           Sex = as.numeric(factor(Sex)),
           Age = (Age - AgeM) / AgeSD)
  
  XMissing <- as.matrix(MissingValues %>% select(Year, EmploymentTypes, Age, MaritalStatus, Sex))
  XMissing <- array(XMissing, dim = c(nrow(XMissing), 1, ncol(XMissing)))
  
  PredictedIncome <- model %>% predict(XMissing)
  PredictedIncome <- PredictedIncome * IncomeSD + IncomeM
  data$Income[MissingRows] <- PredictedIncome
  data <- cbind(data, Data[c("Education", "EmploymentHours")])
  data <- data[c("ID", "Year", "Age", "EmploymentTypes", "Income", "MaritalStatus", "EmploymentHours", "Education", "Sex")]
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
print(ExecutionTime_LSTM) # Time difference of 2.409152 hours

#############################
# Distribution Comparason #
#############################

##### mice #####
###############
miceData <- function(data) {
  
  CombinedData <- do.call(rbind, data)
  return(CombinedData)
}
 
# Data Combination
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

### Balanced data

DataTemp_mice <- density(balanced_panel_data$Income, na.rm = TRUE)
par(mfrow = c(2, 3))
plot(DataTemp_mice, 
     main = "Balanced panel (MNAR)", 
     xlab = "Income", 
     ylab = "Density",
     lwd = 2, col = "black")

IncDist_mice(mice_imp_bal_mcar_50, "blue")
IncDist_mice(mice_imp_bal_mcar_30, "red")
IncDist_mice(mice_imp_bal_mcar_10, "green")
legend("topright", 
       legend = c("Initial Data", "bal_mcar_50", "bal_mcar_30", "bal_mcar_10"),
       col = c("black", "blue", "red", "green"),
       lwd = 2,
       cex = 0.8)

IncDist_mice(mice_imp_bal_mar_50, "blue")
IncDist_mice(mice_imp_bal_mar_30, "red")
IncDist_mice(mice_imp_bal_mar_10, "green")
legend("topright", 
       legend = c("Initial Data", "bal_mar_50", "bal_mar_30", "bal_mar_10"),
       col = c("black", "blue", "red", "green"),
       lwd = 2,
       cex = 0.8)

IncDist_mice(mice_imp_bal_mnar_50, "blue")
IncDist_mice(mice_imp_bal_mnar_30, "red")
IncDist_mice(mice_imp_bal_mnar_10, "green")
legend("topright", 
       legend = c("Initial Data", "bal_mnar_50", "bal_mnar_30", "bal_mnar_10"),
       col = c("black", "blue", "red", "green"),
       lwd = 2,
       cex = 0.8)

### Unbalanced data

DataTemp_mice <- density(unbalanced_panel_data$Income, na.rm = TRUE)
plot(DataTemp_mice, 
     main = "Unbalanced panel (MNAR)", 
     xlab = "Income", 
     ylab = "Density",
     lwd = 2, col = "black")

IncDist_mice(mice_imp_unbal_mcar_50, "blue")
IncDist_mice(mice_imp_unbal_mcar_30, "red")
IncDist_mice(mice_imp_unbal_mcar_10, "green")
legend("topright", 
       legend = c("Initial Data", "unbal_mcar_50", "unbal_mcar_30", "unbal_mcar_10"),
       col = c("black", "blue", "red", "green"),
       lwd = 2,
       cex = 0.8)

IncDist_mice(mice_imp_unbal_mar_50, "blue")
IncDist_mice(mice_imp_unbal_mar_30, "red")
IncDist_mice(mice_imp_unbal_mar_10, "green")
legend("topright", 
       legend = c("Initial Data","unbal_mar_50", "unbal_mar_30", "unbal_mar_10"),
       col = c("black", "blue", "red", "green"),
       lwd = 2,
       cex = 0.8)

IncDist_mice(mice_imp_unbal_mnar_50, "blue")
IncDist_mice(mice_imp_unbal_mnar_30, "red")
IncDist_mice(mice_imp_unbal_mnar_10, "green")
legend("topright", 
       legend = c("Initial Data", "unbal_mnar_50", "unbal_mnar_30", "unbal_mnar_10"),
       col = c("black", "blue", "red", "green"),
       lwd = 2,
       cex = 0.8)

##### mitml #####
################

# Data Combination
mitmlData <- function(data){
  CombinedData <- do.call(rbind, data)
  return(CombinedData)
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

# Balanced Data

DataTemp_mitml <- density(balanced_panel_data$Income, na.rm = TRUE)
plot(DataTemp_mitml, 
     main = "Balanced panel (MNAR)", 
     xlab = "Income", 
     ylab = "Density",
     lwd = 2, col = "black")

IncDist_mitml(mitml_imp_bal_mcar_50, "blue")
IncDist_mitml(mitml_imp_bal_mcar_30, "red")
IncDist_mitml(mitml_imp_bal_mcar_10, "green")
legend("topright", 
       legend = c("Initial Data", "bal_mcar_50", "bal_mcar_30", "bal_mcar_10"),
       col = c("black", "blue", "red", "green"),
       lwd = 2,
       cex = 0.8)

IncDist_mitml(mitml_imp_bal_mar_50, "blue")
IncDist_mitml(mitml_imp_bal_mar_30, "red")
IncDist_mitml(mitml_imp_bal_mar_10, "green")
legend("topright", 
       legend = c("Initial Data", "bal_mar_50", "bal_mar_30", "bal_mar_10"),
       col = c("black","blue", "red", "green"),
       lwd = 2,
       cex = 0.8)

IncDist_mitml(mitml_imp_bal_mnar_50, "blue")
IncDist_mitml(mitml_imp_bal_mnar_30, "red")
IncDist_mitml(mitml_imp_bal_mnar_10, "green")
legend("topright", 
       legend = c("Initial Data", "bal_mnar_50", "bal_mnar_30", "bal_mnar_10"),
       col = c("black", "blue", "red", "green"),
       lwd = 2,
       cex = 0.8)

### Unbalanced

DataTemp_mitml <- density(unbalanced_panel_data$Income, na.rm = TRUE)
plot(DataTemp_mitml, 
     main = "Unbalanced Panel (MNAR)", 
     xlab = "Income", 
     ylab = "Density",
     lwd = 2, col = "black")

IncDist_mitml(mitml_imp_unbal_mcar_50, "blue")
IncDist_mitml(mitml_imp_unbal_mcar_30, "red")
IncDist_mitml(mitml_imp_unbal_mcar_10, "green")
legend("topright", 
       legend = c("Initial Data", "unbal_mcar_50", "unbal_mcar_30", "unbal_mcar_10"),
       col = c("black", "blue", "red", "green"),
       lwd = 2,
       cex = 0.8)

IncDist_mitml(mitml_imp_unbal_mar_50, "blue")
IncDist_mitml(mitml_imp_unbal_mar_30, "red")
IncDist_mitml(mitml_imp_unbal_mar_10, "green")
legend("topright", 
       legend = c("Initial Data", "unbal_mar_50", "unbal_mar_30", "unbal_mar_10"),
       col = c("black", "blue", "red", "green"),
       lwd = 2,
       cex = 0.8)

IncDist_mitml(mitml_imp_unbal_mnar_50, "blue")
IncDist_mitml(mitml_imp_unbal_mnar_30, "red")
IncDist_mitml(mitml_imp_unbal_mnar_10, "green")
legend("topright", 
       legend = c("Initial Data", "unbal_mnar_50", "unbal_mnar_30", "unbal_mnar_10"),
       col = c("black", "blue", "red", "green"),
       lwd = 2,
       cex = 0.8)

### amelia ###
##############

# Data Combination
ameliaData <- function(data){

  CombinedData <- do.call(rbind, data)
  return(CombinedData)
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
     main = "Balanced Panel (MNAR)", 
     xlab = "Income", 
     ylab = "Density",
     lwd = 2, col = "black")

IncDist_amelia(amelia_imp_bal_mcar_50, "blue")
IncDist_amelia(amelia_imp_bal_mcar_30, "red")
IncDist_amelia(amelia_imp_bal_mcar_10, "green")
legend("topright", 
       legend = c("Initial Data", "bal_mcar_50", "bal_mcar_30", "bal_mcar_10"),
       col = c("black", "blue", "red", "green"),
       lwd = 2,
       cex = 0.8)

IncDist_amelia(amelia_imp_bal_mar_50, "blue")
IncDist_amelia(amelia_imp_bal_mar_30, "red")
IncDist_amelia(amelia_imp_bal_mar_10, "green")
legend("topright", 
       legend = c("Initial Data","bal_mar_50", "bal_mar_30", "bal_mar_10"),
       col = c("black", "blue", "red", "green"),
       lwd = 2,
       cex = 0.8)

IncDist_amelia(amelia_imp_bal_mnar_50, "blue")
IncDist_amelia(amelia_imp_bal_mnar_30, "red")
IncDist_amelia(amelia_imp_bal_mnar_10, "green")
legend("topright", 
       legend = c("Initial Data", "bal_mnar_50", "bal_mnar_30", "bal_mnar_10"),
       col = c("black", "blue", "red", "green"),
       lwd = 2,
       cex = 0.8)

# Unbalanced Panel
DataTemp_amelia <- density(unbalanced_panel_data$Income, na.rm = TRUE)
plot(DataTemp_amelia, 
     main = "Unbalanced Panel (MNAR)", 
     xlab = "Income", 
     ylab = "Density",
     lwd = 2, col = "black")

IncDist_amelia(amelia_imp_unbal_mcar_50, "blue")
IncDist_amelia(amelia_imp_unbal_mcar_30, "red")
IncDist_amelia(amelia_imp_unbal_mcar_10, "green")
legend("topright", 
       legend = c("Initial Data", "unbal_mcar_50", "unbal_mcar_30", "unbal_mcar_10"),
       col = c("black", "blue", "red", "green"),
       lwd = 2,
       cex = 0.8)

IncDist_amelia(amelia_imp_unbal_mar_50, "blue")
IncDist_amelia(amelia_imp_unbal_mar_30, "red")
IncDist_amelia(amelia_imp_unbal_mar_10, "green")
legend("topright", 
       legend = c("Initial Data", "unbal_mar_50", "unbal_mar_30", "unbal_mar_10"),
       col = c("black", "blue", "red", "green"),
       lwd = 2,
       cex = 0.8)

IncDist_amelia(amelia_imp_unbal_mnar_50, "blue")
IncDist_amelia(amelia_imp_unbal_mnar_30, "red")
IncDist_amelia(amelia_imp_unbal_mnar_10, "green")
legend("topright", 
       legend = c("Initial Data", "unbal_mnar_50", "unbal_mnar_30", "unbal_mnar_10"),
       col = c("black", "blue", "red", "green"),
       lwd = 2,
       cex = 0.8)

### LSTM-Network ###
####################

# Plot Distribution
IncDist_LSTM <- function(data, col) {
  DataTemp_LSTM <- density(data$Income, na.rm = TRUE)
  lines(DataTemp_LSTM, col = col, lwd = 2)
}

# Balanced Panel
DataTemp_LSTM <- density(balanced_panel_data$Income, na.rm = TRUE)
plot(DataTemp_LSTM, 
     main = "Balanced Panel (MNAR)", 
     xlab = "Income", 
     ylab = "Density",
     lwd = 2, col = "black")

IncDist_LSTM(lstm_bal_mcar_50, "blue")
IncDist_LSTM(lstm_bal_mcar_30, "red")
IncDist_LSTM(lstm_bal_mcar_10, "green")
legend("topright", 
       legend = c("Initial Data", "bal_mcar_50", "bal_mcar_30", "bal_mcar_10"),
       col = c("black", "blue", "red", "green"),
       lwd = 2,
       cex = 0.8)

IncDist_LSTM(lstm_bal_mar_50, "blue")
IncDist_LSTM(lstm_bal_mar_30, "red")
IncDist_LSTM(lstm_bal_mar_10, "green")
legend("topright", 
       legend = c("Initial Data", "bal_mar_50", "bal_mar_30", "bal_mar_10"),
       col = c("black", "blue", "red", "green"),
       lwd = 2,
       cex = 0.8)

IncDist_LSTM(lstm_bal_mnar_50, "blue")
IncDist_LSTM(lstm_bal_mnar_30, "red")
IncDist_LSTM(lstm_bal_mnar_10, "green")
legend("topright", 
       legend = c("Initial Data", "bal_mnar_50", "bal_mnar_30", "bal_mnar_10"),
       col = c("black", "blue", "red", "green"),
       lwd = 2,
       cex = 0.8)

# Unbalanced Panel
DataTemp_LSTM <- density(unbalanced_panel_data$Income, na.rm = TRUE)
plot(DataTemp_LSTM, 
     main = "Unbalanced Panel (MNAR)", 
     xlab = "Income", 
     ylab = "Density",
     lwd = 2, col = "black")

IncDist_LSTM(lstm_unbal_mcar_50, "blue")
IncDist_LSTM(lstm_unbal_mcar_30, "red")
IncDist_LSTM(lstm_unbal_mcar_10, "green")
legend("topright", 
       legend = c("Initial Data", "unbal_mcar_50", "unbal_mcar_30", "unbal_mcar_10"),
       col = c("black", "blue", "red", "green"),
       lwd = 2,
       cex = 0.8)

IncDist_LSTM(lstm_unbal_mar_50, "blue")
IncDist_LSTM(lstm_unbal_mar_30, "red")
IncDist_LSTM(lstm_unbal_mar_10, "green")
legend("topright", 
       legend = c("Initial Data", "unbal_mar_50", "unbal_mar_30", "unbal_mar_10"),
       col = c("black", "blue", "red", "green"),
       lwd = 2,
       cex = 0.8)

IncDist_LSTM(lstm_unbal_mnar_50, "blue")
IncDist_LSTM(lstm_unbal_mnar_30, "red")
IncDist_LSTM(lstm_unbal_mnar_10, "green")
legend("topright", 
       legend = c("Initial Data", "unbal_mnar_50", "unbal_mnar_30", "unbal_mnar_10"),
       col = c("black", "blue", "red", "green"),
       lwd = 2,
       cex = 0.8)

par(mfrow = c(1, 1))

#######################################
# Conditional Distribution Comparason #
#######################################












###################################
# Correlation Matrix Comparason #
###################################

### Balanced Panel ###

# MargedDataframe
MargedDataframe_Bal <- list(
  mice_imp_bal_mcar_50 = mice_imp_bal_mcar_50,
  mice_imp_bal_mcar_30 = mice_imp_bal_mcar_30,
  mice_imp_bal_mcar_10 = mice_imp_bal_mcar_10,
  mice_imp_bal_mar_50 = mice_imp_bal_mar_50,
  mice_imp_bal_mar_30 = mice_imp_bal_mar_30,
  mice_imp_bal_mar_10 = mice_imp_bal_mar_10,
  mice_imp_bal_mnar_50 = mice_imp_bal_mnar_50,
  mice_imp_bal_mnar_30 = mice_imp_bal_mnar_30,
  mice_imp_bal_mnar_10 = mice_imp_bal_mnar_10,
  mitml_imp_bal_mcar_50 = mitml_imp_bal_mcar_50,
  mitml_imp_bal_mcar_30 = mitml_imp_bal_mcar_30,
  mitml_imp_bal_mcar_10 = mitml_imp_bal_mcar_10,
  mitml_imp_bal_mar_50 = mitml_imp_bal_mar_50,
  mitml_imp_bal_mar_30 = mitml_imp_bal_mar_30,
  mitml_imp_bal_mar_10 = mitml_imp_bal_mar_10,
  mitml_imp_bal_mnar_50 = mitml_imp_bal_mnar_50,
  mitml_imp_bal_mnar_30 = mitml_imp_bal_mnar_30,
  mitml_imp_bal_mnar_10 = mitml_imp_bal_mnar_10,
  amelia_imp_bal_mcar_50 = amelia_imp_bal_mcar_50,
  amelia_imp_bal_mcar_30 = amelia_imp_bal_mcar_30,
  amelia_imp_bal_mcar_10 = amelia_imp_bal_mcar_10,
  amelia_imp_bal_mar_50 = amelia_imp_bal_mar_50,
  amelia_imp_bal_mar_30 = amelia_imp_bal_mar_30,
  amelia_imp_bal_mar_10 = amelia_imp_bal_mar_10,
  amelia_imp_bal_mnar_50 = amelia_imp_bal_mnar_50,
  amelia_imp_bal_mnar_30 = amelia_imp_bal_mnar_30,
  amelia_imp_bal_mnar_10 = amelia_imp_bal_mnar_10,
  lstm_bal_mcar_50 = lstm_bal_mcar_50,
  lstm_bal_mcar_30 = lstm_bal_mcar_30,
  lstm_bal_mcar_10 = lstm_bal_mcar_10,
  lstm_bal_mar_50 = lstm_bal_mar_50,
  lstm_bal_mar_30 = lstm_bal_mar_30,
  lstm_bal_mar_10 = lstm_bal_mar_10,
  lstm_bal_mnar_50 = lstm_bal_mnar_50,
  lstm_bal_mnar_30 = lstm_bal_mnar_30,
  lstm_bal_mnar_10 = lstm_bal_mnar_10
)

MargedDataframe_Bal[["RealBalancedPanel"]] <- balanced_panel_data # Add the RealBalancedPanel to the MargedDataframe_Bal

CorrDF_Bal <- data.frame() # Empty dataframe to store the results

# Calculate correlations
for (Names in names(MargedDataframe_Bal)) {
  dataset <- MargedDataframe_Bal[[Names]]
  
  dataset <- dataset[, !colnames(dataset) %in% c("ID", "Year")] # remove ID and Year
  
  # convert categorical columns by implementing dummy encoding
  dataset <- data.frame(lapply(dataset, function(x) {
    if (is.factor(x) || is.character(x)) {
      as.numeric(as.factor(x))
    } else {
      x  # Keep Age, Income unchanged
    }
  }))
  
  # Calculate correlations without Income
  correlations <- cor(dataset, use = "pairwise.complete.obs")[, "Income"]
  correlations <- correlations[!names(correlations) %in% "Income"] # Exclude Income vs Income
  CorrDF_Bal <- rbind(CorrDF_Bal, correlations)
}

rownames(CorrDF_Bal) <- names(MargedDataframe_Bal) # Add row names
colnames(CorrDF_Bal) <- names(correlations) # Add column names
CorrDF_Bal
View(CorrDF_Bal)

### Unbalanced Panel ###

# MargedDataframe
MargedDataframe_Unbal <- list(
  mice_imp_unbal_mcar_50 = mice_imp_unbal_mcar_50,
  mice_imp_unbal_mcar_30 = mice_imp_unbal_mcar_30,
  mice_imp_unbal_mcar_10 = mice_imp_unbal_mcar_10,
  mice_imp_unbal_mar_50 = mice_imp_unbal_mar_50,
  mice_imp_unbal_mar_30 = mice_imp_unbal_mar_30,
  mice_imp_unbal_mar_10 = mice_imp_unbal_mar_10,
  mice_imp_unbal_mnar_50 = mice_imp_unbal_mnar_50,
  mice_imp_unbal_mnar_30 = mice_imp_unbal_mnar_30,
  mice_imp_unbal_mnar_10 = mice_imp_bal_mnar_10,
  mitml_imp_unbal_mcar_50 = mitml_imp_unbal_mcar_50,
  mitml_imp_unbal_mcar_30 = mitml_imp_unbal_mcar_30,
  mitml_imp_unbal_mcar_10 = mitml_imp_unbal_mcar_10,
  mitml_imp_unbal_mar_50 = mitml_imp_unbal_mar_50,
  mitml_imp_unbal_mar_30 = mitml_imp_unbal_mar_30,
  mitml_imp_unbal_mar_10 = mitml_imp_unbal_mar_10,
  mitml_imp_unbal_mnar_50 = mitml_imp_unbal_mnar_50,
  mitml_imp_unbal_mnar_30 = mitml_imp_unbal_mnar_30,
  mitml_imp_unbal_mnar_10 = mitml_imp_unbal_mnar_10,
  amelia_imp_unbal_mcar_50 = amelia_imp_unbal_mcar_50,
  amelia_imp_unbal_mcar_30 = amelia_imp_unbal_mcar_30,
  amelia_imp_unbal_mcar_10 = amelia_imp_unbal_mcar_10,
  amelia_imp_unbal_mar_50 = amelia_imp_unbal_mar_50,
  amelia_imp_unbal_mar_30 = amelia_imp_unbal_mar_30,
  amelia_imp_unbal_mar_10 = amelia_imp_unbal_mar_10,
  amelia_imp_unbal_mnar_50 = amelia_imp_unbal_mnar_50,
  amelia_imp_unbal_mnar_30 = amelia_imp_unbal_mnar_30,
  amelia_imp_unbal_mnar_10 = amelia_imp_unbal_mnar_10,
  lstm_unbal_mcar_50 = lstm_unbal_mcar_50,
  lstm_unbal_mcar_30 = lstm_unbal_mcar_30,
  lstm_unbal_mcar_10 = lstm_unbal_mcar_10,
  lstm_unbal_mar_50 = lstm_unbal_mar_50,
  lstm_unbal_mar_30 = lstm_unbal_mar_30,
  lstm_unbal_mar_10 = lstm_unbal_mar_10,
  lstm_unbal_mnar_50 = lstm_unbal_mnar_50,
  lstm_unbal_mnar_30 = lstm_unbal_mnar_30,
  lstm_unbal_mnar_10 = lstm_unbal_mnar_10
)

MargedDataframe_Unbal[["RealUnbalancedPanel"]] <- balanced_panel_data # Add the RealBalancedPanel to the MargedDataframe_Unbal

CorrDF_Unbal <- data.frame() # Empty dataframe to store the results

# Calculate correlations
for (Names in names(MargedDataframe_Unbal)) {
  dataset <- MargedDataframe_Unbal[[Names]]
  
  dataset <- dataset[, !colnames(dataset) %in% c("ID", "Year")] # remove ID and Year
  
  # convert categorical columns by implementing dummy encoding
  dataset <- data.frame(lapply(dataset, function(x) {
    if (is.factor(x) || is.character(x)) {
      as.numeric(as.factor(x))
    } else {
      x  # Keep Age, Income unchanged
    }
  }))
  
  # Calculate correlations without Income
  correlations <- cor(dataset, use = "pairwise.complete.obs")[, "Income"]
  correlations <- correlations[!names(correlations) %in% "Income"] # Exclude Income vs Income
  CorrDF_Unbal <- rbind(CorrDF_Unbal, correlations)
}

rownames(CorrDF_Unbal) <- names(MargedDataframe_Unbal) # Add row names
colnames(CorrDF_Unbal) <- names(correlations) # Add column names
CorrDF_Unbal
View(CorrDF_Unbal)


###########################
### Coefficients Comparison
###########################

# Select the appropriate model
ModelSelection <- function(data) {
  
  # Breusch-Pagan test
  BPTest <- plmtest(plm(Income ~ Age + EmploymentTypes + MaritalStatus + Education + Sex, data = data, model = "pooling"), type = "bp")
  
  if (BPTest$p.value > 0.05) {
    cat("P-Value:", BPTest$p.value, "\n")
    cat("Model: Pooled OLS")
  } else {
    # Hausman test to check the type of panel effect
    RandomEffectModel <- plm(Income ~ Age + EmploymentTypes + MaritalStatus + Education + Sex, data = data, model = "random")
    FixedEffectModel <- plm(Income ~ Age + EmploymentTypes + MaritalStatus + Education + Sex, data = data, model = "within") # Check this one

    HausmanTest <- phtest(FixedEffectModel, RandomEffectModel)
    
    if (HausmanTest$p.value <= 0.05) {
      cat("P-Value:", HausmanTest$p.value, "\n")
      cat("Model: Fixed Effect")
    } else {
      cat("P-Value:", HausmanTest$p.value, "\n")
      cat("Model: Random Effect")
    }
  }
}

ModelSelection(balanced_panel_data)
ModelSelection(unbalanced_panel_data)

# Extract the coefficients
CoefficientsExtraction <- function(data) {
  
  # Fitting the model and extract coefficients
  model <- plm(Income ~ Age + EmploymentTypes + MaritalStatus + EmploymentHours + Education + Sex, data = data, model = "within")
  Coefficients <- coef(model)
  
  # Convert coefficients to a data frame
  Coefficients <- data.frame(
    Variable = names(Coefficients),
    Coefficient = as.numeric(Coefficients),
    row.names = NULL
  )
  
  return(Coefficients)
}

balanced_panel_data_coef <- CoefficientsExtraction(balanced_panel_data)
unbalanced_panel_data_coef <- CoefficientsExtraction(unbalanced_panel_data)

### mice ###
mice_Coff <- function(mice_imp) {
  
  CoList <- list() # Empty list to store the coefficients from individual datasets
  
  for (i in 1:length(mice_imp)) {
    pdata <- mice_imp[[i]]
    
    # Fitting the model and extracting coefficients
    FixedEffectModel <- plm(Income ~ Age + EmploymentTypes + MaritalStatus + EmploymentHours + Education + Sex, data = pdata, model = "within")
    Coefficients <- coef(FixedEffectModel)

    CoList[[i]] <- Coefficients
  }
  
  # Combine coefficients into a single data frame
  CoefficientsDF <- do.call(rbind, lapply(CoList, function(x) {
    data.frame(
      Variable = names(x),
      Coefficient = as.numeric(x),
      stringsAsFactors = FALSE
    )
  }))
  
  # Rubin's Rule: Calculate pooled coefficients
  PooledCoefficient <- aggregate(CoefficientsDF$Coefficient, by = list(CoefficientsDF$Variable), FUN = mean)
  colnames(PooledCoefficient) <- c("Variable", "Coefficients")
  
  return(PooledCoefficient)
}

# Apply analysis
Coeff_mice_bal_mcar_50 <- mice_Coff(mice_bal_mcar_50)
Coeff_mice_bal_mcar_30 <- mice_Coff(mice_bal_mcar_30)
Coeff_mice_bal_mcar_10 <- mice_Coff(mice_bal_mcar_10)

Coeff_mice_bal_mar_50 <- mice_Coff(mice_bal_mar_50)
Coeff_mice_bal_mar_30 <- mice_Coff(mice_bal_mar_30)
Coeff_mice_bal_mar_10 <- mice_Coff(mice_bal_mar_10)

Coeff_mice_bal_mnar_50 <- mice_Coff(mice_bal_mnar_50)
Coeff_mice_bal_mnar_30 <- mice_Coff(mice_bal_mnar_30)
Coeff_mice_bal_mnar_10 <- mice_Coff(mice_bal_mnar_10)

Coeff_mice_unbal_mcar_50 <- mice_Coff(mice_unbal_mcar_50)
Coeff_mice_unbal_mcar_30 <- mice_Coff(mice_unbal_mcar_30)
Coeff_mice_unbal_mcar_10 <- mice_Coff(mice_unbal_mcar_10)

Coeff_mice_unbal_mar_50 <- mice_Coff(mice_unbal_mar_50)
Coeff_mice_unbal_mar_30 <- mice_Coff(mice_unbal_mar_30)
Coeff_mice_unbal_mar_10 <- mice_Coff(mice_unbal_mar_10)

Coeff_mice_unbal_mnar_50 <- mice_Coff(mice_unbal_mnar_50)
Coeff_mice_unbal_mnar_30 <- mice_Coff(mice_unbal_mnar_30)
Coeff_mice_unbal_mnar_10 <- mice_Coff(mice_unbal_mnar_10)

### mitml ###
mitml_Coff <- function(mitml_imp) {
  
  CoList <- list() # Empty list to store the coefficients from individual datasets
  
  for (i in 1:length(mitml_imp)) {
    pdata <- mitml_imp[[i]]
    
    # Fit the model and extract coefficients
    FixedEffectModel <- plm(Income ~ Age + EmploymentTypes + MaritalStatus + EmploymentHours + Education + Sex, data = pdata, model = "within")
    Coefficients <- coef(FixedEffectModel)
    
    # Store coefficients directly without averaging
    CoList[[i]] <- Coefficients
  }
  
  # Combine coefficients into a single data frame
  CoefficientsDF <- do.call(rbind, lapply(CoList, function(x) {
    data.frame(
      Variable = names(x),
      Coefficient = as.numeric(x),
      stringsAsFactors = FALSE
    )
  }))
  
  # Rubin's Rule: Calculate pooled coefficients
  PooledCoefficient <- aggregate(CoefficientsDF$Coefficient, by = list(CoefficientsDF$Variable), FUN = mean)
  colnames(PooledCoefficient) <- c("Variable", "Coefficients")
  
  return(PooledCoefficient)
}

# Apply the function to each dataset and store results
Coeff_mitml_bal_mcar_50 <- mitml_Coff(mitml_bal_mcar_50)
Coeff_mitml_bal_mcar_30 <- mitml_Coff(mitml_bal_mcar_30)
Coeff_mitml_bal_mcar_10 <- mitml_Coff(mitml_bal_mcar_10)

Coeff_mitml_bal_mar_50 <- mitml_Coff(mitml_bal_mar_50)
Coeff_mitml_bal_mar_30 <- mitml_Coff(mitml_bal_mar_30)
Coeff_mitml_bal_mar_10 <- mitml_Coff(mitml_bal_mar_10)

Coeff_mitml_bal_mnar_50 <- mitml_Coff(mitml_bal_mnar_50)
Coeff_mitml_bal_mnar_30 <- mitml_Coff(mitml_bal_mnar_30)
Coeff_mitml_bal_mnar_10 <- mitml_Coff(mitml_bal_mnar_10)

Coeff_mitml_unbal_mcar_50 <- mitml_Coff(mitml_unbal_mcar_50)
Coeff_mitml_unbal_mcar_30 <- mitml_Coff(mitml_unbal_mcar_30)
Coeff_mitml_unbal_mcar_10 <- mitml_Coff(mitml_unbal_mcar_10)

Coeff_mitml_unbal_mar_50 <- mitml_Coff(mitml_unbal_mar_50)
Coeff_mitml_unbal_mar_30 <- mitml_Coff(mitml_unbal_mar_30)
Coeff_mitml_unbal_mar_10 <- mitml_Coff(mitml_unbal_mar_10)

Coeff_mitml_unbal_mnar_50 <- mitml_Coff(mitml_unbal_mnar_50)
Coeff_mitml_unbal_mnar_30 <- mitml_Coff(mitml_unbal_mnar_30)
Coeff_mitml_unbal_mnar_10 <- mitml_Coff(mitml_unbal_mnar_10)

### amelia ###
amelia_Coff <- function(amelia_imp) {
  
  CoList <- list() # Empty list to store the coefficients from individual datasets
  
  for (i in 1:length(amelia_imp)) {
    pdata <- amelia_imp[[i]]
    
    # Fit the model and extract coefficients
    FixedEffectModel <- plm(Income ~ Age + EmploymentTypes + MaritalStatus + EmploymentHours + Education + Sex, data = pdata, model = "within")
    Coefficients <- coef(FixedEffectModel)
    
    # Store coefficients directly without averaging
    CoList[[i]] <- Coefficients
  }
  
  # Combine coefficients into a single data frame
  CoefficientsDF <- do.call(rbind, lapply(CoList, function(x) {
    data.frame(
      Variable = names(x),
      Coefficient = as.numeric(x),
      stringsAsFactors = FALSE
    )
  }))
  
  # Rubin's Rule: Calculate pooled coefficients
  PooledCoefficient <- aggregate(CoefficientsDF$Coefficient, by = list(CoefficientsDF$Variable), FUN = mean)
  colnames(PooledCoefficient) <- c("Variable", "PooledCoefficient")
  
  return(PooledCoefficient)
}

# Apply the function to each dataset and store results
Coeff_amelia_bal_mcar_50 <- amelia_Coff(amelia_bal_mcar_50)
Coeff_amelia_bal_mcar_30 <- amelia_Coff(amelia_bal_mcar_30)
Coeff_amelia_bal_mcar_10 <- amelia_Coff(amelia_bal_mcar_10)

Coeff_amelia_bal_mar_50 <- amelia_Coff(amelia_bal_mar_50)
Coeff_amelia_bal_mar_30 <- amelia_Coff(amelia_bal_mar_30)
Coeff_amelia_bal_mar_10 <- amelia_Coff(amelia_bal_mar_10)

Coeff_amelia_bal_mnar_50 <- amelia_Coff(amelia_bal_mnar_50)
Coeff_amelia_bal_mnar_30 <- amelia_Coff(amelia_bal_mnar_30)
Coeff_amelia_bal_mnar_10 <- amelia_Coff(amelia_bal_mnar_10)

Coeff_amelia_unbal_mcar_50 <- amelia_Coff(amelia_unbal_mcar_50)
Coeff_amelia_unbal_mcar_30 <- amelia_Coff(amelia_unbal_mcar_30)
Coeff_amelia_unbal_mcar_10 <- amelia_Coff(amelia_unbal_mcar_10)

Coeff_amelia_unbal_mar_50 <- amelia_Coff(amelia_unbal_mar_50)
Coeff_amelia_unbal_mar_30 <- amelia_Coff(amelia_unbal_mar_30)
Coeff_amelia_unbal_mar_10 <- amelia_Coff(amelia_unbal_mar_10)

Coeff_amelia_unbal_mnar_50 <- amelia_Coff(amelia_unbal_mnar_50)
Coeff_amelia_unbal_mnar_30 <- amelia_Coff(amelia_unbal_mnar_30)
Coeff_amelia_unbal_mnar_10 <- amelia_Coff(amelia_unbal_mnar_10)

### LSTM ###

LSTM_Coff <- function(data) {
  
  # Fit the fixed effects model
  FixedEffectModel <- plm(Income ~  Age + EmploymentTypes + MaritalStatus + EmploymentHours + Education + Sex, data = data, model = "within")
  
  # Extract coefficients directly
  Coefficients <- coef(FixedEffectModel)
  
  # Convert coefficients to a data frame
  CoefficientsDF <- data.frame(
    Variable = names(Coefficients),
    Coefficient = as.numeric(Coefficients),
    stringsAsFactors = FALSE
  )
  
  return(CoefficientsDF)
}

# Apply the function to each dataset and store results
Coeff_lstm_bal_mcar_50 <- LSTM_Coff(lstm_bal_mcar_50)
Coeff_lstm_bal_mcar_30 <- LSTM_Coff(lstm_bal_mcar_30)
Coeff_lstm_bal_mcar_10 <- LSTM_Coff(lstm_bal_mcar_10)

Coeff_lstm_bal_mar_50 <- LSTM_Coff(lstm_bal_mar_50)
Coeff_lstm_bal_mar_30 <- LSTM_Coff(lstm_bal_mar_30)
Coeff_lstm_bal_mar_10 <- LSTM_Coff(lstm_bal_mar_10)

Coeff_lstm_bal_mnar_50 <- LSTM_Coff(lstm_bal_mnar_50)
Coeff_lstm_bal_mnar_30 <- LSTM_Coff(lstm_bal_mnar_30)
Coeff_lstm_bal_mnar_10 <- LSTM_Coff(lstm_bal_mnar_10)

Coeff_lstm_unbal_mcar_50 <- LSTM_Coff(lstm_unbal_mcar_50)
Coeff_lstm_unbal_mcar_30 <- LSTM_Coff(lstm_unbal_mcar_30)
Coeff_lstm_unbal_mcar_10 <- LSTM_Coff(lstm_unbal_mcar_10)

Coeff_lstm_unbal_mar_50 <- LSTM_Coff(lstm_unbal_mar_50)
Coeff_lstm_unbal_mar_30 <- LSTM_Coff(lstm_unbal_mar_30)
Coeff_lstm_unbal_mar_10 <- LSTM_Coff(lstm_unbal_mar_10)

Coeff_lstm_unbal_mnar_50 <- LSTM_Coff(lstm_unbal_mnar_50)
Coeff_lstm_unbal_mnar_30 <- LSTM_Coff(lstm_unbal_mnar_30)
Coeff_lstm_unbal_mnar_10 <- LSTM_Coff(lstm_unbal_mnar_10)

## Coefficients list

'Coeff_mice_bal_mcar_50
Coeff_mice_bal_mcar_30
Coeff_mice_bal_mcar_10

Coeff_mice_bal_mar_50 
Coeff_mice_bal_mar_30
Coeff_mice_bal_mar_10

Coeff_mice_bal_mnar_50
Coeff_mice_bal_mnar_30
Coeff_mice_bal_mnar_10

Coeff_mice_unbal_mcar_50
Coeff_mice_unbal_mcar_30
Coeff_mice_unbal_mcar_10

Coeff_mice_unbal_mar_50
Coeff_mice_unbal_mar_30
Coeff_mice_unbal_mar_10

Coeff_mice_unbal_mnar_50
Coeff_mice_unbal_mnar_30
Coeff_mice_unbal_mnar_10

Coeff_mitml_bal_mcar_50
Coeff_mitml_bal_mcar_30
Coeff_mitml_bal_mcar_10

Coeff_mitml_bal_mar_50
Coeff_mitml_bal_mar_30
Coeff_mitml_bal_mar_10

Coeff_mitml_bal_mnar_50
Coeff_mitml_bal_mnar_30
Coeff_mitml_bal_mnar_10

Coeff_mitml_unbal_mcar_50
Coeff_mitml_unbal_mcar_30
Coeff_mitml_unbal_mcar_10

Coeff_mitml_unbal_mar_50
Coeff_mitml_unbal_mar_30
Coeff_mitml_unbal_mar_10

Coeff_mitml_unbal_mnar_50
Coeff_mitml_unbal_mnar_30
Coeff_mitml_unbal_mnar_10

Coeff_amelia_bal_mcar_50
Coeff_amelia_bal_mcar_30
Coeff_amelia_bal_mcar_10

Coeff_amelia_bal_mar_50
Coeff_amelia_bal_mar_30
Coeff_amelia_bal_mar_10

Coeff_amelia_bal_mnar_50
Coeff_amelia_bal_mnar_30
Coeff_amelia_bal_mnar_10

Coeff_amelia_unbal_mcar_50
Coeff_amelia_unbal_mcar_30
Coeff_amelia_unbal_mcar_10

Coeff_amelia_unbal_mar_50
Coeff_amelia_unbal_mar_30
Coeff_amelia_unbal_mar_10

Coeff_amelia_unbal_mnar_50
Coeff_amelia_unbal_mnar_30
Coeff_amelia_unbal_mnar_10

Coeff_lstm_bal_mcar_50
Coeff_lstm_bal_mcar_30
Coeff_lstm_bal_mcar_10

Coeff_lstm_bal_mar_50
Coeff_lstm_bal_mar_30
Coeff_lstm_bal_mar_10

Coeff_lstm_bal_mnar_50
Coeff_lstm_bal_mnar_30
Coeff_lstm_bal_mnar_10

Coeff_lstm_unbal_mcar_50
Coeff_lstm_unbal_mcar_30
Coeff_lstm_unbal_mcar_10

Coeff_lstm_unbal_mar_50
Coeff_lstm_unbal_mar_30
Coeff_lstm_unbal_mar_10

Coeff_lstm_unbal_mnar_50
Coeff_lstm_unbal_mnar_30
Coeff_lstm_unbal_mnar_10'

####################
# Bias and RMSE #
####################

### Balanced Panel ###

# Generate dataframe and columns
NewColNames_Bal <- function(dataset) {
  colnames(dataset) <- c("Variable", "Coefficients")
  return(dataset)
}

AllCoeffDatasets_Bal <- list(
  Coeff_mice_bal_mcar_50, Coeff_mice_bal_mcar_30, Coeff_mice_bal_mcar_10,
  Coeff_mice_bal_mar_50, Coeff_mice_bal_mar_30, Coeff_mice_bal_mar_10,
  Coeff_mice_bal_mnar_50, Coeff_mice_bal_mnar_30, Coeff_mice_bal_mnar_10,
  Coeff_mitml_bal_mcar_50, Coeff_mitml_bal_mcar_30, Coeff_mitml_bal_mcar_10,
  Coeff_mitml_bal_mar_50, Coeff_mitml_bal_mar_30, Coeff_mitml_bal_mar_10,
  Coeff_mitml_bal_mnar_50, Coeff_mitml_bal_mnar_30, Coeff_mitml_bal_mnar_10,
  Coeff_amelia_bal_mcar_50, Coeff_amelia_bal_mcar_30, Coeff_amelia_bal_mcar_10,
  Coeff_amelia_bal_mar_50, Coeff_amelia_bal_mar_30, Coeff_amelia_bal_mar_10,
  Coeff_amelia_bal_mnar_50, Coeff_amelia_bal_mnar_30, Coeff_amelia_bal_mnar_10,
  Coeff_lstm_bal_mcar_50, Coeff_lstm_bal_mcar_30, Coeff_lstm_bal_mcar_10,
  Coeff_lstm_bal_mar_50, Coeff_lstm_bal_mar_30, Coeff_lstm_bal_mar_10,
  Coeff_lstm_bal_mnar_50, Coeff_lstm_bal_mnar_30, Coeff_lstm_bal_mnar_10
)

Datasets_Bal <- lapply(AllCoeffDatasets_Bal, NewColNames_Bal)

# Add Identifiers
DatasetIdentifiers_Bal <- function(dataset, dataset_name) {
  dataset$Dataset <- dataset_name
  return(dataset)
}

# Coefficients lists
DatasetNames_Bal <- c(
  "mice_mcar_bal_50", "mice_mcar_bal_30", "mice_mcar_bal_10",
  "mice_mar_bal_50", "mice_mar_bal_30", "mice_mar_bal_10",
  "mice_mnar_bal_50", "mice_mnar_bal_30", "mice_mnar_bal_10",
  "mitml_mcar_bal_50", "mitml_mcar_bal_30", "mitml_mcar_bal_10",
  "mitml_mar_bal_50", "mitml_mar_bal_30", "mitml_mar_bal_10",
  "mitml_mnar_bal_50", "mitml_mnar_bal_30", "mitml_mnar_bal_10",
  "amelia_mcar_bal_50", "amelia_mcar_bal_30", "amelia_mcar_bal_10",
  "amelia_mar_bal_50", "amelia_mar_bal_30", "amelia_mar_bal_10",
  "amelia_mnar_bal_50", "amelia_mnar_bal_30", "amelia_mnar_bal_10",
  "lstm_mcar_bal_50", "lstm_mcar_bal_30", "lstm_mcar_bal_10",
  "lstm_mar_bal_50", "lstm_mar_bal_30", "lstm_mar_bal_10",
  "lstm_mnar_bal_50", "lstm_mnar_bal_30", "lstm_mnar_bal_10"
)

# Generate combined dataframe of coefficients
LabeledDatasets_Bal <- mapply(DatasetIdentifiers_Bal, Datasets_Bal, DatasetNames_Bal, SIMPLIFY = FALSE)
CombinedDF_Bal <- do.call(rbind, LabeledDatasets_Bal)
CombinedDF_Bal <- CombinedDF_Bal[order(match(CombinedDF_Bal$Variable, balanced_panel_data_coef$Variable)), ]

# Convert to wide format
CoeffDF_Bal <- pivot_wider(
  CombinedDF_Bal,
  names_from = Dataset,
  values_from = Coefficients
)
head(CoeffDF_Bal)

### Bias ###

# Marged Coefficients from imputed datasets with the original datasets coefficients
MergedDF_Bal <- merge(CoeffDF_Bal, balanced_panel_data_coef, by = "Variable")

# Bias Calculation
Bias_Bal <- MergedDF_Bal
for (col in names(CoeffDF_Bal)[-1]) {  # Exclude "Variable" column
  Bias_Bal[[col]] <- MergedDF_Bal[[col]] - MergedDF_Bal$Coefficient
}

Bias_Bal <- Bias_Bal[, !(names(Bias_Bal) %in% "Coefficient")] # Remove the column of the true coefficients
print(Bias_Bal)

# Mean bias calculation
BiasMean_Bal <- colMeans(Bias_Bal[, -1])  # Exclude 'Variable' column
BiasMeanDF_Bal <- data.frame(
  Dataset = names(BiasMean_Bal),
  MeanBias = BiasMean_Bal
)

# Ranking from the minimum to maximum values
BiasMeanDF_Bal <- BiasMeanDF_Bal[order(BiasMeanDF_Bal$MeanBias), ]
BiasMeanDF_Bal$Rank <- seq_len(nrow(BiasMeanDF_Bal))
print(BiasMeanDF_Bal)

# Generate new columns of the algorithm names
BiasMeanDF_Bal <- BiasMeanDF_Bal %>%
  mutate(Algorithm = sub("_.*", "", Dataset))

# Calculation of the average bias for each algorithm for all the datasets
AverageBias_Bal <- BiasMeanDF_Bal %>%
  group_by(Algorithm) %>%
  summarise(MeanBias = mean(MeanBias, na.rm = TRUE)) %>%
  arrange(MeanBias)
print(AverageBias_Bal)

#### RMSE ####

RMSE_Bal <- apply(Bias_Bal[, -1], 2, function(x) sqrt(mean(x^2)))  # Exclude 'Variable' column
RMSEDF_Bal <- data.frame(
  Dataset = names(RMSE_Bal),
  RMSE_Bal = RMSE_Bal
)

# Rank the datasets from minimum to maximum
RMSEDF_Bal <- RMSEDF_Bal[order(RMSEDF_Bal$RMSE_Bal), ]
RMSEDF_Bal$Rank <- seq_len(nrow(RMSEDF_Bal))
print(RMSEDF_Bal)

# Generate new columns of the algorithm names
RMSEDF_Bal <- RMSEDF_Bal %>%
  mutate(Algorithm = sub("_.*", "", Dataset))

# Calculation of the average RMSE for each algorithm for all the datasets
AverageRMSE_Bal <- RMSEDF_Bal %>%
  group_by(Algorithm) %>%
  summarise(MeanRMSE = mean(RMSE_Bal, na.rm = TRUE)) %>%
  arrange(MeanRMSE)
print(AverageRMSE_Bal)

### Unbalanced Panel ###

# Generate dataframe and columns
NewColNames_Unbal <- function(dataset) {
  colnames(dataset) <- c("Variable", "Coefficients")
  return(dataset)
}

AllCoeffDatasets_Unbal <- list(
  Coeff_mice_unbal_mcar_50, Coeff_mice_unbal_mcar_30, Coeff_mice_unbal_mcar_10,
  Coeff_mice_unbal_mar_50, Coeff_mice_unbal_mar_30, Coeff_mice_unbal_mar_10,
  Coeff_mice_unbal_mnar_50, Coeff_mice_unbal_mnar_30, Coeff_mice_unbal_mnar_10,
  Coeff_mitml_unbal_mcar_50, Coeff_mitml_unbal_mcar_30, Coeff_mitml_unbal_mcar_10,
  Coeff_mitml_unbal_mar_50, Coeff_mitml_unbal_mar_30, Coeff_mitml_unbal_mar_10,
  Coeff_mitml_unbal_mnar_50, Coeff_mitml_unbal_mnar_30, Coeff_mitml_unbal_mnar_10,
  Coeff_amelia_unbal_mcar_50, Coeff_amelia_unbal_mcar_30, Coeff_amelia_unbal_mcar_10,
  Coeff_amelia_unbal_mar_50, Coeff_amelia_unbal_mar_30, Coeff_amelia_unbal_mar_10,
  Coeff_amelia_unbal_mnar_50, Coeff_amelia_unbal_mnar_30, Coeff_amelia_unbal_mnar_10,
  Coeff_lstm_unbal_mcar_50, Coeff_lstm_unbal_mcar_30, Coeff_lstm_unbal_mcar_10,
  Coeff_lstm_unbal_mar_50, Coeff_lstm_unbal_mar_30, Coeff_lstm_unbal_mar_10,
  Coeff_lstm_unbal_mnar_50, Coeff_lstm_unbal_mnar_30, Coeff_lstm_unbal_mnar_10
)

Datasets_Unbal <- lapply(AllCoeffDatasets_Unbal, NewColNames_Unbal)

# Add Identifiers
DatasetIdentifiers_Unbal <- function(dataset, dataset_name) {
  dataset$Dataset <- dataset_name
  return(dataset)
}

# Coefficients lists
DatasetNames_Unbal <- c(
  "mice_mcar_unbal_50", "mice_mcar_unbal_30", "mice_mcar_unbal_10",
  "mice_mar_unbal_50", "mice_mar_unbal_30", "mice_mar_unbal_10",
  "mice_mnar_unbal_50", "mice_mnar_unbal_30", "mice_mnar_unbal_10",
  "mitml_mcar_unbal_50", "mitml_mcar_unbal_30", "mitml_mcar_unbal_10",
  "mitml_mar_unbal_50", "mitml_mar_unbal_30", "mitml_mar_unbal_10",
  "mitml_mnar_unbal_50", "mitml_mnar_unbal_30", "mitml_mnar_unbal_10",
  "amelia_mcar_unbal_50", "amelia_mcar_unbal_30", "amelia_mcar_unbal_10",
  "amelia_mar_unbal_50", "amelia_mar_unbal_30", "amelia_mar_unbal_10",
  "amelia_mnar_unbal_50", "amelia_mnar_unbal_30", "amelia_mnar_unbal_10",
  "lstm_mcar_unbal_50", "lstm_mcar_unbal_30", "lstm_mcar_unbal_10",
  "lstm_mar_unbal_50", "lstm_mar_unbal_30", "lstm_mar_unbal_10",
  "lstm_mnar_unbal_50", "lstm_mnar_unbal_30", "lstm_mnar_unbal_10"
)

# Generate combined dataframe of coefficients
LabeledDatasets_Unbal <- mapply(DatasetIdentifiers_Unbal, Datasets_Unbal, DatasetNames_Unbal, SIMPLIFY = FALSE)
CombinedDF_Unbal <- do.call(rbind, LabeledDatasets_Unbal)
CombinedDF_Unbal <- CombinedDF_Unbal[order(match(CombinedDF_Unbal$Variable, unbalanced_panel_data_coef$Variable)), ]

# Convert to wide format
CoeffDF_Unbal <- pivot_wider(
  CombinedDF_Unbal,
  names_from = Dataset,
  values_from = Coefficients
)
head(CoeffDF_Unbal)

### Bias ###

# Marged Coefficients from imputed datasets with the original datasets coefficients
MergedDF_Unbal <- merge(CoeffDF_Unbal, unbalanced_panel_data_coef, by = "Variable")

# Bias Calculation
Bias_Unbal <- MergedDF_Unbal
for (col in names(CoeffDF_Unbal)[-1]) {  # Exclude "Variable" column
  Bias_Unbal[[col]] <- MergedDF_Unbal[[col]] - MergedDF_Unbal$Coefficient
}

Bias_Unbal <- Bias_Unbal[, !(names(Bias_Unbal) %in% "Coefficient")] # Remove the column of the true coefficients
print(Bias_Unbal)

# Mean bias calculation
BiasMean_Unbal <- colMeans(Bias_Unbal[, -1])  # Exclude 'Variable' column
BiasMeanDF_Unbal <- data.frame(
  Dataset = names(BiasMean_Unbal),
  MeanBias = BiasMean_Unbal
)

# Ranking from the minimum to maximum values
BiasMeanDF_Unbal <- BiasMeanDF_Unbal[order(BiasMeanDF_Unbal$MeanBias), ]
BiasMeanDF_Unbal$Rank <- seq_len(nrow(BiasMeanDF_Unbal))
print(BiasMeanDF_Unbal)

# Generate new columns of the algorithm names
BiasMeanDF_Unbal <- BiasMeanDF_Unbal %>%
  mutate(Algorithm = sub("_.*", "", Dataset))

# Calculation of the average bias for each algorithm for all the datasets
AverageBias_Unbal <- BiasMeanDF_Unbal %>%
  group_by(Algorithm) %>%
  summarise(MeanBias = mean(MeanBias, na.rm = TRUE)) %>%
  arrange(MeanBias)
print(AverageBias_Unbal)

#### RMSE ####
RMSE_Unbal <- apply(Bias_Unbal[, -1], 2, function(x) sqrt(mean(x^2)))  # Exclude 'Variable' column
RMSEDF_Unbal <- data.frame(
  Dataset = names(RMSE_Unbal),
  RMSE_Unbal = RMSE_Unbal
)

# Rank the datasets from minimum to maximum
RMSEDF_Unbal <- RMSEDF_Unbal[order(RMSEDF_Unbal$RMSE_Unbal), ]
RMSEDF_Unbal$Rank <- seq_len(nrow(RMSEDF_Unbal))
print(RMSEDF_Unbal)

# Generate new columns of the algorithm names
RMSEDF_Unbal <- RMSEDF_Unbal %>%
  mutate(Algorithm = sub("_.*", "", Dataset))

# Calculation of the average RMSE for each algorithm for all the datasets
AverageRMSE_Unbal <- RMSEDF_Unbal %>%
  group_by(Algorithm) %>%
  summarise(MeanRMSE = mean(RMSE_Unbal, na.rm = TRUE)) %>%
  arrange(MeanRMSE)
print(AverageRMSE_Unbal)


