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
data <- subset(data, Year >= 2013 & Year <= 2023) # Filter the data to keep only rows where the 'Year' is between 2013 and 2023 inclusive

# 'Education' column - Highest general school degree
count(data, Education)
data$Education[is.na(data$Education)] <- 7

# MedianIncome
count(data, Age)
summary(data$Age)

# 'IndividualIncome' column - Income
count(data, IndividualIncome)
data$IndividualIncome[is.na(data$IndividualIncome)] <- 0
summary(data$IndividualIncome)

sum(is.na(data)) #Total number of NA values in the data frame
summary(data)

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
balanced_panel_data <- data[data$ID %in% common_ids, ]

# Print the final data to check the result
print(balanced_panel_data)

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

########################
## Unalanced Panel
########################
# The initial data is considered as a unbalanced panel data
unbalanced_panel_data <- data

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

########################
## Missingness in Balanced Panel
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

#### 30% ####

p_mis_30 <- 0.30
num_rows <- nrow(balanced_panel_data)  # Get the number of rows in balanced_panel_data
mis_simulated_mcar_30 <- sample(1:num_rows, p_mis_30 * num_rows, replace = FALSE)    
balanced_panel_data_mcar_30 <- balanced_panel_data
balanced_panel_data_mcar_30[mis_simulated_mcar_30, 5] <- NA
summary(balanced_panel_data_mcar_30)

#### 30% ####

p_mis_10 <- 0.10
num_rows <- nrow(balanced_panel_data)  # Get the number of rows in balanced_panel_data
mis_simulated_mcar_10 <- sample(1:num_rows, p_mis_10 * num_rows, replace = FALSE)    
balanced_panel_data_mcar_10 <- balanced_panel_data
balanced_panel_data_mcar_10[mis_simulated_mcar_10, 5] <- NA
summary(balanced_panel_data_mcar_10)

#### MAR ####
#############

#### 50% ####

balanced_panel_data_mar_50 <- balanced_panel_data
set.seed(123)
p_mis_50 <- 0.5  # 50% missingness

# Model missingness via linear regression model
# Depending on Education, Age, and random error
mis_simulated_mar_50 <- 0.5 + 2 * balanced_panel_data_mar_50$Education - 
  0.7 * balanced_panel_data_mar_50$Age + 
  rnorm(nrow(balanced_panel_data_mar_50), 0, 3)

# All below the 50% quantile are set to missing
mis_simulated_mar_50 <- mis_simulated_mar_50 < quantile(mis_simulated_mar_50, p_mis_50)

# Proportion of missing data
mean(as.numeric(mis_simulated_mar_50))

# Set values to NA in IndividualIncome where missingness occurs
balanced_panel_data_mar_50$IndividualIncome[mis_simulated_mar_50] <- NA

# Summary of IndividualIncome after introducing missingness
summary(balanced_panel_data_mar_50)


#### 30% ####

balanced_panel_data_mar_30 <- balanced_panel_data
set.seed(123)
p_mis_50 <- 0.3  # 30% missingness

# Model missingness via linear regression model
# Depending on Education, Age, and random error
mis_simulated_mar_30 <- 0.5 + 2 * balanced_panel_data_mar_30$Education - 
  0.7 * balanced_panel_data_mar_30$Age + 
  rnorm(nrow(balanced_panel_data_mar_30), 0, 3)

# All below the 50% quantile are set to missing
mis_simulated_mar_30 <- mis_simulated_mar_30 < quantile(mis_simulated_mar_30, p_mis_30)

# Proportion of missing data
mean(as.numeric(mis_simulated_mar_30))

# Set values to NA in IndividualIncome where missingness occurs
balanced_panel_data_mar_30$IndividualIncome[mis_simulated_mar_30] <- NA

# Summary of IndividualIncome after introducing missingness
summary(balanced_panel_data_mar_30)


#### 10% ####

balanced_panel_data_mar_10 <- balanced_panel_data
set.seed(123)
p_mis_50 <- 0.1  # 10% missingness

# Model missingness via linear regression model
# Depending on Education, Age, and random error
mis_simulated_mar_10 <- 0.5 + 2 * balanced_panel_data_mar_10$Education - 
  0.7 * balanced_panel_data_mar_10$Age + 
  rnorm(nrow(balanced_panel_data_mar_10), 0, 3)

# All below the 50% quantile are set to missing
mis_simulated_mar_10 <- mis_simulated_mar_10 < quantile(mis_simulated_mar_10, p_mis_10)

# Proportion of missing data
mean(as.numeric(mis_simulated_mar_10))

# Set values to NA in IndividualIncome where missingness occurs
balanced_panel_data_mar_10$IndividualIncome[mis_simulated_mar_10] <- NA

# Summary of IndividualIncome after introducing missingness
summary(balanced_panel_data_mar_10)

#### MNAR ####
##############

#### Probabilistic, Linear Regression model ####
#### 50% ####

p_mis_50 <- .50
balanced_panel_data_mnar_50 <- balanced_panel_data
# model missingness by logistic regression (probit):
# the missing of a value now also depends on IndividualIncome itself
mis_simulated_mnar_50 <-0.5 + 1 * balanced_panel_data_mnar_50$Education - 
  0.7 * balanced_panel_data_mnar_50$Age - 
  5 * balanced_panel_data_mnar_50$IndividualIncome + rnorm(nrow(balanced_panel_data), 0, 3)
mis_simulated_mnar_50 <- mis_simulated_mnar_50 < quantile(mis_simulated_mnar_50, p_mis_50)
balanced_panel_data_mnar_50$IndividualIncome[mis_simulated_mnar_50] <- NA
summary(balanced_panel_data_mnar_50)

#### 30% ####

p_mis_30 <- .30
balanced_panel_data_mnar_30 <- balanced_panel_data
# model missingness by logistic regression (probit):
# the missing of a value now also depends on IndividualIncome itself
mis_simulated_mnar_30 <-0.5 + 1 * balanced_panel_data_mnar_30$Education - 
  0.7 * balanced_panel_data_mnar_30$Age - 
  5 * balanced_panel_data_mnar_30$IndividualIncome + rnorm(nrow(balanced_panel_data), 0, 3)
mis_simulated_mnar_30 <- mis_simulated_mnar_30 < quantile(mis_simulated_mnar_30, p_mis_30)
balanced_panel_data_mnar_30$IndividualIncome[mis_simulated_mnar_30] <- NA
summary(balanced_panel_data_mnar_30)

#### 10% ####

p_mis_10 <- .10
balanced_panel_data_mnar_10 <- balanced_panel_data
# model missingness by logistic regression (probit):
# the missing of a value now also depends on IndividualIncome itself
mis_simulated_mnar_10 <-0.5 + 1 * balanced_panel_data_mnar_10$Education - 
  0.7 * balanced_panel_data_mnar_10$Age - 
  5 * balanced_panel_data_mnar_10$IndividualIncome + rnorm(nrow(balanced_panel_data), 0, 3)
mis_simulated_mnar_10 <- mis_simulated_mnar_10 < quantile(mis_simulated_mnar_10, p_mis_10)
balanced_panel_data_mnar_10$IndividualIncome[mis_simulated_mnar_10] <- NA
summary(balanced_panel_data_mnar_10)

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

#### 30% ####

p_mis_30 <- 0.30
num_rows <- nrow(unbalanced_panel_data)  # Get the number of rows in balanced_panel_data
mis_simulated_mcar_30 <- sample(1:num_rows, p_mis_30 * num_rows, replace = FALSE)    
unbalanced_panel_data_mcar_30 <- unbalanced_panel_data
unbalanced_panel_data_mcar_30[mis_simulated_mcar_30, 5] <- NA
summary(unbalanced_panel_data_mcar_30)

#### 10% ####

p_mis_10 <- 0.10
num_rows <- nrow(unbalanced_panel_data)  # Get the number of rows in balanced_panel_data
mis_simulated_mcar_10 <- sample(1:num_rows, p_mis_10 * num_rows, replace = FALSE)    
unbalanced_panel_data_mcar_10 <- unbalanced_panel_data
unbalanced_panel_data_mcar_10[mis_simulated_mcar_10, 5] <- NA
summary(unbalanced_panel_data_mcar_10)

#### MAR ####
#############

#### 50% ####
unbalanced_panel_data_mar_50 <- unbalanced_panel_data
set.seed(123)
p_mis_50 <- 0.5  # 50% missingness

# Model missingness via linear regression model
# Depending on Education, Age, and random error
mis_simulated_mar_50 <- 0.5 + 2 * unbalanced_panel_data_mar_50$Education - 
  0.7 * unbalanced_panel_data_mar_50$Age + 
  rnorm(nrow(unbalanced_panel_data_mar_50), 0, 3)

# All below the 50% quantile are set to missing
mis_simulated_mar_50 <- mis_simulated_mar_50 < quantile(mis_simulated_mar_50, p_mis_50)

# Proportion of missing data
mean(as.numeric(mis_simulated_mar_50))

# Set values to NA in IndividualIncome where missingness occurs
unbalanced_panel_data_mar_50$IndividualIncome[mis_simulated_mar_50] <- NA

# Summary of IndividualIncome after introducing missingness
summary(unbalanced_panel_data_mar_50)


#### 30% ####
unbalanced_panel_data_mar_30 <- unbalanced_panel_data
set.seed(123)
p_mis_50 <- 0.3  # 30% missingness

# Model missingness via linear regression model
# Depending on Education, Age, and random error
mis_simulated_mar_30 <- 0.5 + 2 * unbalanced_panel_data_mar_30$Education - 
  0.7 * unbalanced_panel_data_mar_30$Age + 
  rnorm(nrow(unbalanced_panel_data_mar_30), 0, 3)

# All below the 50% quantile are set to missing
mis_simulated_mar_30 <- mis_simulated_mar_30 < quantile(mis_simulated_mar_30, p_mis_30)

# Proportion of missing data
mean(as.numeric(mis_simulated_mar_30))

# Set values to NA in IndividualIncome where missingness occurs
unbalanced_panel_data_mar_30$IndividualIncome[mis_simulated_mar_30] <- NA

# Summary of IndividualIncome after introducing missingness
summary(unbalanced_panel_data_mar_30)


#### 10% ####

unbalanced_panel_data_mar_10 <- unbalanced_panel_data
set.seed(123)
p_mis_50 <- 0.1  # 10% missingness

# Model missingness via linear regression model
# Depending on Education, Age, and random error
mis_simulated_mar_10 <- 0.5 + 2 * unbalanced_panel_data_mar_10$Education - 
  0.7 * unbalanced_panel_data_mar_10$Age + 
  rnorm(nrow(unbalanced_panel_data_mar_10), 0, 3)

# All below the 50% quantile are set to missing
mis_simulated_mar_10 <- mis_simulated_mar_10 < quantile(mis_simulated_mar_10, p_mis_10)

# Proportion of missing data
mean(as.numeric(mis_simulated_mar_10))

# Set values to NA in IndividualIncome where missingness occurs
unbalanced_panel_data_mar_10$IndividualIncome[mis_simulated_mar_10] <- NA

# Summary of IndividualIncome after introducing missingness
summary(unbalanced_panel_data_mar_10)

#### MNAR ####
##############

#### 50% ####
#### Probabilistic, Linear Regression model, Real data ####

p_mis_50 <- .50
unbalanced_panel_data_mnar_50 <- unbalanced_panel_data
# model missingness by logistic regression (probit):
# the missing of a value now also depends on IndividualIncome itself
mis_simulated_mnar_50 <-0.5 + 1 * unbalanced_panel_data_mnar_50$Education - 
  0.7 * unbalanced_panel_data_mnar_50$Age - 
  5 * unbalanced_panel_data_mnar_50$IndividualIncome + rnorm(nrow(unbalanced_panel_data), 0, 3)
mis_simulated_mnar_50 <- mis_simulated_mnar_50 < quantile(mis_simulated_mnar_50, p_mis_50)
unbalanced_panel_data_mnar_50$IndividualIncome[mis_simulated_mnar_50] <- NA
summary(unbalanced_panel_data_mnar_50)

#### 30% ####

p_mis_30 <- .30
unbalanced_panel_data_mnar_30 <- unbalanced_panel_data
# model missingness by logistic regression (probit):
# the missing of a value now also depends on IndividualIncome itself
mis_simulated_mnar_30 <-0.5 + 1 * unbalanced_panel_data_mnar_30$Education - 
  0.7 * unbalanced_panel_data_mnar_30$Age - 
  5 * unbalanced_panel_data_mnar_30$IndividualIncome + rnorm(nrow(unbalanced_panel_data), 0, 3)
mis_simulated_mnar_30 <- mis_simulated_mnar_30 < quantile(mis_simulated_mnar_30, p_mis_30)
unbalanced_panel_data_mnar_30$IndividualIncome[mis_simulated_mnar_30] <- NA
summary(unbalanced_panel_data_mnar_30)

#### 10% ####

p_mis_10 <- .10
unbalanced_panel_data_mnar_10 <- unbalanced_panel_data
# model missingness by logistic regression (probit):
# the missing of a value now also depends on IndividualIncome itself
mis_simulated_mnar_10 <-0.5 + 1 * unbalanced_panel_data_mnar_10$Education - 
  0.7 * unbalanced_panel_data_mnar_10$Age - 
  5 * unbalanced_panel_data_mnar_10$IndividualIncome + rnorm(nrow(unbalanced_panel_data), 0, 3)
mis_simulated_mnar_10 <- mis_simulated_mnar_10 < quantile(mis_simulated_mnar_10, p_mis_10)
unbalanced_panel_data_mnar_10$IndividualIncome[mis_simulated_mnar_10] <- NA
summary(unbalanced_panel_data_mnar_10)


###################
### Data Sets
###################

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
## mitml package
######################

library(mitml)

balanced_panel_data_mcar_50 = data.frame(balanced_panel_data_mcar_50)
balanced_panel_data_mcar_50 = balanced_panel_data_mcar_50[c("id", "time", "x1", "x2")]
type <- c(0, -2, 1, 3)
names(type) <- colnames(balanced_panel_data_mcar_50)
balanced_panel_data_mcar_50_pan_imp <- panImpute(balanced_panel_data_mcar_50, type = type, n.burn = 1000, n.iter = 100, m = 3)
balanced_panel_data_mcar_50_pan_imp = mitmlComplete(balanced_panel_data_mcar_50_pan_imp, print = 3)
balanced_panel_data_mcar_50_pan_imp

######################
## mice package
######################

library(mice)

# choose to impute 3 dataset.
balanced_panel_data_mcar_50_mice_imp <- mice(balanced_panel_data_mcar_50, m = 3, maxit = 1000, method = 'pmm')
balanced_panel_data_mcar_50_pan_imp$imp$counts
balanced_panel_data_mcar_50_mice_imp <- complete(balanced_panel_data_mcar_50_mice_imp,3)
balanced_panel_data_mcar_50_mice_imp

############################
## LSTM Network
############################

























