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
data = RawData[c("id", "year","EF310", "ind.median", "inc.ind")]
colnames(data) <- c("ID", "Year", "Education", "MedianIncome", "IndividualIncome")
summary(data)

#unique(data$variable) , to check the unique values

# Count unique values in the 'ID' column
count(data, ID)

# Count unique values in the 'Year' column
count(data, Year)

# Count unique values in the 'Education' column - Highest general school degree
count(data, Education)

# Count unique values in the MedianIncome
count(data, MedianIncome)

# Count unique values in the 'IndividualIncome' column - Income
count(data, IndividualIncome)

########################
## Creating the Data
########################

# Load required library
library(plm)

# Set parameters
n <- 100  # Number of individuals
T <- 5    # Number of time periods

# Generate individual and time indices
id <- rep(1:n, each = T)
time <- rep(1:T, times = n)

# Simulate individual-specific effects
alpha_i <- rnorm(n, mean = 0, sd = 1)

# Simulate time-specific effects
gamma_t <- rnorm(T, mean = 0, sd = 1)

# Generate random error term
error <- rnorm(n*T, mean = 0, sd = 1)

# Simulate balanced panel data
data_balanced <- data.frame(
  id = id,
  time = time,
  x1 = rnorm(n*T),
  x2 = rnorm(n*T),
  y = 1 + 2 * rnorm(n*T) + 3 * rnorm(n*T) + alpha_i[id] + gamma_t[time] + error
)

# Convert to panel data object
balanced_panel_data <- pdata.frame(data_balanced, index = c("id", "time"))

# View the first few rows of the balanced panel data
head(balanced_panel_data)

# Set parameters
n <- 100  # Number of individuals
T <- 5    # Maximum number of time periods

# Generate random number of observations per individual
obs_per_individual <- sample(2:T, n, replace = TRUE)

# Generate individual and time indices
id <- rep(1:n, times = obs_per_individual)
time <- unlist(lapply(obs_per_individual, function(x) sample(1:T, x)))

# Simulate individual-specific effects
alpha_i <- rnorm(n, mean = 0, sd = 1)

# Simulate time-specific effects
gamma_t <- rnorm(T, mean = 0, sd = 1)

# Generate random error term
error <- rnorm(length(id), mean = 0, sd = 1)

# Simulate unbalanced panel data
data_unbalanced <- data.frame(
  id = id,
  time = time,
  x1 = rnorm(length(id)),
  x2 = rnorm(length(id)),
  y = 1 + 2 * rnorm(length(id)) + 3 * rnorm(length(id)) + alpha_i[id] + gamma_t[time] + error
)

# Convert to panel data object
unbalanced_panel_data <- pdata.frame(data_unbalanced, index = c("id", "time"))

# View the first few rows of the unbalanced panel data
head(unbalanced_panel_data)


#### MCAR ####
##############

#### 50% ####

p_mis_50 <- 0.50
num_rows <- nrow(balanced_panel_data)  # Get the number of rows in balanced_panel_data
mis_simulated_mcar_50 <- sample(1:num_rows, p_mis_50 * num_rows, replace = FALSE)    
balanced_panel_data_mcar_50 <- balanced_panel_data
balanced_panel_data_mcar_50[mis_simulated_mcar_50, 5] <- NA
summary(balanced_panel_data_mcar_50)

library(mice)

# choose to impute datasets.
balanced_panel_data_mcar_50_mice_imp <- mice(balanced_panel_data_mcar_50, m = 3, maxit = 1000, method = 'pmm')
balanced_panel_data_mcar_50_pan_imp$imp$counts
balanced_panel_data_mcar_50_mice_imp <- complete(balanced_panel_data_mcar_50_mice_imp,3)
balanced_panel_data_mcar_50_mice_imp
























