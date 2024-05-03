###################################
## Tipu Sultan
###################################

# Set Directory
# setwd("/Users/tipusultan/Documents/GitHub/Imputation-of-Panel-Data")

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


########################
## Missingness in Balanced Panel
########################

#### MCAR ####
##############

p_mis_50 <- 0.50
num_rows <- nrow(balanced_panel_data)  # Get the number of rows in balanced_panel_data
mis_simulated_mcar_50 <- sample(1:num_rows, p_mis_50 * num_rows, replace = FALSE)    
balanced_panel_data_mcar_50 <- balanced_panel_data
balanced_panel_data_mcar_50[mis_simulated_mcar_50, 3] <- NA
summary(balanced_panel_data_mcar_50)

p_mis_30 <- 0.30
num_rows <- nrow(balanced_panel_data)  # Get the number of rows in balanced_panel_data
mis_simulated_mcar_30 <- sample(1:num_rows, p_mis_30 * num_rows, replace = FALSE)    
balanced_panel_data_mcar_30 <- balanced_panel_data
balanced_panel_data_mcar_30[mis_simulated_mcar_30, 3] <- NA
summary(balanced_panel_data_mcar_30)

p_mis_10 <- 0.10
num_rows <- nrow(balanced_panel_data)  # Get the number of rows in balanced_panel_data
mis_simulated_mcar_10 <- sample(1:num_rows, p_mis_10 * num_rows, replace = FALSE)    
balanced_panel_data_mcar_10 <- balanced_panel_data
balanced_panel_data_mcar_10[mis_simulated_mcar_10, 3] <- NA
summary(balanced_panel_data_mcar_10)

#### MAR ####
#############

#### 50% ####
balanced_panel_data_mar_50 <- balanced_panel_data
set.seed(123)
p_mis_50 <- 0.5  # 50% missingness

# Model missingness via linear regression model
# Depending on x1, x2, and random error
mis_simulated_mar_50 <- 0.5 + 2 * balanced_panel_data_mar_50$x1 - 
  0.7 * balanced_panel_data_mar_50$x2 + 
  rnorm(nrow(balanced_panel_data_mar_50), 0, 3)

# All below the 50% quantile are set to missing
mis_simulated_mar_50 <- mis_simulated_mar_50 < quantile(mis_simulated_mar_50, p_mis_50)

# Proportion of missing data
mean(as.numeric(mis_simulated_mar_50))

# Set values to NA in y where missingness occurs
balanced_panel_data_mar_50$y[mis_simulated_mar_50] <- NA

# Summary of y after introducing missingness
summary(balanced_panel_data_mar_50)


#### 30% ####
balanced_panel_data_mar_30 <- balanced_panel_data
set.seed(123)
p_mis_50 <- 0.3  # 30% missingness

# Model missingness via linear regression model
# Depending on x1, x2, and random error
mis_simulated_mar_30 <- 0.5 + 2 * balanced_panel_data_mar_30$x1 - 
  0.7 * balanced_panel_data_mar_30$x2 + 
  rnorm(nrow(balanced_panel_data_mar_30), 0, 3)

# All below the 50% quantile are set to missing
mis_simulated_mar_30 <- mis_simulated_mar_30 < quantile(mis_simulated_mar_30, p_mis_30)

# Proportion of missing data
mean(as.numeric(mis_simulated_mar_30))

# Set values to NA in y where missingness occurs
balanced_panel_data_mar_30$y[mis_simulated_mar_30] <- NA

# Summary of y after introducing missingness
summary(balanced_panel_data_mar_30)


#### 10% ####

balanced_panel_data_mar_10 <- balanced_panel_data
set.seed(123)
p_mis_50 <- 0.1  # 10% missingness

# Model missingness via linear regression model
# Depending on x1, x2, and random error
mis_simulated_mar_10 <- 0.5 + 2 * balanced_panel_data_mar_10$x1 - 
  0.7 * balanced_panel_data_mar_10$x2 + 
  rnorm(nrow(balanced_panel_data_mar_10), 0, 3)

# All below the 50% quantile are set to missing
mis_simulated_mar_10 <- mis_simulated_mar_10 < quantile(mis_simulated_mar_10, p_mis_10)

# Proportion of missing data
mean(as.numeric(mis_simulated_mar_10))

# Set values to NA in y where missingness occurs
balanced_panel_data_mar_10$y[mis_simulated_mar_10] <- NA

# Summary of y after introducing missingness
summary(balanced_panel_data_mar_10)

#### MNAR ####
##############

#### 50% ####
#### Probabilistic, Linear Regression model, Real data ####

p_mis_50 <- .50
balanced_panel_data_mnar_50 <- balanced_panel_data
# model missingness by logistic regression (probit):
# the missing of a value now also depends on counts itself
mis_simulated_mnar_50 <-0.5 + 1 * balanced_panel_data_mnar_50$x1 - 
  0.7 * balanced_panel_data_mnar_50$x2 - 
  5 * balanced_panel_data_mnar_50$y + rnorm(nrow(balanced_panel_data), 0, 3)
mis_simulated_mnar_50 <- mis_simulated_mnar_50 < quantile(mis_simulated_mnar_50, p_mis_50)
balanced_panel_data_mnar_50$y[mis_simulated_mnar_50] <- NA
summary(balanced_panel_data_mnar_50)

#### 30% ####

p_mis_30 <- .30
balanced_panel_data_mnar_30 <- balanced_panel_data
# model missingness by logistic regression (probit):
# the missing of a value now also depends on counts itself
mis_simulated_mnar_30 <-0.5 + 1 * balanced_panel_data_mnar_30$x1 - 
  0.7 * balanced_panel_data_mnar_30$x2 - 
  5 * balanced_panel_data_mnar_30$y + rnorm(nrow(balanced_panel_data), 0, 3)
mis_simulated_mnar_30 <- mis_simulated_mnar_30 < quantile(mis_simulated_mnar_30, p_mis_30)
balanced_panel_data_mnar_30$y[mis_simulated_mnar_30] <- NA
summary(balanced_panel_data_mnar_30)

#### 10% ####

p_mis_10 <- .10
balanced_panel_data_mnar_10 <- balanced_panel_data
# model missingness by logistic regression (probit):
# the missing of a value now also depends on counts itself
mis_simulated_mnar_10 <-0.5 + 1 * balanced_panel_data_mnar_10$x1 - 
  0.7 * balanced_panel_data_mnar_10$x2 - 
  5 * balanced_panel_data_mnar_10$y + rnorm(nrow(balanced_panel_data), 0, 3)
mis_simulated_mnar_10 <- mis_simulated_mnar_10 < quantile(mis_simulated_mnar_10, p_mis_10)
balanced_panel_data_mnar_10$y[mis_simulated_mnar_10] <- NA
summary(balanced_panel_data_mnar_10)












