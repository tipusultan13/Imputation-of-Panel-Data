###################################
## Tipu Sultan
###################################

# Set Directory
# setwd("/Users/tipusultan/Documents/GitHub/Imputation-of-Panel-Data")

########################
## Data Preparation
########################

# Load Data
LifeExpectancy <- read_excel('life expectancy.xlsx', col_names = FALSE)
LifeExpectancy <- as.data.frame(LifeExpectancy)
LifeExpectancy <- LifeExpectancy[-1, ]
colnames(LifeExpectancy) <- LifeExpectancy[1, ]
LifeExpectancy <- LifeExpectancy[-1, ]
LifeExpectancy <- LifeExpectancy[order(LifeExpectancy$`Country Name`), ]

LifeExpectancy <- LifeExpectancy[, c('Country Name', 'Year', 'Health Expenditure %')]
View(LifeExpectancy)

#### Sample, Real Data ####

p_mis <- 0.30
data_real_mcar <- data_real
# Indicator for which values are set to missing exactly p_mis % missing values
mis_real_mcar <- sample(1:1000, p_mis * 1000, replace = FALSE)    
data_real_mcar[mis_real_mcar, 3] <- NA
summary(data_real_mcar)

#####################
## MAR Missing Data
#####################


#### Deterministic, Real Data ####

data_real_mar_d <- data_real
q_views <- qnorm(.3, 16, 7)
# This creates a logical vector which indicates when the "true value" of subscribers is below the theoretical quantile.
mis_mar_d <- views < q_views 
data_real_mar_d$counts[mis_mar_d] <- NA
summary(data_real_mar_d)

#### Probabilistic, Linear Regression model, Real data ####

data_real_mnar_p <- data_real
# model missingness by logistic regression (probit):
# the missing of a value now also depends on counts itself
z_miss_mnar_p <-0.5 + 1 * subscribers - 0.7 * views - 5 * counts + rnorm(1000, 0, 3)
mis_mnar_p <- z_miss_mnar_p < quantile(z_miss_mnar_p, p_mis)
data_real_mnar_p$counts[mis_mnar_p] <- NA
summary(data_real_mnar_p)

#############################
## Visualize the Missing Data
#############################


library(VIM)
library("dplyr")
library("ggplot2")
library("gridExtra")
par(mfrow=c(2,3)) 

matrixplot(data_real_mcar, sortby = c('counts'), main = "MCAR on Real Data")
matrixplot(data_simulated_mcar, sortby = c('x3'), main = "MCAR on Simulated Data")

matrixplot(data_real_mar_d, sortby = c('counts'), main = "MAR on Real Data")
matrixplot(data_simulated_mar_d, sortby = c('x3'), main = "MAR on simulated Data")

matrixplot(data_real_mnar_p, sortby = c('counts'), main = "MNAR on Real Data")
matrixplot(data_simulated_mnar_p, sortby = c('x3'), main = "MNAR on simulated Data")

# Some other plot that can be mentioned
par(mfrow=c(1,1)) 
aggr(data_real_mcar,numbers = TRUE, prop = c(TRUE, FALSE)) # similar for all the data. 
pbox(data_real_mcar[, c('views', "counts")]) # similar for all the data. 




