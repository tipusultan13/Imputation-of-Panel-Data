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

# 'ID' column
count(data, ID)

# 'Year' column
count(data, Year)

# 'Education' column - Highest general school degree
count(data, Education)
data$Education[is.na(data$Education)] <- 7

# MedianIncome
count(data, MedianIncome)
summary(data$MedianIncome)

# 'IndividualIncome' column - Income
count(data, IndividualIncome)
data$IndividualIncome[is.na(data$IndividualIncome)] <- 0
summary(data$IndividualIncome)

sum(is.na(data)) #Total number of NA values in the data frame
summary(data)

########################
## Balanced Panel
########################

