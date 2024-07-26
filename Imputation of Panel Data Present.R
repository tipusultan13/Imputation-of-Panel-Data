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
## Balanced Panel
########################






