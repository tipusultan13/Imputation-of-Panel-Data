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
data <- subset(data, Year >= 2013 & Year <= 2023) # Filter the data to keep only rows where the 'Year' is between 2013 and 2023 inclusive

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
# Ensure the 'Year' column is numeric or integer
data$Year <- as.numeric(data$Year)

# Find IDs that are present in all years between 2013 and 2023
years <- 2013:2023
common_ids <- Reduce(intersect, lapply(years, function(year) {
  unique(data$ID[data$Year == year])
}))

# Filter the data to keep only rows with common IDs
BalancedPanel <- data[data$ID %in% common_ids, ]

# Print the final data to check the result
print(BalancedPanel)

# Count the number of unique years each ID appears in
id_year_count <- aggregate(Year ~ ID, data = BalancedPanel, 
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
UnbalancedPanel <- data

# Count the number of unique years each ID appears in
id_year_count <- aggregate(Year ~ ID, data = UnbalancedPanel, 
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




