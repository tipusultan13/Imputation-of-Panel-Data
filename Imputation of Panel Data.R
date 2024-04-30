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





