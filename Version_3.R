

pdata <- pdata.frame(lstm_bal_mcar_50, index = c("ID", "Year"))
fixed_model <- plm(Income ~ Year + Education + Age, data = pdata, model = "within")



# Load necessary library
library(dplyr)

# Convert 'Year' and 'Education' to factors
lstm_bal_mcar_50 <- lstm_bal_mcar_50 %>%
  mutate(
    Year = as.factor(Year),
    Education = as.factor(Education)
  )

# De-mean 'Income' and 'Age' (numeric variables) by group (ID)
lstm_bal_mcar_50 <- lstm_bal_mcar_50 %>%
  group_by(ID) %>%
  mutate(
    Income_demeaned = Income - mean(Income, na.rm = TRUE),
    Age_demeaned = Age - mean(Age, na.rm = TRUE)
  ) %>%
  ungroup()

# Run OLS regression with de-meaned 'Income' and 'Age', and categorical 'Year' and 'Education'
fixed_model_manual <- lm(Income_demeaned ~ Year + Education + Age_demeaned, data = lstm_bal_mcar_50)

# Display the summary of the regression model
summary(fixed_model_manual)

