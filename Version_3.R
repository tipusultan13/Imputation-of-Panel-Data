### Balanced Panel ###

ImpDataBal <- list(
  mice_imp_bal_mcar_10, amelia_imp_bal_mcar_10, mitml_imp_bal_mcar_10, lstm_bal_mcar_10,
  mice_imp_bal_mcar_30, amelia_imp_bal_mcar_30, mitml_imp_bal_mcar_30, lstm_bal_mcar_30,
  mice_imp_bal_mcar_50, amelia_imp_bal_mcar_50, mitml_imp_bal_mcar_50, lstm_bal_mcar_50,
  mice_imp_bal_mar_10, amelia_imp_bal_mar_10, mitml_imp_bal_mar_10, lstm_bal_mar_10,
  mice_imp_bal_mar_30, amelia_imp_bal_mar_30, mitml_imp_bal_mar_30, lstm_bal_mar_30,
  mice_imp_bal_mar_50, amelia_imp_bal_mar_50, mitml_imp_bal_mar_50, lstm_bal_mar_50,
  mice_imp_bal_mnar_10, amelia_imp_bal_mnar_10, mitml_imp_bal_mnar_10, lstm_bal_mnar_10,
  mice_imp_bal_mnar_30, amelia_imp_bal_mnar_30, mitml_imp_bal_mnar_30, lstm_bal_mnar_30,
  mice_imp_bal_mnar_50, amelia_imp_bal_mnar_50, mitml_imp_bal_mnar_50, lstm_bal_mnar_50
)

# Dataset Names
DatasetsNameBal <- c(
  "mice_imp_bal_mcar_10", "amelia_imp_bal_mcar_10", "mitml_imp_bal_mcar_10", "lstm_bal_mcar_10",
  "mice_imp_bal_mcar_30", "amelia_imp_bal_mcar_30", "mitml_imp_bal_mcar_30", "lstm_bal_mcar_30",
  "mice_imp_bal_mcar_50", "amelia_imp_bal_mcar_50", "mitml_imp_bal_mcar_50", "lstm_bal_mcar_50",
  "mice_imp_bal_mar_10", "amelia_imp_bal_mar_10", "mitml_imp_bal_mar_10", "lstm_bal_mar_10",
  "mice_imp_bal_mar_30", "amelia_imp_bal_mar_30", "mitml_imp_bal_mar_30", "lstm_bal_mar_30",
  "mice_imp_bal_mar_50", "amelia_imp_bal_mar_50", "mitml_imp_bal_mar_50", "lstm_bal_mar_50",
  "mice_imp_bal_mnar_10", "amelia_imp_bal_mnar_10", "mitml_imp_bal_mnar_10", "lstm_bal_mnar_10",
  "mice_imp_bal_mnar_30", "amelia_imp_bal_mnar_30", "mitml_imp_bal_mnar_30", "lstm_bal_mnar_30",
  "mice_imp_bal_mnar_50", "amelia_imp_bal_mnar_50", "mitml_imp_bal_mnar_50", "lstm_bal_mnar_50"
)

# Generate empty dataframe
WD_Bal <- data.frame(
  Dataset = character(),
  WassersteinDistance = numeric(),
  stringsAsFactors = FALSE
)

# Wasserstein distances calculation
for (i in seq_along(ImpDataBal)) {
  ImpIncomeBal <- ImpDataBal[[i]]$Income
  wasserstein_distance <- wasserstein1d(balanced_panel_data$Income, ImpIncomeBal)
  
  # Append results
  WD_Bal <- rbind(WD_Bal, data.frame(
    Dataset = DatasetsNameBal[i],
    WassersteinDistance = wasserstein_distance
  ))
}

print(WD_Bal)

# Barplot
GroupColors <- rep(c("red", "blue", "green", "red", "blue", "green", "red", "blue", "green", "red", "blue", "green"), each = 4)
par(mar = c(5, 15, 4, 2)) 
barplot(
  height = WD_Bal$WassersteinDistance,
  names.arg = WD_Bal$Dataset,
  las = 2,
  col = GroupColors,
  main = "Wasserstein Distance Across the Imputed Datasets for Balanced Panel",
  xlab = "Wasserstein Distance",
  ylab = "",
  cex.names = 0.9,
  horiz = TRUE
)

### Unbalanced Panel ###

ImpDataUnbal <- list(
  mice_imp_unbal_mcar_10, amelia_imp_unbal_mcar_10, mitml_imp_unbal_mcar_10, lstm_unbal_mcar_10,
  mice_imp_unbal_mcar_30, amelia_imp_unbal_mcar_30, mitml_imp_unbal_mcar_30, lstm_unbal_mcar_30,
  mice_imp_unbal_mcar_50, amelia_imp_unbal_mcar_50, mitml_imp_unbal_mcar_50, lstm_unbal_mcar_50,
  mice_imp_unbal_mar_10, amelia_imp_unbal_mar_10, mitml_imp_unbal_mar_10, lstm_unbal_mar_10,
  mice_imp_unbal_mar_30, amelia_imp_unbal_mar_30, mitml_imp_unbal_mar_30, lstm_unbal_mar_30,
  mice_imp_unbal_mar_50, amelia_imp_unbal_mar_50, mitml_imp_unbal_mar_50, lstm_unbal_mar_50,
  mice_imp_unbal_mnar_10, amelia_imp_unbal_mnar_10, mitml_imp_unbal_mnar_10, lstm_unbal_mnar_10,
  mice_imp_unbal_mnar_30, amelia_imp_unbal_mnar_30, mitml_imp_unbal_mnar_30, lstm_unbal_mnar_30,
  mice_imp_unbal_mnar_50, amelia_imp_unbal_mnar_50, mitml_imp_unbal_mnar_50, lstm_unbal_mnar_50
)

# Dataset Names
DatasetsNameUnbal <- c(
  "mice_imp_unbal_mcar_10", "amelia_imp_unbal_mcar_10", "mitml_imp_unbal_mcar_10", "lstm_unbal_mcar_10",
  "mice_imp_unbal_mcar_30", "amelia_imp_unbal_mcar_30", "mitml_imp_unbal_mcar_30", "lstm_unbal_mcar_30",
  "mice_imp_unbal_mcar_50", "amelia_imp_unbal_mcar_50", "mitml_imp_unbal_mcar_50", "lstm_unbal_mcar_50",
  "mice_imp_unbal_mar_10", "amelia_imp_unbal_mar_10", "mitml_imp_unbal_mar_10", "lstm_unbal_mar_10",
  "mice_imp_unbal_mar_30", "amelia_imp_unbal_mar_30", "mitml_imp_unbal_mar_30", "lstm_unbal_mar_30",
  "mice_imp_unbal_mar_50", "amelia_imp_unbal_mar_50", "mitml_imp_unbal_mar_50", "lstm_unbal_mar_50",
  "mice_imp_unbal_mnar_10", "amelia_imp_unbal_mnar_10", "mitml_imp_unbal_mnar_10", "lstm_unbal_mnar_10",
  "mice_imp_unbal_mnar_30", "amelia_imp_unbal_mnar_30", "mitml_imp_unbal_mnar_30", "lstm_unbal_mnar_30",
  "mice_imp_unbal_mnar_50", "amelia_imp_unbal_mnar_50", "mitml_imp_unbal_mnar_50", "lstm_unbal_mnar_50"
)

# Generate empty dataframe
WD_Unbal <- data.frame(
  Dataset = character(),
  WassersteinDistance = numeric(),
  stringsAsFactors = FALSE
)

# Wasserstein distances calculation
for (i in seq_along(ImpDataUnbal)) {
  ImpIncomeUnbal <- ImpDataUnbal[[i]]$Income
  wasserstein_distance <- wasserstein1d(unbalanced_panel_data$Income, ImpIncomeUnbal)
  
  # Append results
  WD_Unbal <- rbind(WD_Unbal, data.frame(
    Dataset = DatasetsNameUnbal[i],
    WassersteinDistance = wasserstein_distance
  ))
}

print(WD_Unbal)

# Barplot
GroupColors <- rep(c("red", "blue", "green", "red", "blue", "green", "red", "blue", "green", "red", "blue", "green"), each = 4)
par(mar = c(5, 15, 4, 2)) 
barplot(
  height = WD_Unbal$WassersteinDistance,
  names.arg = WD_Unbal$Dataset,
  las = 2,
  col = GroupColors,
  main = "Wasserstein Distance Across the Imputed Datasets for Unbalanced Panel",
  xlab = "Wasserstein Distance",
  ylab = "",
  cex.names = 0.9,
  horiz = TRUE
)
par(mar = c(5, 4, 4, 2) + 0.1) # set default R margin
