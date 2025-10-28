library(tidyverse)
library(tseries)
library(zoo)
library(here)

# Charger les données
data <- read_csv(here("data/raw/financial_data_raw.csv"))

# 1. Gestion des valeurs manquantes
data_clean <- data %>%
  group_by(Symbol) %>%
  arrange(Date) %>%
  mutate(
    # Imputation par interpolation linéaire
    Close_imputed = na.approx(Close, na.rm = FALSE),
    Volume_imputed = na.approx(Volume, na.rm = FALSE),
    
    # Flag pour valeurs imputées
    Close_was_missing = is.na(Close),
    Volume_was_missing = is.na(Volume)
  ) %>%
  ungroup()

# 2. Détection des outliers (méthode IQR)
detect_outliers <- function(x, k = 3) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lower <- q1 - k * iqr
  upper <- q3 + k * iqr
  return(x < lower | x > upper)
}

data_clean <- data_clean %>%
  group_by(Symbol) %>%
  mutate(
    Returns_outlier = detect_outliers(Returns),
    Volume_outlier = detect_outliers(Volume)
  ) %>%
  ungroup()

# 3. Créer des features techniques
data_features <- data_clean %>%
  group_by(Symbol) %>%
  arrange(Date) %>%
  mutate(
    # Moving Averages
    MA_7 = rollmean(Close, k = 7, fill = NA, align = "right"),
    MA_30 = rollmean(Close, k = 30, fill = NA, align = "right"),
    MA_90 = rollmean(Close, k = 90, fill = NA, align = "right"),
    
    # Volatilité roulante
    Vol_30 = rollapply(Returns, width = 30, FUN = sd, 
                       fill = NA, align = "right"),
    
    # RSI simplifié
    Price_change = Close - lag(Close),
    Gain = ifelse(Price_change > 0, Price_change, 0),
    Loss = ifelse(Price_change < 0, abs(Price_change), 0),
    Avg_gain = rollmean(Gain, k = 14, fill = NA, align = "right"),
    Avg_loss = rollmean(Loss, k = 14, fill = NA, align = "right"),
    RS = Avg_gain / Avg_loss,
    RSI = 100 - (100 / (1 + RS)),
    
    # Bollinger Bands
    BB_middle = MA_30,
    BB_sd = rollapply(Close, width = 30, FUN = sd, 
                      fill = NA, align = "right"),
    BB_upper = BB_middle + 2 * BB_sd,
    BB_lower = BB_middle - 2 * BB_sd
  ) %>%
  ungroup()

# 4. Split Train/Test (90% / 10%)
split_data <- function(df, train_ratio = 0.9) {
  df %>%
    group_by(Symbol) %>%
    arrange(Date) %>%
    mutate(
      n_total = n(),
      split_point = floor(n_total * train_ratio),
      dataset = ifelse(row_number() <= split_point, "train", "test")
    ) %>%
    ungroup()
}

data_split <- split_data(data_features)

# Sauvegarder
write_csv(data_split, here("data/processed/financial_data_processed.csv"))
saveRDS(data_split, here("data/processed/financial_data_processed.rds"))

# Rapport de qualité
quality_report <- data_split %>%
  group_by(Symbol) %>%
  summarise(
    Total_obs = n(),
    Train_obs = sum(dataset == "train"),
    Test_obs = sum(dataset == "test"),
    Missing_before = sum(Close_was_missing),
    Outliers_returns = sum(Returns_outlier, na.rm = TRUE),
    Outliers_volume = sum(Volume_outlier, na.rm = TRUE)
  )

write_csv(quality_report, here("results/tables/data_quality_report.csv"))
