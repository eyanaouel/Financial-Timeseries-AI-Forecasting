library(forecast)
library(tseries)
library(tidyverse)
library(here)

# Fonction pour modéliser un actif avec ARIMA
fit_arima_model <- function(symbol_name, data) {
  
  cat("=== Modélisation ARIMA pour", symbol_name, "===\n")
  
  # Préparer les données
  train_data <- data %>%
    filter(Symbol == symbol_name, dataset == "train") %>%
    arrange(Date) %>%
    pull(Returns) %>%
    na.omit() %>%
    ts(frequency = 252)
  
  test_data <- data %>%
    filter(Symbol == symbol_name, dataset == "test") %>%
    arrange(Date) %>%
    pull(Returns) %>%
    na.omit()
  
  # 1. Auto ARIMA avec recherche exhaustive
  model_auto <- auto.arima(
    train_data,
    seasonal = FALSE,
    stepwise = FALSE,
    approximation = FALSE,
    trace = TRUE,
    ic = "aicc"
  )
  
  cat("\nModèle optimal:", paste0("ARIMA", 
      paste(arimaorder(model_auto), collapse = ",")), "\n")
  
  # 2. Diagnostics des résidus
  cat("\n--- Diagnostics des résidus ---\n")
  
  # Test de Ljung-Box
  lb_test <- Box.test(residuals(model_auto), 
                      lag = 20, 
                      type = "Ljung-Box")
  cat("Ljung-Box Test p-value:", lb_test$p.value, "\n")
  
  # Test de normalité (Jarque-Bera)
  jb_test <- jarque.bera.test(residuals(model_auto))
  cat("Jarque-Bera Test p-value:", jb_test$p.value, "\n")
  
  # Test ARCH (autocorrélation des résidus au carré)
  arch_test <- Box.test(residuals(model_auto)^2, 
                        lag = 12, 
                        type = "Ljung-Box")
  cat("ARCH Test p-value:", arch_test$p.value, "\n")
  
  # 3. Graphiques de diagnostic
  png(paste0("results/figures/arima_diagnostics_", symbol_name, ".png"),
      width = 12, height = 8, units = "in", res = 300)
  checkresiduals(model_auto)
  dev.off()
  
  # 4. Prévisions
  h_forecast <- length(test_data)
  forecasts <- forecast(model_auto, h = h_forecast)
  
  # 5. Calcul des métriques
  metrics <- tibble(
    Symbol = symbol_name,
    Model = "ARIMA",
    Order = paste0("(", paste(arimaorder(model_auto), collapse = ","), ")"),
    AIC = AIC(model_auto),
    BIC = BIC(model_auto),
    RMSE = sqrt(mean((test_data - forecasts$mean)^2)),
    MAE = mean(abs(test_data - forecasts$mean)),
    MAPE = mean(abs((test_data - forecasts$mean) / test_data)) * 100,
    LB_pvalue = lb_test$p.value,
    JB_pvalue = jb_test$p.value,
    ARCH_pvalue = arch_test$p.value
  )
  
  # 6. Sauvegarder le modèle
  saveRDS(list(
    model = model_auto,
    forecasts = forecasts,
    metrics = metrics,
    train_data = train_data,
    test_data = test_data
  ), paste0("results/models/arima_", symbol_name, ".rds"))
  
  return(metrics)
}

# Exécution pour tous les symboles
data <- readRDS("data/processed/financial_data_processed.rds")
symbols <- unique(data$Symbol)

arima_results <- map_dfr(symbols, ~fit_arima_model(.x, data))

# Sauvegarder les résultats
write_csv(arima_results, here("results/tables/arima_performance.csv"))

# Tableau comparatif
kable(arima_results %>% arrange(RMSE), 
      caption = "Performance des modèles ARIMA",
      digits = 4) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))