library(here)

# model SARIMA avec diagnostics et comparaison ARIMA
fit_sarima_model <- function(symbol_name, data) {
  
  cat("=== Modélisation SARIMA pour", symbol_name, "===\n")
  
  # Préparer les données avec fréquence hebdomadaire
  train_data <- data %>%
    filter(Symbol == symbol_name, dataset == "train") %>%
    arrange(Date) %>%
    pull(Returns) %>%
    na.omit() %>%
    ts(frequency = 5) # 5 jours de trading par semaine
  
  test_data <- data %>%
    filter(Symbol == symbol_name, dataset == "test") %>%
    arrange(Date) %>%
    pull(Returns) %>%
    na.omit()
  
  # Auto ARIMA avec composante saisonnière
  model_sarima <- auto.arima(
    train_data,
    seasonal = TRUE,
    stepwise = FALSE,
    approximation = FALSE,
    D = 1, # Différenciation saisonnière
    max.P = 2,
    max.Q = 2,
    ic = "aicc"
  )
  
  cat("\nModèle SARIMA optimal:\n")
  print(model_sarima)
  
  # Diagnostics
  checkresiduals(model_sarima)
  
  # Prévisions
  h_forecast <- length(test_data)
  forecasts <- forecast(model_sarima, h = h_forecast)
  
  # Métriques
  metrics <- tibble(
    Symbol = symbol_name,
    Model = "SARIMA",
    Order = paste0(arimaorder(model_sarima), collapse = ","),
    RMSE = sqrt(mean((test_data - forecasts$mean)^2)),
    MAE = mean(abs(test_data - forecasts$mean)),
    MAPE = mean(abs((test_data - forecasts$mean) / test_data)) * 100
  )
  
  # Graphique de comparaison ARIMA vs SARIMA
  arima_results <- readRDS(paste0("results/models/arima_", symbol_name, ".rds"))
  
  comparison_plot <- data.frame(
    Date = seq_along(test_data),
    Actual = test_data,
    ARIMA = arima_results$forecasts$mean,
    SARIMA = forecasts$mean
  ) %>%
    pivot_longer(cols = -Date, names_to = "Model", values_to = "Returns") %>%
    ggplot(aes(x = Date, y = Returns, color = Model)) +
    geom_line(size = 1) +
    labs(title = paste("Comparaison ARIMA vs SARIMA -", symbol_name),
         y = "Returns Prédits",
         x = "Période de Test") +
    theme_minimal() +
    scale_color_manual(values = c("Actual" = "black", 
                                   "ARIMA" = "blue", 
                                   "SARIMA" = "red"))
  
  ggsave(paste0("results/figures/arima_sarima_comparison_", symbol_