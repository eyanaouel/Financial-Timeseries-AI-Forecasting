library(quantmod)
library(tidyverse)
library(lubridate)
library(here)

options(scipen = 999) 
options(warnings = -1) 

# Configuration
symbols <- c("AAPL", "MSFT", "NVDA", "TSLA", "BTC-USD", "ETH-USD", "GC=F", "CL=F", "^GSPC", "^VIX")
start_date <- Sys.Date() - years(5)
end_date <- Sys.Date()

# Fonction de téléchargement robuste
download_stock_data <- function(symbol, start, end) {
  tryCatch({
    cat("Téléchargement de", symbol, "...\n")
    data <- getSymbols(symbol, 
                       src = "yahoo",
                       from = start,
                       to = end,
                       auto.assign = FALSE)
    
    # Nettoyer les noms de colonnes
    df <- as.data.frame(data) %>%
      rownames_to_column("Date") %>%
      mutate(Date = as.Date(Date),
             Symbol = symbol) %>%
      rename_all(~str_replace_all(., paste0(symbol, "."), ""))
    
    # Calculer les returns
    df <- df %>%
      mutate(Returns = (Close - lag(Close)) / lag(Close),
             Log_Returns = log(Close / lag(Close)))
    
    return(df)
  }, error = function(e) {
    cat("Erreur pour", symbol, ":", e$message, "\n")
    return(NULL)
  })
}

# Télécharger toutes les données
all_data <- map_dfr(symbols, ~download_stock_data(.x, start_date, end_date))

# Créer les dossiers s'ils n'existent pas
dir.create("data", showWarnings = FALSE)
dir.create("data/raw", showWarnings = FALSE)

# Sauvegarder
write_csv(all_data, here("data/raw/financial_data_raw.csv"))
saveRDS(all_data, here("data/raw/financial_data_raw.rds"))

# Statistiques descriptives
summary_stats <- all_data %>%
  group_by(Symbol) %>%
  summarise(
    Start = min(Date),
    End = max(Date),
    N_obs = n(),
    Missing = sum(is.na(Close)),
    Mean_Price = mean(Close, na.rm = TRUE),
    SD_Price = sd(Close, na.rm = TRUE),
    Mean_Volume = mean(Volume, na.rm = TRUE)
  )

write_csv(summary_stats, here("results/tables/data_summary.csv"))
