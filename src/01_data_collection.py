import yfinance as yf
import pandas as pd
import numpy as np
from datetime import datetime, timedelta

def download_financial_data(symbols, period="5y"):
    """
    Télécharge les données financières pour une liste de symboles
    """
    all_data = []
    
    for symbol in symbols:
        try:
            print(f"Downloading {symbol}...")
            ticker = yf.Ticker(symbol)
            df = ticker.history(period=period)
            df['Symbol'] = symbol
            df['Returns'] = df['Close'].pct_change()
            df['Log_Returns'] = np.log(df['Close'] / df['Close'].shift(1))
            df.reset_index(inplace=True)
            all_data.append(df)
            
        except Exception as e:
            print(f"Error downloading {symbol}: {str(e)}")
    
    combined_df = pd.concat(all_data, ignore_index=True)
    return combined_df

# Exécution
symbols = ['AAPL', 'GOOGL', 'MSFT', 'NVDA', 'TSLA', 
           '^GSPC', '^VIX', 'BTC-USD']

data = download_financial_data(symbols)
data.to_csv('data/raw/financial_data_raw.csv', index=False)
data.to_pickle('data/raw/financial_data_raw.pkl')

print(data.info())
print(data.describe())