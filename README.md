# Financial Time Series Forecasting with Hybrid AI

[![Python](https://img.shields.io/badge/Python-3.9+-blue.svg)]()
[![R](https://img.shields.io/badge/R-4.3+-276DC3.svg)]()
[![License](https://img.shields.io/badge/License-MIT-green.svg)]()

## Project Overview

This project implements a comprehensive comparison of classical statistical models, deep learning architectures, and LLM-powered insights for financial time series forecasting.

### Key Features
-  Multi-asset portfolio analysis (AAPL, GOOGL, MSFT, NVDA, TSLA, BTC-USD, ETH-USD, ^GSPC, ^VIX, GC=F, CL=F)
-  Classical models: ARIMA, SARIMA, VAR, ARCH/GARCH
-  Deep Learning: LSTM, GRU, Hybrid architectures
-  LLM integration for automated insights
-  Interactive dashboard for real-time visualization
-  Comprehensive evaluation metrics

## Installation

### R Environment
```r
source("setup_environment.R")
```

### Python Environment
```bash
conda env create -f environment.yml
conda activate timeseries-ai
```

## Quick Start

1. **Data Collection**
```bash
Rscript src/01_data_collection.R
```

2. **EDA**
```bash
Rscript -e "rmarkdown::render('notebooks/01_exploratory_data_analysis.Rmd')"
```

## License
MIT License
