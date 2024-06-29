# Thesis: Modelling Long Term Volatility in Financial Markets

## Overview
This repository contains the code and datasets used in our thesis, which focuses on modeling long-term volatility in financial markets. 
The study builds upon the Realized GARCH specification introduced by Hansen (2011) and extends the work of Huang et al. (2016) 
to enhance volatility forecasting using option-implied variances and jump and continuous components.

## Data
We use Realized volatility based on 5 min stock returns, VIX and daily stock returns.

## Methodology
- **HAR Model Extension:** Incorporates option-implied variances from volatility indices and jump and continuous components into the HAR model framework.
- - **Realized GARCH Model Extension:** Incorporates HAR structure into GARCH'S measurement equation
- **Evaluation:** Utilizes a value-at-risk (VaR) and Expected Shortfall(ES) approach to assess the practical significance of the improved forecasts in risk management scenarios.

