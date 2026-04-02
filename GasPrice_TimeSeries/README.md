# Explaining Gas Price Dynamics in Italy and Turkey

## Business, Economic and Financial Data Project  
**University of Padova — MSc Data Science**

This project investigates the determinants of **residential natural gas price dynamics** in **Italy** and **Turkey** using an **explanatory econometric approach**.

Rather than focusing on forecasting, the goal is to identify the **economic, climate-related, and structural factors** driving gas price evolution across the two countries.

---

# Project Overview

The analysis compares multiple econometric models:

- Linear Regression Models  
- ARIMA and SARIMA Models  
- ARIMAX Models (with external regressors)  
- Generalized Additive Models (GAM)

The results reveal **structurally different price dynamics**:

- **Italy** → Nonlinear trends and structural breaks dominate  
- **Turkey** → Linear downward trend and seasonal dynamics dominate  

These findings highlight the importance of **choosing models that reflect country-specific energy market characteristics**.

---

# Data Sources

The study uses **biannual data from 2007–2025**.

## Gas Prices

- Source: Eurostat  
- Residential household gas prices  
- Consumption Band: 20–199 GJ  
- Unit: Euro  

## External Variables

### Henry Hub Natural Gas Prices
- Source: U.S. Energy Information Administration (EIA)  
- Proxy for global gas market dynamics  

### Brent Crude Oil Prices
- Source: U.S. Energy Information Administration (EIA)  
- Proxy for global energy market conditions  

### European Temperature Anomalies
- Source: National Centers for Environmental Information (NCEI)  
- Climate-related demand proxy  

All variables are harmonized to **biannual frequency**.

---

# Methodology

The modeling strategy follows a progressive framework.

## 1. Linear Models

Baseline explanatory models including:

- Trend components  
- Structural shocks  
- External regressors  

---

## 2. ARIMA / SARIMA

Pure time-series models capturing:

- Autocorrelation  
- Seasonality  
- Temporal dependencies  

---

## 3. ARIMAX

Extended time-series models incorporating:

- External regressors  
- Structural shocks  
- Seasonal autoregressive components  

---

## 4. Generalized Additive Models (GAM)

Flexible nonlinear models allowing:

- Smooth trends  
- Nonlinear relationships  
- Structural changes  

---

# Project Structure

```
BEFD_project/
│
├── code/
│   ├── BEFD_Project_code.R
│
├── docs/
│   ├── BEFD_Project_Report.pdf
│
├── data/
│   ├── nrg_pc_202__custom_18451811_page_spreadsheet.xlsx
│   ├── BRENT_price.xls
│   ├── HenryHub_price.xls
│   └── temperature_anomalies_EU.csv
│
└── README.md
```

---

# References

- Eurostat — Natural Gas Prices  
- U.S. Energy Information Administration (EIA)  
- National Centers for Environmental Information (NCEI)  
- Hyndman & Athanasopoulos (2018) — Forecasting: Principles and Practice  
- Wood (2017) — Generalized Additive Models  

---

# Authors

- Tommaso Lazzari  
- Iaroslav Tkachenko  
- Kübra Deniz Değirmenci  
