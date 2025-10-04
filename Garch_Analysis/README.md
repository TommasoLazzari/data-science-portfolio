# Modeling and Forecasting Volatility of Financial Returns: a GARCH Analysis of Starbucks and Walmart

---

### Project Overview

This project implements a detailed time series volatility analysis using GARCH-type models applied to two financial assets:

- **Starbucks Corporation (SBUX.O)**
- **Walmart Inc. (WMT)**

The objectives of the study are:

1. Visualization and preliminary statistical analysis of daily log returns  
2. Testing for heteroskedasticity in return series  
3. Fitting and comparing various GARCH-family models  
4. Evaluating innovation distributional assumptions (normal vs. Student-t)  
5. Performing out-of-sample volatility forecasts  
6. Comparing predictive accuracy via Diebold-Mariano tests  
7. Assessing dynamic correlations using rolling window analysis  

**Timeframe analyzed:**

- **Training set**: From start of data to **March 31, 2023**  
- **Test set**: **April 1, 2023** to **March 31, 2024**

---

### Dataset

The dataset consists of **daily adjusted closing prices** downloaded from **Refinitiv**, for a period exceeding **5 years**, ending in March 2024. The following were computed:

- Daily **log-returns** (percentage scale)
- Proxy measures for **volatility** and **variance**
- Rolling windows for correlation analysis

---

### Methods and Techniques

- **Return Analysis**:  
  - Summary statistics (mean, skewness, kurtosis)  
  - Jarque-Bera test for normality  
  - Graphical analysis of return, absolute return, and squared return series  

- **Model Estimation**:  
  - GARCH(1,1), EGARCH(1,1), GJR-GARCH(1,1), and APARCH(1,1)  
  - Estimation under both **Normal** and **Student-t** innovation assumptions  
  - Standardized residual diagnostics  
  - Sign bias tests and Q-Q plots  

- **Model Evaluation**:  
  - Forecasting static one-step-ahead volatility for one year  
  - **Information criteria** (AIC, BIC, HQ, Shibata)  
  - **Diebold-Mariano tests** on forecast accuracy across model pairs  

- **Correlation Dynamics**:  
  - Rolling correlations between standardized residuals using:
    - 62-day window
    - 252-day window  
  - Graphical inspection for time-varying behavior

---

### Files Included

- `code/garch_analysis_sbux_wmt.R`  
  Main R script containing the full analysis: data preprocessing, GARCH model estimation, forecasting, model evaluation, and rolling correlation computation.

- `data/Price_History_SBUX.O_WMT.xlsx`  
  Excel file with historical price data for Starbucks (SBUX.O) and Walmart (WMT).

- `docs/Garch_Analysis_Report.pdf`  
  Final project report summarizing the methodology, empirical results, figures, and comments.

---

### How to Run

To replicate the analysis:

1. Clone this repository and open `garch_analysis_sbux_wmt.R` in R or RStudio  
2. Ensure the following R packages are installed:  
   `quantmod`, `astsa`, `readxl`, `tidyverse`, `fImport`, `fBasics`, `rugarch`, `roll`, `distr`, `sandwich`  
3. Place the Excel data file `Price_History_SBUX.O_WMT.xlsx` in your working directory  
4. Run the script step-by-step to generate all outputs, figures, and model diagnostics  

---
