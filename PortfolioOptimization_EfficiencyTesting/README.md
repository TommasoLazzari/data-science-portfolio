# Portfolio Optimization and Efficiency Testing  

### Project Overview  

This project investigates **portfolio optimization and efficiency testing** within the framework of **Modern Portfolio Theory** (Markowitz, 1952).  

The study develops both the **theoretical background** and **empirical analysis** of portfolio selection, with special focus on:  

1. The **efficient frontier** and its properties  
2. The role of the **risk-free asset** and the **Capital Market Line (CML)**  
3. The impact of **short-selling constraints**  
4. Statistical **inference on portfolio efficiency** (Sharpe ratio tests, exclusion tests)  
5. Practical applications to **EuroStoxx 600 stocks** across multiple sectors and time horizons  

The goal is to combine theory with data-driven analysis, critically assessing how efficiency concepts translate into real financial markets.  

---

### Dataset  

The empirical analysis is based on **20 arbitrarily selected stocks** from the **EuroStoxx 600 index**, covering four key sectors:  

- **Energy** (e.g., Shell, Repsol, OMV)  
- **Technology** (e.g., ASM International, Soitec)  
- **Financials** (e.g., Allianz, Aviva, Swedbank)  
- **Real Estate** (e.g., Castellum, Covivio)  

Three time horizons were considered:  

- **S1**: 31/12/1999 – 30/04/2024  
- **S2**: 31/12/1999 – 31/12/2016  
- **S3**: 31/12/2016 – 31/12/2023  

The dataset is contained in:  

- `data/DatiHW3.xlsx`  

---

### Methods and Techniques  

- **Efficient Frontier**: construction of minimum variance, maximum trade-off, and tangency portfolios  
- **Capital Market Line (CML)**: integration of a risk-free asset to determine optimal allocations under different risk aversion levels  
- **Short-Selling Constraints**: analysis of long-only portfolios and their effect on efficiency  
- **Efficiency Inference**:  
  - Sharpe ratio significance and equality tests  
  - Exclusion tests for asset groups or sectors  
- **Sector-Level Analysis**: comparison of efficiency across Energy, Technology, Financials, and Real Estate  
- **Alternative Portfolios**: testing of inverse-variance weighted portfolios  

---

### Files Included  

- `PortfolioOptimization_EfficiencyTesting_Report.pdf`  
  Final report including theoretical background, methodology, empirical results, and visualizations.  

- `PortfolioOptimization_EfficiencyTesting_code.R`  
  R script implementing portfolio optimization routines, efficiency tests, and graphical outputs.  

- `data/DatiHW3.xlsx`  
  Excel file with historical stock return data used in the analysis.  

---

### How to Run  

To replicate the analysis:  

1. Clone this repository and open `PortfolioOptimization_EfficiencyTesting_code.R` in R or RStudio.  
2. Ensure the following R packages are installed:  
   `quadprog`, `readxl`, `tidyverse`, `xts`, `PerformanceAnalytics`, `tseries`, `ggplot2`  
3. Place the Excel file `DatiHW3.xlsx` in the `data/` directory.  
4. Run the script step-by-step to reproduce all calculations, efficiency tests, and plots.  

---

### References  

1. Markowitz, H. (1952). *Portfolio Selection*. The Journal of Finance.  
2. Tobin, J. (1958). *Liquidity Preference as Behavior Towards Risk*. The Review of Economic Studies.  
3. Sharpe, W. F. (1966). *Mutual Fund Performance*. Journal of Business.  
4. Jobson, J. D. & Korkie, B. M. (1981). *Performance Hypothesis Testing with the Sharpe and Treynor Measures*. Journal of Finance.  
5. Ledoit, O. & Wolf, M. (2008). *Robust Performance Hypothesis Testing with the Sharpe Ratio*. Journal of Empirical Finance.  
