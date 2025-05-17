# Portfolio Optimization with EuroStoxx 600 Assets

**University Project – Statistical Methods for Finance**  

---

### Project Overview

This project performs a comprehensive portfolio analysis using a subset of 20 stocks from the EuroStoxx 600 index. The main objectives include:

1. Statistical analysis of asset returns across multiple timeframes  
2. Construction of efficient frontiers with and without a risk-free asset  
3. Comparison of constrained and unconstrained optimization strategies  
4. Statistical testing of portfolio performance  
5. Evaluation of the impact of sector performance on portfolio efficiency  

**Time periods analyzed:**

- **S1**: Full sample (1999–2024)  
- **S2**: Pre-2017 (1999–2016)  
- **S3**: Recent years (2017–2023)

---

### Dataset

The project uses a provided Excel file that includes:

- Monthly prices of selected stocks  
- EuroStoxx 600 index values  
- Monthly Euribor rates (proxy for risk-free rate)  
- Economic sector classification for each stock  

---

### Methods and Techniques

- **Return Analysis**: Monthly log-returns, averages, variance, and correlation matrices  
- **Efficient Frontiers**:  
  - Without risk-free rate (with and without positivity constraints on weights)  
  - With risk-free rate (using the latest Euribor data for each period)  
- **Key Portfolios**:  
  - Global Minimum Variance (GMV)  
  - Maximum Trade-Off  
  - Tangency Portfolio  
- **Performance Testing**:  
  - Sharpe ratio significance tests  
  - Sector exclusion tests  
  - Efficiency testing of inverse-variance-weighted portfolios  
- **Risk Aversion Modeling**:  
  - Optimal portfolios computed for various risk aversion levels (0.1, 1, 3)

---

### Files Included

- _To be added at the end of the project_

---

### How to Run

To replicate the analysis:

1. Clone this repository and open `portfolio_optimization_eurostoxx.R` in R or RStudio  
2. Ensure the following R packages are installed:  
   `quantmod`, `astsa`, `readxl`, `tidyverse`, `fImport`, `fBasics`, `rugarch`, `roll`, `distr`, `PortfolioAnalytics`, `DEoptim`, `ROI`  
3. Place the Excel data file `DatiHW3.xlsx` in the same directory  
4. Run the script step-by-step to generate all outputs and plots  

---

### References

1. EuroStoxx 600 index and constituents  
2. Modern Portfolio Theory (Markowitz, Sharpe)  
3. `PortfolioAnalytics` and `ROI` R packages documentation  

