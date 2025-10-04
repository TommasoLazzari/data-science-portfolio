# FIRST ORDER OPTIMIZATION METHODS: a Comparative Analysis of the Projected Gradient Method and the Frank-Wolfe Algorithm on Portfolio Optimization 

---

### Project Overview  

This project investigates and compares several **first-order optimization algorithms** on the **Markowitz Portfolio Optimization Problem**.  

The analysis covers:  
1. Classical **Frank-Wolfe (FW)** algorithm  
2. **Away-Step Frank-Wolfe (AFW)**  
3. **Pairwise Frank-Wolfe (PFW)**  
4. **Projected Gradient (PG)** method  

The objective is to evaluate their **convergence speed, computational efficiency, and solution sparsity** on real-world financial datasets.  

---

### Dataset  

The algorithms were tested on weekly adjusted returns (dividend- and split-adjusted) from four major stock indices:  

- **FTSE 100**  
- **Dow Jones Industrial Average**  
- **NASDAQ 100**  
- **S&P 500**  

(Source: Bruni et al., *Real-world datasets for portfolio selection and stochastic dominance models*, *Omega*, 2015)  

---

### Methods and Techniques  

- **Frank-Wolfe (FW)**: Projection-free method with linear subproblems  
- **Away-Step FW (AFW)**: Enhanced convergence by stepping away from active vertices  
- **Pairwise FW (PFW)**: Refined updates via pairwise moves in the active set  
- **Projected Gradient (PG)**: Gradient descent with explicit projection  

---

### Files included  

- `docs/FrankWolfe_Portfolio_Optimization_Report.pdf`  
  Final report including theoretical background, algorithmic descriptions, results, and visualizations  

- `code/FrankWolfe_Portfolio_Optimization.ipynb`  
  Jupyter Notebook implementing FW, AFW, PFW, and PG, with empirical analysis on the datasets  

- `data/FTSE100.xlsx`, `DowJones.xlsx`, `NASDAQ100.xlsx`, `SP500.xlsx`  
  Excel files with weekly return data for the four indices  

---

### How to Run  

To replicate the analysis:  

1. Clone this repository and open `FrankWolfe_Portfolio_Optimization.ipynb` in Jupyter Notebook or JupyterLab  
2. Make sure the following Python packages are installed:  
   `numpy`, `pandas`, `matplotlib`, `scipy`, `cvxpy`, `time`  
3. Place the Excel files from the `data/` folder in the same working directory  
4. Run the notebook step-by-step to reproduce algorithm runs, results, and plots  

---

### References  

1. Frank, M. & Wolfe, P. (1956). *An algorithm for quadratic programming*. Naval Research Logistics Quarterly.  
2. Jaggi, M. (2013). *Revisiting Frank-Wolfe: Projection-free sparse convex optimization*. ICML.  
3. Lacoste-Julien, S. & Jaggi, M. (2015). *On the global linear convergence of Frank-Wolfe variants*. NeurIPS.  
4. Markowitz, H. (1952). *Portfolio Selection*. The Journal of Finance.  
5. Bruni, R., Cesarone, F., Scozzari, A., & Tardella, F. (2015). *Real-world datasets for portfolio selection*. *Omega*.  
