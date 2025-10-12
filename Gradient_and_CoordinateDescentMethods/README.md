# Gradient and Coordinate Descent Methods  

---

### Project Overview  

This project explores the **theoretical foundations**, **algorithmic implementations**, and **empirical performance** of several **first-order optimization algorithms**, focusing on **Gradient Descent** and **Coordinate Descent** methods.  

The study aims to provide both **theoretical insights** and **experimental comparisons** between different descent strategies when solving supervised learning problems. Specifically, it analyzes:  

1. **Gradient Descent (GD)** with multiple step-size strategies:  
   - Fixed step size  
   - Armijo rule  
   - Exact line search  
   - Lipschitz-based step size  
2. **Coordinate Minimization (CD)**  
3. **Block Coordinate Gradient Descent (BCGD)**  

Each method is assessed in terms of **convergence rate**, **computational efficiency**, and **robustness** across both synthetic and real-world datasets.  

---

### Datasets  

Two main datasets were employed for empirical evaluation:  

- **Synthetic Dataset**:  
  - 400 points sampled uniformly from the unit square \([0,1]^2\)  
  - Semi-supervised binary classification setup with labeled and unlabeled points  
  - Similarity matrices \(W\) and \(\hat{W}\) computed from Euclidean distances  

- **Breast Cancer Wisconsin (Diagnostic) Dataset**:  
  - Source: [UCI Machine Learning Repository](https://archive.ics.uci.edu/ml/datasets/breast+cancer+wisconsin+(diagnostic))  
  - 569 samples, 30 standardized features  
  - Binary classification task (malignant vs. benign)  

---

### Methods and Techniques  

The analysis covers both **theoretical derivations** and **empirical implementation** of descent algorithms:  

- **Gradient Descent (GD)**  
  - Convergence analysis under Lipschitz and strong convexity assumptions  
  - Rate comparison between fixed step size, Armijo, and line search variants  

- **Coordinate Minimization (CD)**  
  - Sequential and parallel update schemes (Gauss–Seidel and Jacobi)  
  - Efficient handling of high-dimensional problems  

- **Block Coordinate Gradient Descent (BCGD)**  
  - Random, cyclic, and Gauss–Southwell block selection strategies  
  - Convergence proofs under block-wise Lipschitz continuity  

- **Experimental Evaluation**  
  - Loss minimization on both synthetic and real datasets  
  - Comparative performance in terms of objective value, optimality gap, runtime, and iterations  

---

### Files Included  

- `docs/Gradient_and_CoordinateDescentMethods_Report.pdf`  
  Final report detailing the theoretical framework, algorithmic derivations, experimental design, and results.  

- `code/Gradient_and_CoordinateDescentMethods_code.ipynb`  
  Jupyter Notebook implementing all algorithms (GD, CD, BCGD) with convergence analysis, plots, and tables.  

---

### How to Run  

To reproduce the results:  

1. Clone this repository and open the notebook `code/Gradient_and_CoordinateDescentMethods_code.ipynb` in **Jupyter Notebook** or **JupyterLab**.  
2. Make sure the following Python packages are installed:  
   `numpy`, `pandas`, `matplotlib`, `scikit-learn`, `time`  
3. Run the notebook sequentially to reproduce:  
   - Synthetic dataset experiment  
   - Breast Cancer dataset experiment  
   - Convergence plots and performance tables  

---

### References  

1. Rinaldi, F. (2025). *Optimization Methods for Data Science* – Lecture notes, University of Padova (unpublished material).  
2. Dua, D. & Graff, C. (2019). *Breast Cancer Wisconsin (Diagnostic) Data Set*. UCI Machine Learning Repository.  
