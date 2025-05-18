# Gradient Descent Methods for Semi-Supervised Learning

---

### Project Overview

This project investigates and compares several **gradient-based optimization methods** in the context of **semi-supervised learning**, using both synthetic and real-world data.

The analysis covers:

1. Classical Gradient Descent with fixed step sizes (arbitrary and Lipschitz-based)  
2. Adaptive Gradient Descent via the Armijo rule  
3. Block Coordinate Gradient Descent with Gauss–Southwell (GS) rule  
4. Coordinate Minimization  

These methods are assessed in terms of convergence behavior, theoretical properties, and performance on real data from the **Breast Cancer Wisconsin (Diagnostic)** dataset.

---

### Contents

- `GradientDescentMethods_SemisupervisedLearning_Report.pdf`  
  Final report detailing the methodology, theoretical derivations, algorithmic comparisons, and empirical evaluations.

- `GradientDescentMethods_SemisupervisedLearning_TheoreticalApproach.ipynb`  
  Jupyter notebook implementing all optimization algorithms on **synthetic data** (randomly generated 2D points).

- `GradientDescentMethods_SemisupervisedLearning_RealData_.ipynb`  
  Jupyter notebook applying the same algorithms to the **Breast Cancer dataset**, including preprocessing and classification accuracy evaluation.

---

### Methods and Techniques

- **Synthetic Data Modeling**:  
  - Generation of labeled and unlabeled points in a 2D space  
  - Similarity matrices (between labeled/unlabeled and unlabeled/unlabeled data)  
  - Smooth loss function enforcing class consistency across similar nodes  

- **Optimization Algorithms**:  
  - Fixed Step Size GD (arbitrary and Lipschitz-derived)  
  - Armijo Line Search GD  
  - Block Coordinate GD (Gauss–Southwell rule)  
  - Coordinate Minimization (closed-form updates)

- **Evaluation Metrics**:  
  - Convergence rate (iterations, gradient norm)  
  - Loss function value trends  
  - Classification accuracy (on unlabeled data in real dataset)  
  - CPU runtime comparison

---

### How to Run

1. Clone the repository and open either `.ipynb` file in **Jupyter Notebook**  
2. Make sure you have the required Python packages installed:  
   `numpy`, `scipy`, `matplotlib`, `sklearn`, `pandas`  
3. Run all cells to reproduce synthetic experiments and real-data classification  

---

### References

- Breast Cancer Wisconsin (Diagnostic) dataset (UCI Machine Learning Repository)  
- Gradient descent and coordinate optimization techniques
