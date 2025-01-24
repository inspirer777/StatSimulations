# StatSimulations
# Acceptance-Rejection (AR) Sampling Method in R

This repository demonstrates the use of the **Acceptance-Rejection (AR) sampling method** in R to simulate random variables from different target distributions. It provides a hands-on approach to understanding and validating the AR sampling technique.

---

## Features

1. **Simulated Distributions**:
   - **Beta(2,2) Distribution**.
   - **Exponential Distribution**: \( \frac{1}{2} \exp(-|x|) \).

2. **Proposal Distributions**:
   - Uniform.
   - Normal.
   - Student’s t.
   - Exponential.

3. **Validation Methods**:
   - QQ-plots.
   - Histograms.
   - Density curves.

4. **Comparison**:
   - Simulated results are compared with the true densities to validate the sampling method.

---# Acceptance-Rejection (AR) Sampling Method in R

This repository demonstrates the use of the **Acceptance-Rejection (AR) sampling method** in R to simulate random variables from different target distributions. It provides a hands-on approach to understanding and validating the AR sampling technique.

---

## Features

1. **Simulated Distributions**:
   - **Beta(2,2) Distribution**.
   - **Exponential Distribution**: \( \frac{1}{2} \exp(-|x|) \).

2. **Proposal Distributions**:
   - Uniform.
   - Normal.
   - Student’s t.
   - Exponential.

3. **Validation Methods**:
   - QQ-plots.
   - Histograms.
   - Density curves.

4. **Comparison**:
   - Simulated results are compared with the true densities to validate the sampling method.

---


# Fisher Distribution: Likelihood and Log-Likelihood in R

This repository demonstrates several statistical techniques related to the **Fisher distribution** in R, focusing on **likelihood estimation** and **log-likelihood functions** for parameter estimation.

---

## Features

### 1. **Fisher Distribution Likelihood**
- The function `fisher(df1, y)` computes the likelihood for the Fisher distribution with degrees of freedom `df1` based on sample data `y`.
- It multiplies the probability density function (PDF) for each `y` across the different `df1` values.

### 2. **Likelihood Without Loops**
- The function `lik.fisher1(df1, x)` calculates the likelihood of the Fisher distribution using the `outer` function, which avoids explicit loops. 
- This method computes the product of the PDF for each sample across the `df1` values and returns the result.

### 3. **Optimization**
- The `optim()` function is used to find the parameter that maximizes the likelihood.
- For different sample sizes (`n = 10, 30, 100, 500`), the function minimizes the negative log-likelihood to estimate the best-fitting parameter (degree of freedom for the Fisher distribution).

### 4. **Log-Likelihood**
- The function `loglik.fisher()` computes the log-likelihood of the Fisher distribution.
- It calculates the sum of the log of the PDF for each sample in `y` for each value in `df1`.

### 5. **Log-Likelihood Without Loops**
- The `Loglik.fisherl(df1, x)` function is a more efficient approach, utilizing `outer` and `apply` to calculate the log-likelihood without explicit loops.

### 6. **Visualization**
- Likelihood and log-likelihood functions are plotted as curves, and the true parameter value is marked with vertical lines.
- Sample sizes range from `10` to `5000` to observe how estimates improve as the sample size increases.

---

# Poisson Distribution: Likelihood and Log-Likelihood in R

This repository showcases the **likelihood** and **log-likelihood** functions for the **Poisson distribution**, providing examples of their implementation, visualization, and use in maximum likelihood estimation (MLE).

---

## Overview

### Likelihood Function
The likelihood function for the Poisson distribution is given by:
\[
f(x, \lambda) = \exp(-\lambda) \frac{\lambda^x}{x!}
\]
Where:
- \( x \): Observed data.
- \( \lambda \): Rate parameter.
- \( x! \): Factorial of \( x \).

### Log-Likelihood Function
The log-likelihood function transforms the likelihood for easier optimization:
\[
\log(f(x, \lambda)) = -\lambda + y \log(\lambda) - \log(x!)
\]

---

## Features

1. **Likelihood Function (`Pois`)**:
   - Computes the likelihood for a range of \( \lambda \) values.
   - Iterates through sample data to evaluate the Poisson probability.

2. **Log-Likelihood Function (`log.Pois`)**:
   - Calculates the log of the likelihood for numerical optimization.
   - Aggregates the log-transformed probabilities for the observed data.

3. **Visualization**:
   - Likelihood and log-likelihood curves are plotted side-by-side to illustrate parameter behavior.

---

## Code Examples

### Likelihood Function
The `Pois` function computes the likelihood for specified \( \lambda \) values:

```r
# Likelihood function
Pois <- function(lambda, y) {
  sapply(lambda, function(l) prod(dpois(y, lambda = l)))
}

# Example usage
lambda <- c(1, 2, 3)
y <- rpois(10, 4)  # Generate sample data
Pois(lambda, y)    # Compute likelihood for lambda = 1, 2, 3

```



## Installation and Usage

To use this repository, simply clone it and run the code within the R environment.

```bash
git clone https://github.com/yourusername/repositoryname.git
