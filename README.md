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



# Gamma Distribution: Likelihood and Log-Likelihood

This repository demonstrates the computation and visualization of the **Likelihood** and **Log-Likelihood** functions for the Gamma Distribution. It also includes parameter estimation using optimization techniques.

---

## Gamma Distribution Formula

The probability density function for the Gamma distribution is:

\[
f(x) = \frac{1}{\beta^\alpha \Gamma(\alpha)} x^{\alpha-1} e^{-x / \beta}
\]

Where:
- \( \alpha \): Shape parameter
- \( \beta \): Scale parameter (fixed at \( \beta = 1 \) in this example)

The **Log-Likelihood** is given by:
\[
\log(f(x)) = -\alpha \log(\beta) - \log(\Gamma(\alpha)) + (\alpha - 1) \log(x) - \frac{x}{\beta}
\]

---

## Features

1. **Likelihood Function**:
   - Computes the likelihood of the Gamma distribution for different values of \( \alpha \).
   - Plots the likelihood curve.

2. **Log-Likelihood Function**:
   - Computes the log-likelihood for different values of \( \alpha \).
   - Plots the log-likelihood curve.

3. **Optimization**:
   - Estimates the parameter \( \alpha \) that maximizes the likelihood or log-likelihood using various optimization methods.

4. **Visualization**:
   - Side-by-side comparison of Likelihood and Log-Likelihood plots.
   - Highlights the real \( \alpha \) and estimated \( \hat{\alpha} \) values.

---

## Code

### Likelihood Function
```r
L.Gamma <- function(alpha) {
  beta <- 1
  n <- 100
  set.seed(143)
  y <- rgamma(n, 10, 1) # Real alpha = 10
  f <- 0
  for (i in 1:length(alpha)) {
    f[i] <- prod(1 / (beta^alpha[i] * gamma(alpha[i])) * y^(alpha[i] - 1) * exp(-y / beta))
  }
  return(f)
}

alpha <- c(1, 4, 7)
L.Gamma(alpha)

curve(L.Gamma(x), 1, 20)
```


# Class 9: Two Parameters - Computing Likelihood for Gamma Distribution

This code computes the **log-likelihood** for the **Gamma distribution** with two parameters: \( \alpha \) (shape) and \( \beta \) (scale). It also demonstrates plotting the likelihood surfaces and optimizes the parameters using different methods.

## 1. **Log-Likelihood of Gamma Distribution**

The log-likelihood for a Gamma distribution is given by:

\[
\log f(x) = -\alpha \cdot \log(\beta) - \log(\Gamma(\alpha)) + (\alpha - 1) \cdot \log(x) - \frac{x}{\beta}
\]

The `Log.Gamma2` function calculates the log-likelihood for given values of \( \alpha \) and \( \beta \).

```r
Log.Gamma2 <- function(alpha, beta) {
  n = 100
  set.seed(143)
  y <- rgamma(n, 10, 1) 
  f <- matrix(0, ncol = length(beta), nrow = length(alpha))
  for (i in 1:length(alpha)) {
    for (j in 1:length(beta)) {
      f[i, j] <- sum(-alpha[i] * log(beta[j]) - log(gamma(alpha[i])) + (alpha[i] - 1) * log(y) - y / beta[j])
    }
  }
  return(f)
}
```
# Bernoulli Distribution Likelihood Calculation

This R code demonstrates how to compute the likelihood of a **Bernoulli distribution** given a set of parameters and data.

## 1. **Simulate Data**

```r
set.seed(143)
n = 30
y <- rbinom(n, 1, .5)  # Generate 30 random values from a Bernoulli distribution with p = 0.5
y  # Display the simulated data
```
# Beta Distribution Likelihood and Log-Likelihood

This R code demonstrates how to compute the likelihood and log-likelihood for the **Beta distribution** using different values of the parameter \( b \).

## 1. **Simulate Data**

```r
set.seed(143)
y <- rbeta(3, 0.1, 0.2)  # Simulate 3 random values from a Beta distribution with alpha = 0.1 and beta = 0.2
y  # Display the simulated data
```
# Distribution Likelihoods and Plotting

This script computes the likelihood for various probability distributions using different functions, and visualizes the likelihood curves. The distributions covered are **Exponential**, **Binomial**, **Chi-Square**, **Beta**, **Cauchy**, and **Uniform**. 

## 1. **Exponential Distribution**

The likelihood function for the **Exponential distribution** is calculated based on the parameter \( \lambda \).

### Exponential Likelihood Function

```r
Exp <- function(lambda, y) {
  set.seed(143)
  f = 0
  y <- rexp(10, 4)  # Generate 10 random values from an exponential distribution with rate = 4

  for (i in 1:length(lambda)) {
    f[i] <- prod(lambda[i] * exp(-lambda[i] * y))  # Calculate likelihood for each lambda
  }
  return(f)
}
```

# Likelihood Function Example

### Problem:
We are tasked with creating likelihood functions using 15 random samples. The given vector `x` contains the following values:

```r
x <- c(0.41, 0.91, -0.61, 0.38, 0.37, 0.36, 0.01, -0.28, -0.33, 0.99, -0.35, 0.1, 0.31, 0.75, -0.34)
```





## Installation and Usage

To use this repository, simply clone it and run the code within the R environment.

```bash
git clone https://github.com/yourusername/repositoryname.git
