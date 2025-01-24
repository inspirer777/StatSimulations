# StatSimulations: Acceptance-Rejection Sampling and Likelihood Estimation in R

This repository provides an implementation of various statistical techniques using **Acceptance-Rejection (AR) sampling**, **Likelihood estimation**, and **Log-Likelihood estimation** in R. It demonstrates methods for simulating random variables, estimating parameters, and visualizing results.

---

## Acceptance-Rejection (AR) Sampling Method

This section explores the **Acceptance-Rejection (AR) sampling** method in R, allowing users to simulate random variables from different target distributions. It provides a hands-on approach to understanding and validating the AR technique.

### Features
1. **Simulated Distributions**:
   - Beta(2,2) Distribution.
   - Exponential Distribution: \( \frac{1}{2} \exp(-|x|) \).

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
   - Simulated results are compared with the true densities to validate the AR sampling method.

---

## Fisher Distribution: Likelihood and Log-Likelihood in R

This section demonstrates several statistical techniques related to the **Fisher distribution**, focusing on **likelihood estimation** and **log-likelihood** functions for parameter estimation.

### Features
1. **Fisher Distribution Likelihood**:
   - Computes the likelihood of the Fisher distribution with degrees of freedom `df1` based on sample data `y`.

2. **Likelihood Without Loops**:
   - Uses the `outer` function to compute the likelihood without explicit loops for efficiency.

3. **Optimization**:
   - Utilizes the `optim()` function to find the parameter that maximizes the likelihood, estimating the best-fitting degree of freedom for different sample sizes.

4. **Log-Likelihood**:
   - Computes the log-likelihood by summing the log of the PDF for each sample in `y` for each value of `df1`.

5. **Visualization**:
   - Likelihood and log-likelihood functions are plotted, with vertical lines marking the true parameter values. Sample sizes range from `10` to `5000` to observe how estimates improve as sample size increases.

---

## Poisson Distribution: Likelihood and Log-Likelihood in R

This section covers the **likelihood** and **log-likelihood** functions for the **Poisson distribution**, demonstrating their implementation, visualization, and use in maximum likelihood estimation (MLE).

### Features
1. **Likelihood Function**:
   - Computes the likelihood for a range of \( \lambda \) values.
   - Evaluates the Poisson probability for sample data.

2. **Log-Likelihood Function**:
   - Transforms the likelihood into log-likelihood for easier optimization.

3. **Visualization**:
   - Likelihood and log-likelihood curves are plotted side-by-side to illustrate the behavior of the Poisson distribution’s parameters.

---

## Gamma Distribution: Likelihood and Log-Likelihood in R

This section demonstrates the **Likelihood** and **Log-Likelihood** functions for the **Gamma distribution** with an example of parameter estimation using optimization techniques.

### Gamma Distribution Formula
- PDF: 
  \[
  f(x) = \frac{1}{\beta^\alpha \Gamma(\alpha)} x^{\alpha-1} e^{-x / \beta}
  \]
- Log-Likelihood:
  \[
  \log(f(x)) = -\alpha \log(\beta) - \log(\Gamma(\alpha)) + (\alpha - 1) \log(x) - \frac{x}{\beta}
  \]

### Features
1. **Likelihood Function**:
   - Computes the likelihood for different values of \( \alpha \) and plots the likelihood curve.

2. **Log-Likelihood Function**:
   - Computes the log-likelihood and plots the log-likelihood curve.

3. **Optimization**:
   - Estimation of \( \alpha \) that maximizes the likelihood or log-likelihood through various optimization methods.

4. **Visualization**:
   - Side-by-side comparison of Likelihood and Log-Likelihood plots, highlighting real and estimated values of \( \alpha \).

---

## Code Examples

### Poisson Distribution Likelihood Function:
```r
# Likelihood function for Poisson distribution
Pois <- function(lambda, y) {
  sapply(lambda, function(l) prod(dpois(y, lambda = l)))
}

# Example usage
lambda <- c(1, 2, 3)
y <- rpois(10, 4)  # Generate sample data
Pois(lambda, y)    # Compute likelihood for lambda = 1, 2, 3
```
### Gamma Distribution Likelihood:
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
### Bernoulli Distribution:
```r
set.seed(143)
n = 30
y <- rbinom(n, 1, .5)  # Simulate data from a Bernoulli distribution

```

### Exponential Likelihood Function:
```r
Exp <- function(lambda, y) {
  set.seed(143)
  f = 0
  y <- rexp(10, 4)  # Generate sample data from an exponential distribution with rate = 4
  for (i in 1:length(lambda)) {
    f[i] <- prod(lambda[i] * exp(-lambda[i] * y))  # Compute likelihood
  }
  return(f)
}

```

### Installation and Usage
To use this repository, clone it and run the code in an R environment : 
```bash git clone https://github.com/inspirer777/StatSimulations.git

```

