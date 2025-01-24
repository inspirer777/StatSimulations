# StatSimulations
# Statistical Simulation and Likelihood Estimation in R

This repository demonstrates the use of various statistical techniques in R, including **Acceptance-Rejection (AR) sampling** and **Likelihood Estimation** for different probability distributions. The code covers the following techniques:

1. **Acceptance-Rejection Sampling (AR)**
2. **Likelihood Estimation for Fisher, Poisson, and Gamma Distributions**
3. **Log-Likelihood Function Optimization**

The repository includes examples of various proposal distributions (Uniform, Normal, Student's t, Exponential) and compares the simulated results with true densities to validate the AR sampling method.

---

## 1. **Acceptance-Rejection Sampling Method**

### Overview:
The **Acceptance-Rejection (AR)** sampling method is used for simulating random variables from a target distribution using a proposal distribution. This repository demonstrates how AR sampling can be applied to simulate from **Beta(2,2)** and **Exponential distributions**.

### Features:
- **Target Distributions**: Beta(2,2) and Exponential (1/2 * exp(-|x|)).
- **Proposal Distributions**: Uniform, Normal, Student’s t, and Exponential.
- **Comparison Methods**: QQ-plots, histograms, and density curves are used to compare the simulated data against the true density functions.

### Key Concepts:
- **AR Sampling**: A statistical method used to generate random samples from a target distribution by utilizing a proposal distribution.
- **Comparison**: Visual methods such as QQ-plots and histograms to compare the simulated results with theoretical distributions.

---

## 2. **Likelihood Estimation for Fisher Distribution**

### Overview:
This section focuses on likelihood estimation for the **Fisher distribution** in R, with an emphasis on both **likelihood** and **log-likelihood** functions.

### Functions:
1. **Fisher Distribution Likelihood**:
   - `fisher(df1, y)`: Computes the likelihood for the Fisher distribution with degrees of freedom `df1` based on sample data `y`.
2. **Likelihood Without Loops**:
   - `lik.fisher1(df1, x)`: Computes the likelihood of the Fisher distribution efficiently using the `outer` function, avoiding explicit loops.
3. **Optimization**:
   - `optim()`: Used to find the parameter that maximizes the likelihood, applied to different sample sizes.
4. **Log-Likelihood**:
   - `loglik.fisher()`: Computes the log-likelihood by summing the log of the probability density function (PDF) for each sample.
5. **Log-Likelihood Without Loops**:
   - `Loglik.fisherl(df1, x)`: A more efficient approach using `outer` and `apply` to compute the log-likelihood.

### Visualization:
- Likelihood and log-likelihood functions are visualized as curves. The true parameter value is marked, and results are plotted for sample sizes ranging from 10 to 5000 to show how estimates improve as sample size increases.

---

## 3. **Poisson Distribution Likelihood Estimation**

### Overview:
This section demonstrates how to estimate the parameter for the **Poisson distribution** using likelihood and log-likelihood functions.

### 1. **Likelihood Function for Poisson Distribution**:
   - The Poisson likelihood is computed using the formula:
     \[
     f(x, \lambda) = \exp(-\lambda) \frac{\lambda^x}{x!}
     \]
   - The function `Pois(lambda, y)` calculates the likelihood for given values of `lambda` and observed data `y`.

### 2. **Log-Likelihood Function for Poisson Distribution**:
   - The log-likelihood function for the Poisson distribution is computed using:
     \[
     \log(f(x, \lambda)) = -\lambda + y \log(\lambda) - \log(x!)
     \]
   - The function `log.Pois(lambda, y)` calculates the log-likelihood based on the observed data.

### 3. **Visualization**:
   - Likelihood and log-likelihood curves are plotted to visualize how they vary with different values of \( \lambda \).

---

## 4. **Gamma Distribution Likelihood Estimation**

### Overview:
This section illustrates the likelihood and log-likelihood functions for the **Gamma distribution** with a fixed beta value (`beta = 1`).

### Functions:
1. **Gamma Likelihood**:
   - `L.Gamma(alpha)`: Computes the likelihood for the Gamma distribution with shape parameter `alpha` and fixed `beta = 1`.
2. **Gamma Log-Likelihood**:
   - `Log.Gamma(alpha)`: Computes the log-likelihood for the Gamma distribution, useful for numerical optimization.

### Optimization:
- The `optim()` function is used to find the parameter that maximizes the likelihood and log-likelihood functions for the Gamma distribution.

### Visualization:
- Both likelihood and log-likelihood curves are plotted for visual comparison.
- The true parameter value (`alpha = 10`) is marked on the plots, and the optimization result is overlaid for comparison.

---

## Installation and Usage

To use this repository, simply clone it and run the code within the R environment.

```bash
git clone https://github.com/yourusername/repositoryname.git
