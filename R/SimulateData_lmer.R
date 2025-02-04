# Load necessary libraries
library(dplyr)

# Set seed for reproducibility
set.seed(123)

# Define parameters
num_clusters <- 100        # Number of clusters
obs_per_cluster <- 10      # Observations per cluster
total_obs <- num_clusters * obs_per_cluster

# Fixed effect coefficients
beta <- c(intercept = 2,    # Fixed intercept
          X1 = 1.5,         # Coefficient for X1
          X2 = -2,          # Coefficient for X2
          X3 = 0.5)         # Coefficient for X3

# Variance components
sigma_intercept <- 1       # Std dev of random intercepts
sigma_slope <- 0.5         # Std dev of random slopes (for Dataset 2)
rho <- 0.3                  # Correlation between intercept and slope (for Dataset 2)
sigma_residual <- 1        # Std dev of residuals

# Create a data frame with cluster IDs
data <- data.frame(
  cluster = rep(1:num_clusters, each = obs_per_cluster)
)

# Simulate covariates
data <- data %>%
  mutate(
    X1 = rnorm(total_obs, mean = 0, sd = 1),
    X2 = rnorm(total_obs, mean = 0, sd = 1),
    X3 = rnorm(total_obs, mean = 0, sd = 1)
  )

### Dataset 1: Random Intercept Model ###

# Simulate random intercepts for each cluster
random_intercepts <- rnorm(num_clusters, mean = 0, sd = sigma_intercept)

# Assign random intercepts to observations
data$random_intercept <- random_intercepts[data$cluster]

# Generate residual errors
data$residual <- rnorm(total_obs, mean = 0, sd = sigma_residual)

# Compute response variable Y for Dataset 1
data$Y_random_intercept <- beta['intercept'] +
  beta['X1'] * data$X1 +
  beta['X2'] * data$X2 +
  beta['X3'] * data$X3 +
  data$random_intercept +
  data$residual

# Create Dataset 1
dataset1 <- data %>%
  select(cluster, X1, X2, X3, Y = Y_random_intercept)

### Dataset 2: Random Intercept and Random Slope Model ###

# Simulate random intercepts and slopes with specified correlation
library(MASS)

# Define the covariance matrix
cov_matrix <- matrix(c(sigma_intercept^2, rho * sigma_intercept * sigma_slope,
                       rho * sigma_intercept * sigma_slope, sigma_slope^2),
                     nrow = 2, byrow = TRUE)

# Simulate random intercepts and slopes
random_effects <- mvrnorm(n = num_clusters, mu = c(0, 0), Sigma = cov_matrix)

# Assign random intercepts and slopes to observations
data$random_intercept_rs <- random_effects[data$cluster, 1]
data$random_slope_rs <- random_effects[data$cluster, 2]

# Generate residual errors (new residuals for Dataset 2)
data$residual_rs <- rnorm(total_obs, mean = 0, sd = sigma_residual)

# Compute response variable Y for Dataset 2
# Assuming random slope for X1; modify as needed for other covariates
data$Y_random_slope <- beta['intercept'] +
  (beta['X1'] + data$random_slope_rs) * data$X1 +
  beta['X2'] * data$X2 +
  beta['X3'] * data$X3 +
  data$random_intercept_rs +
  data$residual_rs

# Create Dataset 2
dataset2 <- data %>%
  select(cluster, X1, X2, X3, Y = Y_random_slope)

### Summary ###

# Display the first few rows of each dataset
head(dataset1)
head(dataset2)

# Optionally, save datasets to CSV files
# write.csv(dataset1, "dataset_random_intercept.csv", row.names = FALSE)
# write.csv(dataset2, "dataset_random_intercept_slope.csv", row.names = FALSE)
