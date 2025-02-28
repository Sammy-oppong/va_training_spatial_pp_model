library(tidyverse)
#library(glm)
library(greta)


# read in data
gambiae_counts <- read_csv(
  file = "data/gambiae_counts.csv"
)

# greta model
# prepare data
count <- as_data(gambiae_counts$count)
rainfall <- as_data(gambiae_counts$rainfall)
temperature <- as_data(gambiae_counts$temperature)

# priors
count_intercept <- normal(
  mean = 20,
  sd = 40,
  truncation = c(0, Inf)
)

beta_rf <- normal(
  mean = 0,
  sd = 0.5
)

beta_temp <- normal(
  mean = 0,
  sd = 0.5
)

# likelihood
log_lambda <- log(count_intercept) + 
  beta_rf*rainfall +
  beta_temp*temperature

lambda <- exp(log_lambda)

distribution(count) <- poisson(
  lambda = lambda
)

# model

m <- model(
  count_intercept,
  beta_rf,
  beta_temp
)

plot(m)

# prior predictive checks
prior_sims <- calculate(
  count,
  nsim = 1000
)

dim(prior_sims$count)
which_row <- 99

data_point_prior_sims <- prior_sims$count[,which_row,1]
data_point_truth <- gambiae_counts$count[which_row]
mean(data_point_prior_sims < data_point_truth)

hist(data_point_prior_sims, breaks = 100)
abline(v = data_point_truth)


bayesplot::ppc_dens_overlay(
  y = gambiae_counts$count,
  yrep = prior_sims$count[,,1]
)


bayesplot::ppc_ecdf_overlay(
  y = gambiae_counts$count,
  yrep = prior_sims$count[,,1]
)

## fit model

draws <- mcmc(
  model = m,
  n_samples = 1000,
  warmup = 1000,
  chains = 100
)

bayesplot::mcmc_trace(draws)
coda::gelman.diag(draws, autoburnin = FALSE)


summary(draws)
