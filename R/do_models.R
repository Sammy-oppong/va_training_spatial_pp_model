library(tidyverse)
#library(glm)
library(greta)
library(terra)


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
) + coord_cartesian(xlim = c(0, 50))

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



# we can calculate the value of objects, *conditional* on specific parameter
# values
calculate(
  lambda,
  values = list(
    count_intercept = 100,
    beta_rf = -1,
    beta_temp = 0.5
  )
)

# for random variables (count is poisson distributed) they will be randomly
# sampled
calculate(
  count,
  values = list(
    count_intercept = 100,
    beta_rf = -1,
    beta_temp = 0.5
  )
)

# we can even calculate values of objects, created later after model fitting
lambda2 <- lambda ^ 2

calculate(
  lambda2,
  values = list(
    count_intercept = 100,
    beta_rf = -1,
    beta_temp = 0.5
  )
)

# and compute posterior distributions for them, using the posterior sampels, and
# the greta arrays from the model

# plot MCMC traces of the first three lambdas, squared
lambda2_draws <- calculate(
  lambda2[1:3],
  values = draws
)

plot(lambda2_draws)

summary(lambda2_draws)

# we can use this to make predictions to new data

# first, we'll create a sequence of rainfall values, and predict the posterior
# mean and 95% CI abundance, *conditional* on temperature, being at its average
# value across Uganda (set to 0, because we standardised the covariates)


# make a sequence of values to predict to
new_rainfall <- seq(-3, 3, length.out = 100)
new_temperature <- 0

# create a corresponding vector of lambdas (as a greta array), using the same
# greta array objects we fitted to data
new_log_lambda <- log(count_intercept) + 
  beta_rf * new_rainfall +
  beta_temp * new_temperature

new_lambda <- exp(new_log_lambda)

# also create a vector of random variables representing single trap-hours
new_counts <- poisson(
  lambda = new_lambda
)

# generate lots of posterior samples of the expected abundance, and trap counts
# for each of these rainfall values
n_post_sims <- 10000

new_lambda_sims <- calculate(
  new_lambda,
  values = draws,
  nsim = n_post_sims
)

# compute the posterior mean from the samples
new_lambda_post_mean <- colMeans(new_lambda_sims[[1]])

# compute the posterior credible intervals from the samples (quantiles at 2.5%
# and 97.5% give us the bounds of the 95% credible interval)

# for one rainfall value
quantile(new_lambda_sims[[1]][, 1, 1], probs = c(0.025, 0.975))

# for all rainfall values
new_lambda_post_ci <- apply(new_lambda_sims[[1]], 2, quantile, c(0.025, 0.975))

# also get posterior simulations of observations. These can be used to calculate
# 'predictive intervals' - representing how likely it is to observe different
# counts
new_counts_sims <- calculate(
  new_counts,
  values = draws,
  nsim = n_post_sims
)

new_counts_post_mean <- colMeans(new_counts_sims[[1]])
quantile(new_counts_sims[[1]][, 1, 1], probs = c(0.025, 0.975))
new_counts_post_ci <- apply(new_counts_sims[[1]], 2, quantile, c(0.025, 0.975))

# we can plot this against the original sclae for rainfall, but
# back-transforming the modelled scale (standardised rainfall). Note that we
# need to get the mean and the scale parameter that were calculated when
# standardising the rasters in the first place

# For now pretend these were the true mean and standard deviation of the rainfall
# raster, before standardisation
rainfall_mean <- 1000
rainfall_sd <- 300

# invert/undo the standardisation - only on x axis being plotted
new_rainfall_mm <- new_rainfall * rainfall_sd + rainfall_mean

# plot mean and CI for lambda
plot(new_lambda_post_mean[, 1] ~ new_rainfall_mm,
     type = "l")
lines(new_lambda_post_ci[1, ] ~ new_rainfall_mm,
      lty = 2)
lines(new_lambda_post_ci[2, ] ~ new_rainfall_mm,
      lty = 2)

# plot predictive interval
lines(new_counts_post_ci[1, ] ~ new_rainfall_mm,
      lty = 3)
lines(new_counts_post_ci[2, ] ~ new_rainfall_mm,
      lty = 3)



# now we can use the same approach to make predictions to rasters

# make spatial posterior predictions using the standardised rasters (because
# that's what we fitted the model to)

# read in the rasters
rainfall <- rast(x = "data/rainfall_standardised.tif")
temperature <- rast(x = "data/temperature_standardised.tif")

# find an index to all the cells that have data
not_na_idx <- which(!is.nan(values(rainfall)))

# extract values into vectors
raster_rainfall_vec <- extract(rainfall, not_na_idx)
raster_temperature_vec <- extract(temperature, not_na_idx)

# convert to greta objects
raster_rainfall <- as_data(raster_rainfall_vec)
raster_temperature <- as_data(raster_temperature_vec)

raster_log_lambda <- log(count_intercept) + 
  beta_rf * raster_rainfall +
  beta_temp * raster_temperature

raster_lambda <- exp(raster_log_lambda)

raster_counts <- poisson(
  lambda = raster_lambda
)


# only 1000 posterior sims, as there are a lot of pixels, and only so much
# computer memory
n_post_sims <- 1000

raster_lambda_sims <- calculate(
  raster_lambda,
  values = draws,
  nsim = n_post_sims
)

# compute the posterior mean and variance from these posterior samples
raster_lambda_post_mean <- colMeans(raster_lambda_sims[[1]])
raster_lambda_post_var <- apply(raster_lambda_sims[[1]], 2, var)

# insert them into rasters
pred_rast <- rainfall * 0
names(pred_rast) <- "posterior_mean"
pred_rast[not_na_idx] <- raster_lambda_post_mean

var_rast <- rainfall * 0
names(var_rast) <- "posterior_variance"
var_rast[not_na_idx] <- raster_lambda_post_var

# plot these, alongside the covariates
plot(var_rast)
plot(pred_rast)
plot(rainfall)
plot(temperature)


# we can do other things with posteriors, such as plotting exceedance
# probabilities

# what's the probability of catching more that *target_count* mosquitoes in a
# single trap-hour
target_count <- 10

raster_counts
raster_counts_sims <- calculate(
  raster_counts,
  values = draws,
  nsim = n_post_sims
)

raster_counts_gt <- raster_counts_sims[[1]] > target_count
raster_counts_prob_gt <- colMeans(raster_counts_gt)

gt_rast <- rainfall * 0
names(gt_rast) <- "prob_greater_than_target"
gt_rast[not_na_idx] <- raster_counts_prob_gt

plot(gt_rast)
