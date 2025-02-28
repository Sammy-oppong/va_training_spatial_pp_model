# get data ready for model
library(tidyverse)
library(terra)
library(tidyterra)

# read in data
rainfall <- rast(x = "data/rainfall_standardised.tif")
temperature <- rast(x = "data/temperature_standardised.tif")

gambiae_coords <- read_csv(file = "data/gambiae_coords.csv")

rf_vals <- extract(
  x = rainfall,
  y = gambiae_coords
)

temp_vals <- extract(
  x = temperature,
  y = gambiae_coords
)

model_data <- bind_cols(
  gambiae_coords,
  rf_vals,
  temp_vals
) |>
  select(
    x = X,
    y = Y,
    rainfall = rf_st,
    temperature = temp_st
  )

model_data

model_data |>
  ggplot() +
  geom_point(
    aes(
      x = rainfall,
      y = temperature
    )
  )

# get sample values from background

# index of non-nan values
not_na_idx <- which(!is.nan(values(rainfall)))

sample_idx <- sample(
  x = not_na_idx,
  size = 10000
)

rf_sample <- values(rainfall)[sample_idx]
temp_sample <- values(temperature)[sample_idx]

sample_data <- tibble(
  type = "background",
  rainfall = rf_sample,
  temperature = temp_sample
)

sample_data |>
  ggplot() +
  geom_point(
    aes(
      x = rainfall,
      y = temperature
    )
  )


env_space_data <- bind_rows(
  sample_data,
  model_data |>
    mutate(
      type = "data"
    ) |>
    select(type, rainfall, temperature)
)

ggplot() +
  geom_point(
    data = env_space_data |>
      filter(type == "background"),
    aes(
      x = rainfall,
      y = temperature,
    ),
    colour = "blue"
  ) +
  geom_point(
    data = env_space_data |>
      filter(type == "data"),
    aes(
      x = rainfall,
      y = temperature,
    ),
    colour = "springgreen",
    size = 3
  )


# create data simulation function
sim_dat <- function(
    rainfall,
    temperature,
    count_intercept = 3,
    beta_rf = 0.7,
    beta_temp = 0.3
){
  
  

  loglam <- log(count_intercept) + 
    beta_rf*rainfall +
    beta_temp*temperature
  
  lam <- exp(loglam)
  
  count <- rpois(
    n = 1,
    lambda = lam
  )
  
}

# simulate some data based on environemntal space
# of our data and check results 
mapply(
  FUN = sim_dat,
  rainfall = rnorm(
    n = 1000,
    mean = mean(rf_sample),
    sd = sd(rf_sample)
  ),
  temperature = rnorm(
    n = 1000,
    mean = mean(temp_sample),
    sd = sd(temp_sample)
  ),
  MoreArgs = list(
    count_intercept = 3,
    beta_rf = 0.7,
    beta_temp = 0.3
  )
) |>
  hist()



gambiae_counts <- model_data |>
  rowwise() |>
  mutate(
    count = sim_dat(
      rainfall,
      temperature
    )
  )


write_csv(
  gambiae_counts,
  file = "data/gambiae_counts.csv"
)


