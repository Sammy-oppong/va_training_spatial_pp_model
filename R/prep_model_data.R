# get data ready for model

library(tidyverse)
library(terra)

# read in data
rainfall <- rast(x = "data/rainfall.tif")
temperature <- rast(x = "data/temperature.tif")

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
    rainfall,
    temperature
  )

model_data





