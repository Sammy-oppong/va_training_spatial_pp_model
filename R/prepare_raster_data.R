install.packages("malariaAtlas")

library(tidyverse)
library(terra)
library(geodata)
library(tidyterra)

# get vector boundary
uga_vector <- gadm(
  country = "UGA",
  level = 0,
  path = "data/downloads"
) # slow to run though not large

uga_vector

plot(uga_vector)

# download climate data
bioclim_uga <- worldclim_country(
  country = "UGA",
  var = "bio",
  res = 0.5,
  path = "data/downloads"
)

# get variables of interest
uga_annual_mean_temp <- bioclim_uga[["wc2.1_30s_bio_1"]]

uga_annual_rainfall <- bioclim_uga$wc2.1_30s_bio_12


# add sensible layer names
names(uga_annual_mean_temp) <- "temperature"

names(uga_annual_rainfall) <- "rainfall"

# mask to area of interest
temperature <- uga_annual_mean_temp |>
  mask(mask = uga_vector)

rainfall <- uga_annual_rainfall |>
  mask(mask = uga_vector)


temperature_standardised <- scale(
  x = temperature
)

rainfall_standardised <- scale(
  x = rainfall
)

names(temperature_standardised) <- "temp_st"
names(rainfall_standardised) <- "rf_st"

writeRaster(
  x = temperature,
  filename = "data/temperature.tif",
  overwrite = TRUE
)

writeRaster(
  x = rainfall,
  filename = "data/rainfall.tif",
  overwrite = TRUE
)


writeRaster(
  x = temperature_standardised,
  filename = "data/temperature_standardised.tif",
  overwrite = TRUE
)

writeRaster(
  x = rainfall_standardised,
  filename = "data/rainfall_standardised.tif",
  overwrite = TRUE
)
