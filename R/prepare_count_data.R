library(tidyverse)
library(malariaAtlas)

uga_anopheles <- getVecOcc(
  ISO = "UGA"
)

table(uga_anopheles$species_plain)

unique_spp <- unique(uga_anopheles$species_plain)

# NOTE TO SELF: INVESTIGATE AND REPORT ISSUE ON TRAILING SPACE
# FOR AN GAMBIAE RECORDS. GRRRRRRRR


gambiae_coords <- uga_anopheles |>
  filter(
    species_plain %in% c(
      "Anopheles gambiae",
      "Anopheles gambiae "
    )
  ) |>
  sf::st_coordinates() |>
  as_tibble()

write_csv(
  x = gambiae_coords,
  file = "data/gambiae_coords.csv"
)
