# Setup libraries ----
library(tidyverse)

# Read in the raw boston housing dataset ----
# Need to skip header rows
boston_raw <- readr::read_tsv(
  glue::glue("http://lib.stat.cmu.edu",
    "datasets/boston_corrected.txt",
    .sep = "/"
  ),
  skip = 9,
  col_types = cols(
    .default = readr::col_double(),
    TOWN = readr::col_factor(),
    TRACT = readr::col_factor()
  ),
  col_names = TRUE
  ) %>%
  dplyr::rename(
    .data = .,
    "obs" = .data$OBS.,
    "town" = .data$TOWN,
    "town_no" = .data$`TOWN#`,
    "tract" = .data$TRACT,
    "lon" = .data$LON,
    "lat" = .data$LAT,
    "medv" = .data$MEDV,
    "cmedv" = .data$CMEDV,
    "crim" = .data$CRIM,
    "zn" = .data$ZN,
    "indus" = .data$INDUS,
    "chas" = .data$CHAS,
    "nox" = .data$NOX,
    "rm" = .data$RM,
    "age" = .data$AGE,
    "dis" = .data$DIS,
    "rad" = .data$RAD,
    "tax" = .data$TAX,
    "ptratio" = .data$PTRATIO,
    "b" = .data$B,
    "lstat" = .data$LSTAT
  )

# Save the data in our package
usethis::use_data(boston_housing, overwrite = TRUE)
