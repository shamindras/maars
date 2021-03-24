# Setup libraries ----
library(tidyverse)

# Read in the raw boston housing dataset ----
# Need to skip header rows
boston_raw <- readr::read_table("http://lib.stat.cmu.edu/datasets/boston",
                                skip = 21,
                                col_names = FALSE) %>%
    # TODO: Change this to only sum columns X4:X11, not across all columns,
    #       since these are the only columns that should be NA
    mutate(num_nas = rowSums(is.na(.)))

# Split the raw boston housing into 2 datasets ----
# This step is needed since the original source above split each observation
# across 2 lines! The first line contains the first 11 features and the
# second line contains the remaining 3 features. This is clear because the
# second line has 8 missing (i.e. NA) feature columns
# Get the split of the first line for each observation
boston_raw_11_cols <- boston_raw %>%
    dplyr::filter(.data = ., num_nas != 8) %>%
    dplyr::select(-num_nas)
# Get the split
boston_raw_3_cols <- boston_raw %>%
    dplyr::filter(.data = ., num_nas == 8) %>%
    dplyr::select(X1:X3) %>%
    dplyr::rename(X12 = X1, X13 = X2, X14 = X3)

# Create final boston housing dataset ----
# Column bind the 2 datasets and rename the 14 feature columns
boston_housing <- dplyr::bind_cols(boston_raw_11_cols,
                                   boston_raw_3_cols) %>%
    tibble::remove_rownames(.data = .) %>%
    purrr::set_names(x = ., nm = c("crim", "zn", "indus", "chas", "nox",
                                   "rm", "age", "dis", "rad", "tax",
                                   "ptratio", "b", "lstat", "medv"))

# Save the data in our package
usethis::use_data(boston_housing, overwrite = TRUE)
