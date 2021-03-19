# Setup
library(tidyverse)

# Fit the well-specified model on a subset of data
mtcars_slice <- mtcars %>% dplyr::slice(1:10)
lm_fit <- lm(formula = mpg ~ cyl + disp + hp, data = mtcars_slice)

# This produces an object of class "lm"
class(lm_fit)

# This comes built in with nice generic methods
print(lm_fit)
summary(lm_fit)
predict(lm_fit)
confint(lm_fit)

# Or the more modern workflow
broom::tidy(x = lm_fit)
broom::glance(x = lm_fit)
broom::augment(x = lm_fit)

# Setup - install required libraries
# install.packages(c("pkgdown", "here"))

# If you are in your local package directory, run the following
# to get the required package metadata
pkg <- pkgdown::as_pkgdown(pkg = here::here())

# Inspect the topics object, which contains function metadata
pkg$topics %>% dplyr::glimpse()

# Get list of all functions and just required metadata
pkg_fns_all <- pkg$topics %>%
    dplyr::select(name, file_in, internal)

# Get the non-internal functions, acccessed using pkgname::function
pkg_fns_user <- pkg_fns_all %>% dplyr::filter(!internal)

# Get the internal functions, acccessed using pkgname:::function
pkg_fns_internal <- pkg_fns_all %>% dplyr::filter(internal)
