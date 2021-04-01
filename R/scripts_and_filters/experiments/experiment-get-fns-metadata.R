# Setup - install required libraries
# install.packages(c("pkgdown", "here"))

# If you are in your local package directory, run the following
# to get the required package metadata
pkg <- pkgdown::as_pkgdown(pkg = here::here())

# This doesn't work for CRAN packages. TODO: check why
# pkg <- pkgdown::as_pkgdown(pkg = "/Library/Frameworks/R.framework/Resources/library/dplyr")

# Inspect the topics object, which contains function metadata
pkg$topics %>% dplyr::glimpse()

# Get list of all functions and just required metadata
pkg_fns_all <- pkg$topics %>%
    dplyr::select(name, file_in, internal)

# Get the non-internal functions, acccessed using pkgname::function
pkg_fns_user <- pkg_fns_all %>% dplyr::filter(!internal)

# Get the internal functions, acccessed using pkgname:::function
pkg_fns_internal <- pkg_fns_all %>% dplyr::filter(internal)
