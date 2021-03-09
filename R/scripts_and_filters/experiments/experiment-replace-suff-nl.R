# Just an experiment to take a string e.g. "p.value.sand" and replace
# it with "p.value\nsand" i.e. the "sand" is moved to a new line below
# the "p.value"

library(tidyverse)
x <- c("statistic.sand", "p.value.sand")
x_suff <- x %>%
    stringr::str_split(string = ., pattern = "\\.") %>%
    purrr::map_chr(.x = ., ~utils::tail(x = .x, n = 1)) %>%
    base::unique(x = .)
x_suff
assertthat::assert_that(length(x = x_suff) == 1,
                        msg = glue::glue("The suffix must be unique, ",
                                         "it is currently: {x_suff}"))
x_excl_suff <- x %>% stringr::str_replace_all(string = .,
                                              pattern = glue::glue("\\.{x_suff}"),
                                              replacement = "")
x_excl_suff
x_mod <- x_excl_suff %>% stringr::str_c(x_suff, sep = "\n")
