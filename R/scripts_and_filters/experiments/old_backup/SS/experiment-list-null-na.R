# y <- NA
y <- 50
# z <- ifelse(, y, NULL)
z <- switch(!is.na(y),
  y,
  NULL
)
z


# Create toy list with NULL/NA values -------------------------------------

m <- list("a1" = 10, "a2" = NULL, "a3" = NA, "a4" = 100, "a5" = NA, "a6" = NULL)
# Check if compact removes NULL values
purrr::compact(m)
# Answer: it doesn't, the a3, a5 value remains, only a2, a6 values is removed

# Now try to remove NA values after first removing the NULL values
purrr::compact(m) %>% purrr::discard(.x = ., .p = ~ is.na(x = .x))
# Answer: Yes, only a1, a4 values remain
