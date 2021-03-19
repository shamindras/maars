## code to prepare `la-county` dataset goes here ----
# LA County source data url, use glue to split string in easy to read format
url_la_county <- glue::glue("http://www-stat.wharton.upenn.edu/~buja",
                            "STAT-961/Homeless_LA_by_Census_Tracts.csv",
                            .sep = "/")

# Let's read in the data to a tibble
# rename the MedianInc ($1000) to have no spaces
la_county <- vroom::vroom(file = url_la_county) %>%
    dplyr::rename("MedianInc1000" = "MedianInc ($1000)")

# Save the data in our package
usethis::use_data(la_county, overwrite = TRUE)
