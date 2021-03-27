set.seed(455343)

# Read in the (corrected) Boston Housing dataset from our package
la_county_df <- la_county

test_that("La County data dimensions are as expected", {
    testthat::expect_equal(nrow(la_county_df), 505)
    testthat::expect_equal(ncol(la_county_df), 7)
})
