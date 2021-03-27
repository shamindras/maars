set.seed(455343)

# Read in the (corrected) Boston Housing dataset from our package
boston_df <- boston_housing

test_that("Boston Housing data dimensions are as expected", {
  testthat::expect_equal(nrow(boston_df), 506)
  testthat::expect_equal(ncol(boston_df), 21)
})
