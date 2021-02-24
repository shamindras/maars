# Assertion checking for lists using purrr ----
library(rlang)
library(tidyverse)

#' Checks a list contains only positive integers, and contains elements with
#' specified names
#'
#' @param var_name
#' @param lst_names
#'
#' @return
#'
#' @keywords internal
#'
#' @examples
chk_args_lst_posint <- function(var_name, lst_names) {
  assertthat::assert_that(all(
    is.character(var_name),
    length(var_name) == 1
  ),
  msg = glue::glue("var_name must be a character vector of length 1")
  )

  # Parse the variable value from the variable name
  var_val <- rlang::eval_tidy(rlang::parse_expr(x = var_name))

  assertthat::assert_that(is.list(var_val),
    msg = glue::glue("{var_name} must be the name of a list")
  )
  assertthat::assert_that(length(var_val) == length(lst_names),
    msg = glue::glue("{var_name} must be the name of list",
      "with the same number length as lst_names",
      .sep = " "
    )
  )
  assertthat::assert_that(all(sort(names(var_val)) == sort(lst_names)),
    msg = glue::glue("{var_name} list must only contain the ",
      "names {sort(lst_names)}",
      .sep = " "
    )
  )

  lst_names %>%
    purrr::walk(.x = ., .f = ~ assertthat::assert_that(all(
      is.numeric(var_val[[.x]]),
      var_val[[.x]] == as.integer(var_val[[.x]]),
      as.integer(var_val[[.x]]) > 0
    ),
    msg = glue::glue("{.x} must be a positive integer",
      "it is currently {var_val[[.x]]}",
      .sep = " "
    )
    ))

  # B <- as.integer(B)
  # m <- as.integer(m)
}

boot_emp <- list(B = 0, m = 100)
chk_args_lst_posint(var_name = "boot_emp", lst_names = c("B", "m"))

boot_emp <- list(B = 1, m = 100)
chk_args_lst_posint(var_name = "boot_emp", lst_names = c("B", "m"))

boot_emp <- list(B = 200, m = 1.5)
chk_args_lst_posint(var_name = "boot_emp", lst_names = c("B", "m"))

# Assertion checking for comp_var ----

# list(B = 10, m = 100) # valid
# list(B = 10, m = NULL) # valid
# list(B = 10) # valid, but need to set m = NULL
# list(B = 0, m = 100) # invalid due to B = 0
# list(B = 10, m = 0) # invalid due to m = 0
# list(B = 10, m = -1) # invalid due to m = -1
# list(B = -10, m = -1) # invalid due to B = -10 and m = -1
# list(B = 200, m = 400, z = 2300) # invalid due to extra z parameter

# Check if input is a list of values (assume we have checked non-NULL)
# -> If FALSE, then error and explain that it must be a list
# -> -> If true, check that it is not empty. If it is, error
#       if(length(inp_list) == 0)
# If it is not empty, first check if there are more than 2 variables
# -> error
# Next check if there are exactly 2 variables
# -> Check that they are named "B" and "m", else error
# -> -> If TRUE, then check that they are both positive integers
# else
# -> -> -> Set B = as.integer(B), m = as.integer(m)
# If it is just one, then it must be "B"
# -> if FALSE, then error
# -> -> if TRUE then check it is positive integer
# -> -> -> Set B = as.integer(B), m = NULL

# inp_list <- list() # Error, empty list
# inp_list <- c("B" = 10, "m" = 100) # FALSE, passing in "numeric" vector not list
# inp_list <- list(B = 200, m = 400, z = 2300) # FALSE, list has more than 2 elements
# inp_list <- list(B = 10, m = 100) # TRUE, valid inputs
# inp_list <- list("B" = 10) # TRUE, named element "B"
# inp_list <- list(B = 10) # TRUE, named element "B", without quotes
# inp_list <- list(B = 10) # TRUE, named element "B", without quotes
# inp_list <- list(B = 0) # FALSE, B must be a positive integer
# inp_list <- list(B = 0.2) # FALSE, B must be a positive integer
inp_list <- list(B = 1) # TRUE, B is a positive integer
# inp_nms_chk <- c("B", "m")
# inp_list <- list("B" = 10)
assertthat::assert_that(is.list(inp_list)
                        && length(inp_list) > 0
                        && length(purrr::compact(.x = inp_list)) > 0,
                        msg = glue::glue("Input must be a non-empty non-NULL list"))
assertthat::assert_that(length(inp_list) <= 2,
                        msg = glue::glue("Input list must have 1 or 2 elements",
                                         "currently it has",
                                         "{length(inp_list)} items",
                                         .sep = " "))
if (length(inp_list) == 1 || length(purrr::compact(.x = inp_list)) == 1) {
  assertthat::assert_that(all(names(inp_list) == c("B")),
                          msg = glue::glue("inp_list must only contain the ",
                                           "named element: B",
                                           .sep = " "
                          )
  )
  out_list <- list(B = inp_list[["B"]], m = NULL)
}

if (length(inp_list) == 2) {
  assertthat::assert_that(all(sort(names(inp_list)) == sort(c("B", "m"))),
                          msg = glue::glue("{var_name} list must only contain the ",
                                           "names ['B', 'm']",
                                           .sep = " "
                          )
  )
  out_list <- list(B = inp_list[["B"]], m = inp_list[["m"]])
}

check_fn_args_boot_emp <- function(inp_list) {
  assertthat::assert_that(
    is.list(inp_list)
    && length(inp_list) > 0
    && length(purrr::compact(.x = inp_list)) > 0,
    msg = glue::glue("Input must be a non-empty non-NULL list")
  )

  assertthat::assert_that(
    length(inp_list) <= 2,
    msg = glue::glue("Input list must have 1 or 2 elements",
                     "currently it has",
                     "{length(inp_list)} items",
                     .sep = " "
    )
  )

  if (length(inp_list) == 1 || length(purrr::compact(.x = inp_list)) == 1) {
    assertthat::assert_that(
      all(names(purrr::compact(.x = inp_list)) == c("B")),
      msg = glue::glue("inp_list must only contain the ",
                       "named element: B",
                       .sep = " "
      )
    )
  } else if (length(inp_list) == 2) {
    assertthat::assert_that(all(sort(names(inp_list)) == sort(c("B", "m"))),
                            msg = glue::glue("{var_name} list must only contain the ",
                                             "names ['B', 'm']",
                                             .sep = " "
                            )
    )
  }
}

# valid since the named arguments are B, m
testthat::expect_true(check_fn_args_boot_emp(inp_list = list(B = 10, m = 100)))
# valid since the named arguments are B, m with m = NULL
testthat::expect_true(check_fn_args_boot_emp(inp_list = list(B = 10, m = NULL)))
# valid since the named arguments are B only, so m = NULL here by assumption
testthat::expect_true(check_fn_args_boot_emp(inp_list = list(B = 10)))
# valid since the named arguments are B, m
testthat::expect_true(check_fn_args_boot_emp(inp_list = list(B = 0, m = 100)))
# valid since the named arguments are B, m
testthat::expect_true(check_fn_args_boot_emp(inp_list = list(B = 10, m = 0)))
# valid since the named arguments are B, m
testthat::expect_true(check_fn_args_boot_emp(inp_list = list(B = 10, m = -1)))
# valid since the named arguments are B, m
testthat::expect_true(check_fn_args_boot_emp(inp_list = list(B = -10, m = -1)))
# invalid due to extra z parameter
testthat::expect_error(check_fn_args_boot_emp(inp_list = list(B = 200, m = 400, z = 2300)))
# invalid since the named arguments are b, m i.e. not B, m
testthat::expect_error(check_fn_args_boot_emp(inp_list = list(b = 200, m = 400)))
# invalid since the named arguments are b, M i.e. not B, m
testthat::expect_error(check_fn_args_boot_emp(inp_list = list(b = 200, M = 400)))
# invalid since the named arguments are B, M i.e. not B, m
testthat::expect_error(check_fn_args_boot_emp(inp_list = list(B = 200, M = 400)))
testthat::expect_error(check_fn_args_boot_emp(inp_list = list(B = 200, m = 400, z = NULL)))


check_fn_args_boot_res <- function(inp_list) {
  assertthat::assert_that(
    is.list(inp_list)
    && length(inp_list) > 0
    && length(purrr::compact(.x = inp_list)) > 0,
    msg = glue::glue("Input must be a non-empty non-NULL list")
  )

  assertthat::assert_that(
    length(inp_list) == 1,
    msg = glue::glue("Input list must have 1 element",
                     "currently it has",
                     "{length(inp_list)} items",
                     .sep = " "
    )
  )

  assertthat::assert_that(
    all(names(purrr::compact(.x = inp_list)) == c("B")),
    msg = glue::glue("inp_list must only contain the ",
                     "named element: B",
                     .sep = " "
    )
  )
}

# invalid since the named arguments are B, m i.e. no just B
testthat::expect_error(check_fn_args_boot_res(inp_list = list(B = 10, m = 100)))
# invalid since the named arguments are B, m i.e. no just B
testthat::expect_error(check_fn_args_boot_res(inp_list = list(B = 10, m = NULL)))
# valid since the named arguments are B only
testthat::expect_true(check_fn_args_boot_res(inp_list = list(B = 10)))
# invalid since the named arguments are B, m i.e. not just B
testthat::expect_error(check_fn_args_boot_res(inp_list = list(B = 0, m = 100)))
# invalid since the named arguments are B, m i.e. not just B
testthat::expect_error(check_fn_args_boot_res(inp_list = list(B = 10, m = 0)))
# invalid since the named arguments are B, m i.e. not just B
testthat::expect_error(check_fn_args_boot_res(inp_list = list(B = 10, m = -1)))
# valid since the named arguments are B, m
testthat::expect_error(check_fn_args_boot_res(inp_list = list(B = -10, m = -1)))
# invalid since the named arguments are B, m, z i.e. not just B
testthat::expect_error(check_fn_args_boot_res(inp_list = list(B = 200, m = 400, z = 2300)))
# invalid since the named arguments are b, m i.e. not just B
testthat::expect_error(check_fn_args_boot_res(inp_list = list(b = 200, m = 400)))
# invalid since the named arguments are b, M i.e. not just B
testthat::expect_error(check_fn_args_boot_res(inp_list = list(b = 200, M = 400)))
# invalid since the named arguments are B, M i.e. not just B
testthat::expect_error(check_fn_args_boot_res(inp_list = list(B = 200, M = 400)))
# invalid since the named arguments are B, m, z i.e. no just B
testthat::expect_error(check_fn_args_boot_res(inp_list = list(B = 200, m = 400, z = NULL)))
# invalid since the named arguments are b i.e. not B
testthat::expect_error(check_fn_args_boot_res(inp_list = list(b = 200)))
# invalid since the named arguments are all NULL
testthat::expect_error(check_fn_args_boot_res(inp_list = list(B = NULL)))


