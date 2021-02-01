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
