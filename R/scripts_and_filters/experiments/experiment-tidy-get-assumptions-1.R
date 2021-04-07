library(tidyverse)
proto_gen_comp_var_ind <- function(var_type_abb,
                                   summary_tbl = NULL, # TODO: Should be passed in, no NULLS
                                   cov_mat = NULL,
                                   B = NULL, m = NULL, replace = NULL) {

  # Define the emoji lookup table.
  # Ensure that output is a {glue} string from consistency
  # TODO: We might delete this, if we are happy with just constructing
  #       a glue string directly, as per code below, rather than filtering
  #       this tibble. For discussion
  var_emoji_tbl <-
    tibble::tribble(
      ~"var_abb", ~"var_emoji",
      "lm", "\U1F4C9\U1F4C8",
      "sand", "\U1F969\U1F35E",
      "emp", "\U1F9EE\U1F45F",
      "res", "\U2696\U1F45F",
      "mul", "\U274C\U1F45F"
    ) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), glue::as_glue))

  # Define the Title lookup table.
  # Ensure that output is a {glue} string from consistency
  # TODO: We might delete this, if we are happy with just constructing
  #       a glue string directly, as per code below, rather than filtering
  #       this tibble. For discussion
  var_title_tbl <-
    tibble::tribble(
      ~"var_abb", ~"var_title",
      "lm", "Well Specified Model",
      "sand", "Sandwich",
      "emp", "Empirical Bootstrap",
      "res", "Residual Bootstrap",
      "mul", "Multiplier Bootstrap"
    ) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), glue::as_glue))

  # Define the Emoji Title lookup table.
  # Ensure that output is a {glue} string from consistency
  # TODO: We might delete this, if we are happy with just constructing
  #       a glue string directly, as per code below, rather than filtering
  #       this tibble. For discussion
  var_emoji_title_tbl <- var_emoji_tbl %>%
    dplyr::left_join(x = ., y = var_title_tbl, by = "var_abb") %>%
    dplyr::mutate(
      .data = .,
      var_emoji_title = glue::glue("{var_emoji}: {var_title}:")
    )

  # Define the Emoji Title lookup table. Second version
  # This one only requires the var_emoji_tbl to be created
  # Ensure that output is a {glue} string from consistency
  # TODO: We might delete this, if we are happy with just constructing
  #       a glue string directly, as per code below, rather than filtering
  #       this tibble. For discussion
  var_emoji_title_tbl2 <-
    tibble::tribble(
      ~"var_abb", ~"var_emoji",
      "lm", "\U1F4C9\U1F4C8",
      "sand", "\U1F969\U1F35E",
      "emp", "\U1F9EE\U1F45F",
      "res", "\U2696\U1F45F",
      "mul", "\U274C\U1F45F"
    ) %>%
    dplyr::mutate(
      .data = .,
      var_title = dplyr::case_when(
        var_abb == "lm" ~ "Well Specified Model",
        var_abb == "sand" ~ "Sandwich",
        var_abb == "emp" ~ "Empirical Bootstrap",
        var_abb == "res" ~ "Residual Bootstrap",
        var_abb == "mul" ~ "Multiplier Bootstrap"
      ),
      var_emoji_title = glue::glue("{var_emoji}: {var_title}:")
    ) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), glue::as_glue))

  # Get the specific type of individual variance variable
  out <- switch(var_type_abb,
    "lm" = {
      var_emoji <- glue::glue("\U1F4C9\U1F4C8")
      var_title <- glue::glue("Well Specified Model")
      out_list <- list(
        "var_type" = glue::glue("well_specified"),
        "var_type_abb" = glue::glue("{var_type_abb}"),
        var_emoji = var_emoji,
        var_title = var_title,
        "var_emoji_title" = glue::glue("{var_emoji}: {var_title}:"),
        "var_summary" = summary_tbl,
        "var_assumptions" = tibble::tribble(
          ~"var_abb", ~"var_assumption_type", ~"var_assumption_val",
          glue::glue("{var_type_abb}"), "text", "Observations are assumed to be independent",
          glue::glue("{var_type_abb}"), "text", "Residuals are assumed to be homoscedastic",
          glue::glue("{var_type_abb}"), "text", "Linearity of the conditional expectation is assumed"
        ) %>%
          dplyr::mutate(dplyr::across(dplyr::everything(), glue::as_glue)),
        "cov_mat" = cov_mat
      )
    },
    "sand" = {
      var_emoji <- glue::glue("\U1F969\U1F35E")
      var_title <- glue::glue("Sandwich")
      out_list <- list(
        "var_type" = glue::glue("sand"),
        "var_type_abb" = glue::glue("{var_type_abb}"),
        var_emoji = var_emoji,
        var_title = var_title,
        "var_emoji_title" = glue::glue("{var_emoji}: {var_title}:"),
        "var_summary" = summary_tbl,
        "var_assumptions" = tibble::tribble(
          ~"var_abb", ~"var_assumption_type", ~"var_assumption_val",
          glue::glue("{var_type_abb}"), "text", "Observations are assumed to be independent"
        ) %>%
          dplyr::mutate(dplyr::across(dplyr::everything(), glue::as_glue)),
        "cov_mat" = cov_mat
      )
    },
    "emp" = {
      var_emoji <- glue::glue("\U1F9EE\U1F45F")
      var_title <- glue::glue("Empirical Bootstrap")
      out_list <- list(
        "var_type" = glue::glue("boot_emp"),
        "var_type_abb" = glue::glue("{var_type_abb}"),
        var_emoji = var_emoji,
        var_title = var_title,
        "var_emoji_title" = glue::glue("{var_emoji}: {var_title}:"),
        "var_summary" = summary_tbl,
        "var_assumptions" = tibble::tribble(
          ~"var_abb", ~"var_assumption_type", ~"var_assumption_val",
          glue::glue("{var_type_abb}"), "text", "Observations are assumed to be independent",
          glue::glue("{var_type_abb}"), "text", "Linearity of the conditional expectation is assumed",
          glue::glue("{var_type_abb}"), "B", glue::glue("{B}"),
          glue::glue("{var_type_abb}"), "m", glue::glue("{m}"),
          glue::glue("{var_type_abb}"), "replace", glue::glue("{replace}")
        ) %>%
          dplyr::mutate(dplyr::across(dplyr::everything(), glue::as_glue)),
        "cov_mat" = cov_mat
      )
    },
    "res" = {
      var_emoji <- glue::glue("\U2696\U1F45F")
      var_title <- glue::glue("Residual Bootstrap")
      out_list <- list(
        "var_type" = glue::glue("boot_res"),
        "var_type_abb" = glue::glue("{var_type_abb}"),
        var_emoji = var_emoji,
        var_title = var_title,
        # "var_emoji" = dplyr::filter(.data = var_emoji_tbl,
        #                             var_abb == var_type_abb),
        # "var_title" = dplyr::filter(.data = var_title_tbl,
        #                             var_abb == var_type_abb),
        "var_emoji_title" = glue::glue("{var_emoji}: {var_title}:"),
        "var_summary" = summary_tbl,
        "var_assumptions" = tibble::tribble(
          ~"var_abb", ~"var_assumption_type", ~"var_assumption_val",
          glue::glue("{var_type_abb}"), "text", "Observations are assumed to be independent",
          glue::glue("{var_type_abb}"), "text", "Residuals are assumed to be homoscedastic",
          glue::glue("{var_type_abb}"), "text", "Linearity of the conditional expectation is assumed"
        ) %>%
          dplyr::mutate(dplyr::across(dplyr::everything(), glue::as_glue)),
        "cov_mat" = cov_mat
      )
    },
    "mul" = {
      var_emoji <- glue::glue("\U274C\U1F45F")
      var_title <- glue::glue("Multiplier Bootstrap")
      out_list <- list(
        "var_type" = glue::glue("boot_mul"),
        "var_type_abb" = glue::glue("{var_type_abb}"),
        var_emoji = var_emoji,
        var_title = var_title,
        "var_emoji_title" = glue::glue("{var_emoji}: {var_title}:"),
        "var_summary" = summary_tbl,
        "var_assumptions" = tibble::tribble(
          ~"var_abb", ~"var_assumption_type", ~"var_assumption_val",
          glue::glue("{var_type_abb}"), "text", "Observations are assumed to be independent",
          glue::glue("{var_type_abb}"), "text", "Linearity of the conditional expectation is assumed",
          glue::glue("{var_type_abb}"), "B", glue::glue("{B}")
        ) %>%
          dplyr::mutate(dplyr::across(dplyr::everything(), glue::as_glue)),
        "cov_mat" = cov_mat
      )
    }
  )
  return(out)
}


# Variables as they would be passed in from individual functions
B <- 150
m <- 60
replace <- TRUE
cov_mat <- diag(3)

# Run the get_assumptions
proto_gen_comp_var_ind(
  var_type_abb = "res",
  summary_tbl = NULL, # TODO: Should be passed in, no NULLS
  cov_mat = cov_mat,
  B = B,
  m = m,
  replace = replace
)
