# generate tibble with coefficients and stats
get_tidy_summary <- function(x, sand = TRUE,
                             boot_emp = FALSE,
                             boot_res = FALSE,
                             boot_mul = FALSE,
                             well_specified = FALSE, ...) {
  out_summ <- list(
    sand = ifelse(sand, "sand", FALSE),
    boot_emp = ifelse(boot_emp, "boot_emp", FALSE),
    boot_res = ifelse(boot_res, "boot_res", FALSE),
    boot_mul = ifelse(boot_mul, "boot_mul", FALSE),
    well_specified = ifelse(boot_mul, "boot_mul", FALSE)
  ) %>%
    purrr::keep(~ !isFALSE(.x)) %>%
    purrr::map(~ mod_fit[["var"]][[paste0("var_", .x)]]$var_summary %>%
      dplyr::mutate(sig = dplyr::case_when(
        p.value <= 0.001 ~ "***",
        p.value <= 0.01 ~ "**",
        p.value <= 0.05 ~ "*",
        TRUE ~ "."
      )) %>%
      stats::setNames(c(
        "Term",
        "Estimate",
        paste0("t value ", .x),
        paste0("Std. Error ", .x),
        paste0("Pr(>|t|) ", .x),
        paste0("Significance ", .x)
      ))) %>%
    purrr::reduce(dplyr::left_join, by = c("Term", "Estimate")) %>%
    dplyr::relocate(
      Term, Estimate, matches("Std. Error"),
      matches("t value"), matches("Pr(>|t|)"),
      matches("Significance")
    )
  out_summ
}

get_assumptions <- function(x, sand = sand,
                            boot_emp = boot_emp,
                            boot_res = boot_res,
                            boot_mul = boot_mul,
                            well_specified = well_specified, ...) {
  ass <- list(
    sand = ifelse(sand, "sand", FALSE),
    boot_emp = ifelse(boot_emp, "boot_emp", FALSE),
    boot_res = ifelse(boot_res, "boot_res", FALSE),
    boot_mul = ifelse(boot_mul, "boot_mul", FALSE),
    well_specified = ifelse(well_specified, 'well_specified', FALSE)
  ) %>%
    purrr::keep(~ !isFALSE(.x)) %>%
    purrr::map(~ tibble::tibble(Assumption = mod_fit[["var"]][[paste0("var_", .x)]]$var_assumptions)) %>%
    dplyr::bind_rows(.id = "Type")
  paste0("Assumptions:\n", paste0(ass$Type, "->", ass$Assumption, collapse = "\n"))
}

# print summary of object
summary.maars_lm <- function(x, sand = TRUE, boot_emp = FALSE, boot_res = FALSE,
                             boot_mul = FALSE,
                             well_specified = FALSE, ...) {
  out_summ <- get_tidy_summary(x,
    sand = sand,
    boot_emp = boot_emp,
    boot_res = boot_res,
    boot_mul = boot_mul,
    well_specified = well_specified, ...
  )

  out_summ

  cat("\nCall:\n", # S has ' ' instead of '\n'
    stringr::str_c(rlang::expr_deparse(x = mod_fit$call),
      sep = "\n",
      collapse = "\n"
    ),
    "\n\n",
    sep = ""
  )
  cat("Coefficients:\n")
  print.data.frame(out_summ, row.names = FALSE, digits = 2)
  cat("\n---\n")
  cat("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1")

  warning(get_assumptions(x,
    sand = sand, boot_emp = boot_emp,
    boot_res = boot_res,
    boot_mul = boot_mul,
    well_specified = well_specified
  ))
}

# print variance
mss_var <- function(mod_fit, boot_emp = NULL, boot_res = NULL,
                    boot_mul = NULL) {
  # the following condition will need to be revised once we introduce glm
  if (all(c("maars_lm", "lm") %in% class(mod_fit))) {
    attr(mod_fit, "class") <-
      "lm"
  }
  var_est <- comp_var(mod_fit, boot_emp, boot_res, boot_mul)
  mod_fit$var <- var_est
  attr(mod_fit, "class") <- c("maars_lm", "lm")
  mod_fit
}


print.maars_lm <- function(x, ...) {
  # this function is not really needed, but let's keep it for now
  NextMethod("print")
}


# Convert stats::lm object into maars_lm object
as.maars.lm <- function(x, ...) {
  UseMethod("as.maars")
}
as.maars <- function(x, ...) {
  attr(x, "class") <- c("maars_lm", "lm")
  return(x)
}



