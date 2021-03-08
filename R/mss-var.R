# print summary of object
summary.maars_lm <- function(mod_fit,
                             sand = TRUE,
                             boot_emp = FALSE,
                             boot_res = FALSE,
                             boot_mul = FALSE,
                             well_specified = FALSE,
                             digits = 3, ...) {

  out_summ <- get_var_tidy_summary(mod_fit,
    sand = sand,
    boot_emp = boot_emp,
    boot_res = boot_res,
    boot_mul = boot_mul,
    well_specified = well_specified
  )

  # add asterisks for statistical significance in a separate tibble
  significance_ast <- out_summ %>%
    dplyr::select(dplyr::starts_with('p.value')) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(),
                                ~dplyr::case_when(round(.x,3) == 0.000 ~ '***',
                                           .x < 0.001 ~ '**',
                                           .x < 0.01 ~ '*',
                                           .x < 0.05 ~ '.',
                                           TRUE ~ ' '))) %>%
  stats::setNames(paste0('signif.', stringr::str_sub(names(.), start=9)))

  # join asterisks with out_summ
  out_summ <- out_summ %>% dplyr::bind_cols(significance_ast)

  # reorder columns - we could change the order
  cols_prefix_order <- c('term', 'estimate', 'std.error',
                  'statistic', 'p.value', 'signif')
  out_summ <- out_summ[,cols_prefix_order %>%
                          purrr::map(~ names(out_summ)[grepl(., names(out_summ))]) %>%
                         unlist()]

  # format p values
  out_summ <- out_summ %>%
    dplyr::mutate(dplyr::across(
      dplyr::starts_with("p.value"),
      ~ format.pval(.x, digits = 1)
    ))

  cat("\nCall:\n",
    stringr::str_c(rlang::expr_deparse(x = mod_fit$call),
      sep = "\n",
      collapse = "\n"
    ),
    "\n\n",
    sep = ""
  )
  cat("Coefficients:\n")
  print.data.frame(out_summ, row.names = FALSE, digits = digits, right = TRUE)
  cat("\n---\n")
  cat("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1")

  summ_lm <- summary.lm(mod_fit)
  cat(
    "\n\nResidual standard error:",
    formatC(signif(summ_lm$sigma, digits = digits)), "on",
    mod_fit$df.residual, "degrees of freedom"
  )
  cat(
    "\nMultiple R-squared:", formatC(summ_lm$r.squared, digits = digits),
    ",\tAdjusted R-squared:", formatC(summ_lm$adj.r.squared, digits = digits)
  )
  cat(
    "\nF-statistic:", summ_lm$fstatistic[1L],
    " on ", summ_lm$fstatistic[2L], " and ", summ_lm$fstatistic[3L],
    "DF,  p-value:",
    format.pval(pf(summ_lm$fstatistic[1L],
      summ_lm$fstatistic[2L],
      summ_lm$fstatistic[3L],
      lower.tail = FALSE
    ))
  )

  cat("\n---\nAssumptions:\n")
  cat(paste0(utf8::utf8_format('\U001F3AF'),
             get_assumptions(mod_fit = mod_fit,
    sand = sand, boot_emp = boot_emp,
    boot_res = boot_res,
    boot_mul = boot_mul,
    well_specified = well_specified
  )) %>% paste0(., collapse = '\n'))
}


# print variance
mss_var <- function(mod_fit, boot_emp = NULL, boot_res = NULL,
                    boot_mul = NULL) {
  # the following condition will need to be revised once we introduce glm
  if (all(c("maars_lm", "lm") %in% class(mod_fit))) {
    attr(mod_fit, "class") <-
      "lm"
  }

  out_var <- comp_var(mod_fit, boot_emp, boot_res, boot_mul)
  mod_fit$var <- out_var

  attr(mod_fit, "class") <- c("maars_lm", "lm")

  return(mod_fit)
}


tidy.maars_lm <- function(x, ...){
  # do be added
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

# This is just a prototype - open for discussion
plot.maars_lm <- function(x, ...){
  mms_diag_plots <- get_ols_diag_plots(mod_fit = x)
  for (i in base::seq_along(mms_diag_plots)) {
    if (i == 1) {
      # For the first plot, don't ask the user for prompt
      par(ask = FALSE)
      print(mms_diag_plots[[i]])
    } else {
      # For subsequent plots, ask the user for prompts to display
      # the plots sequentially
      par(ask = TRUE)
      print(mms_diag_plots[[i]])
    }
    par(ask = FALSE)
  }
}
