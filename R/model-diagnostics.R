

get_grid_centers <- function(x, grid_method, n_grid) {
  if (grid_method == "even") {
    lb <- stats::quantile(x, probs = 0.025)
    ub <- stats::quantile(x, probs = 0.975)
    out <- base::seq(lb, ub, length = n_grid)
  } else if (grid_method == "quantile") {
    out <- stats::quantile(x, probs = base::seq(0.025, 0.975, length = n_grid))
  }
  return(out)
}



get_model_diagnostics_single_var <- function(mod_fit, term_to_rwgt, boot_samples, centers = NULL) {

  data <- model.frame(mod_fit)
  centers <- sort(unique(centers))

  bins_assigned <- tibble::tibble(
    n_obs = 1:base::nrow(data),
    bin = as.list(dplyr::pull(data, !!term_to_rwgt)) %>%
      purrr::map(~ which.min(abs(.x - centers))) %>%
      unlist())

  boot_samples$data <- boot_samples$data %>%
    purrr::map(~ .x %>%
      dplyr::inner_join(bins_assigned, by = "n_obs") %>%
      dplyr::select(-n_obs))

  out <- tibble::tibble()
  for (bin_chosen in 1:length(centers)) {

    out_join <- purrr::map(boot_samples$data, ~ maar::comp_empirical_bootstrap_cond_model(mod_fit, .x[.x$bin == bin_chosen, ] %>% dplyr::select(-bin))) %>%
          dplyr::bind_rows(.id = "b")

    out <- out %>%
      dplyr::bind_rows(tibble::tibble(bin_center = centers[bin_chosen], out_join))
  }

  return(out)
}


get_model_diagnostics <- function(mod_fit, terms_to_rwgt, B=100, grid_bins_centers=NULL, grid_method="even", n_grid=10) {
  data <- model.frame(mod_fit)
  boot_samples <- maar::comp_empirical_bootstrap_samples(data %>% tibble::add_column(n_obs = 1:nrow(data)), B = B)

  if (is.null(grid_bins_centers)) {
    grid_bins_centers <- data %>%
      dplyr::summarise_all(function(x) get_grid_centers(x, grid_method = grid_method, n_grid = n_grid))
  }

  out <- terms_to_rwgt %>%
    purrr::map(~ get_model_diagnostics_single_var(
      mod_fit,
      .x[[1]],
      boot_samples,
      grid_bins_centers %>% dplyr::pull(!!.x[[1]])
    )) %>%
    dplyr::bind_rows(.id = "term_rwgt") %>%
    tidyr::nest(boot_out = c(term, estimate))
  return(out)
}



data <- vroom::vroom("http://www-stat.wharton.upenn.edu/~buja/STAT-961/Homeless_LA_by_Census_Tracts.csv")
mod_fit <- lm(StreetTotal ~ ., data)
terms_to_rwgt <- as.list(setdiff(colnames(data), "StreetTotal"))
names(terms_to_rwgt) <- unlist(terms_to_rwgt)
out <- get_model_diagnostics(
  mod_fit = mod_fit,
  terms_to_rwgt = terms_to_rwgt,
  B = 100,
  grid_method = "quantile",
  n_grid = 5
)
out

# bootstrap_samples <- maar::comp_empirical_bootstrap_samples(data %>% tibble::add_column(n_obs = 1:nrow(data)), B = 100)




# coefs <- terms_to_rwgt %>%
#   purrr::map(~ get_est_under_var_rwgt(.x[[1]], data, bootstrap_samples)) %>%
#   bind_rows(.id = "term_rwgt")
#
conf_int <- out %>% tidyr::unnest(boot_out) %>%
   dplyr::group_by(term_rwgt, term, bin_center) %>%
   dplyr::summarise(
     lb = stats::quantile(estimate, probs = c(0.025), na.rm = T),
     mean_estimate = mean(estimate),
     ub = stats::quantile(estimate, probs = c(0.975), na.rm = T)
)
#
term_chosen <- "PercVacant"
out %>%
    tidyr::unnest(boot_out) %>%
   dplyr::filter(term == term_chosen) %>%
    ggplot2::ggplot(ggplot2::aes(bin_center, estimate, group = b)) +
    ggplot2::geom_line(alpha = 0.3, col = "grey") +
    ggplot2::theme_bw() +
    #ggplot2::ylim(0,50) +
    ggplot2::facet_wrap(~term_rwgt, ncol = 3, scale = "free_x") +
    ggplot2::ylab(paste("Coef. of", term_chosen)) +
    ggplot2::xlab("Value of regressor") +
    ggplot2::geom_segment(
     data = conf_int %>% dplyr::filter(term == term_chosen),
     ggplot2::aes(x = bin_center, xend = bin_center, y = lb, yend = ub, group = NULL)
   ) +
    ggplot2::geom_line(data = conf_int %>% dplyr::filter(term == term_chosen),
                       ggplot2::aes(x = bin_center, y = mean_estimate, group = NULL))








