#' Create grid of centers for reweighting the distribution of the regressor
#'
#' Create grid of centers for reweighting the distribution of the regressor.
#' The function creates either a grid of evenly spaced values between the
#' minimum and the maximum of the regressor's values or based on the quantiles.
#' If based on the quantiles, then the values of the grid consists
#' of the quantiles corresponding
#' to seq(0.1,0.9,length=n_grid).
#'
#' @param x Vector of values of the regressor
#' @param grid_method Method to construct the grid ("regular" or "quantiles")
#' @param n_grid Number of centers present in the grid
#'
#' @return A vector of values corresponding to the centers for the reweighting
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Construct a grid of centers for the reweighting of the regressor
#' set.seed(162632)
#' n <- 100
#' x <- rnorm(n, 0, 1)
#' centers <- comp_grid_centers(x, "quantiles", 9)
#'
#' # Display the output
#' print(centers)
#' }
comp_grid_centers <- function(x, grid_method, n_grid) {
  assertthat::assert_that(grid_method == 'regular' | grid_method == 'quantiles',
                          msg = glue::glue('The method to construct the grid needs to be either "regular" or "quantiles".'))
  assertthat::assert_that((n_grid > 1) & (n_grid%%1 == 0),
                          msg = glue::glue('The number of points in the grid needs to be an integer larger than 1.'))
  if (grid_method == "regular") {
    lb <- stats::quantile(x, probs = 0)
    ub <- stats::quantile(x, probs = 1)
    out <- base::seq(lb, ub, length = n_grid)
  } else if (grid_method == "quantiles") {
    out <- stats::quantile(x, probs = base::seq(0.1, 0.9, length = n_grid))
  }
  return(unname(out))
}


#' Obtain estimates of a GLM or OLS estimates under reweighting of one regressor
#'
#' Obtain estimates of a GLM or OLS estimates under reweighting of one regressor.
#' The function returns a set of estimates for each bootstrapped data set and
#' each center of reweighting.
#'
#' @param mod_fit An object of class "lm" or "glm" to fit on the data. This object
#' should contain the formula, the data, and, in case of "glm", the family
#' @param term_to_rwgt A character corresponding to the regressor to be reweighted
#' @param boot_samples A list of data sets that have been bootstrapped from the
#' original data set. Each data set needs to contain a column "n_obs" which indicates
#' the order of each observation in the original data
#' @param term_to_rwgt_centers A vector of numeric values corresponding to the
#' centers for the reweighting procedure
#'
#' @return A tibble containing the number of the bootstrapped data set (b),
#' the names of the regressor under reweighting, and the sets of model estimates
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get OLS estimates under reweighting of regressor X1
#' n <- 1e3
#' X1 <- stats::rnorm(n, 0, 1)
#' X2 <- stats::rnorm(n, 0, 3)
#' y <- 2 + X1 + X2 * 0.3 + stats::rnorm(n, 0, 1)
#' df <- tibble::tibble(y = y, X1 = X1, X2 = X2, n_obs = 1:length(X1))
#' mod_fit <- stats::lm(y ~ X1 + X2, df)
#' boot_samples <- comp_empirical_bootstrap_samples(df, B = 100)
#' ols_rwgt_X1 <- comp_coef_rwgt_single(mod_fit, "X1", boot_samples, c(-1, 0, 1, 2))
#'
#' # Display the output
#' print(ols_rwgt_X1)
#' }
comp_coef_rwgt_single <- function(mod_fit, term_to_rwgt, boot_samples, term_to_rwgt_centers) {
  data <- model.frame(mod_fit)

  alpha <- 1
  gamma <- sd(data[[term_to_rwgt]]) * alpha
  weights_assigned <- tibble::tibble(
    center = sort(unique(term_to_rwgt_centers)),
    weights = as.list(center) %>%
      purrr::map(~ exp(-(.x[[1]] - data[[term_to_rwgt]])^2 / (2 * gamma^2)))
  )

  out <- as.list(1:length(weights_assigned$center)) %>%
    purrr::map_df(~ purrr::map(boot_samples$data,
      ~ comp_cond_model(
        mod_fit = mod_fit,
        data = .x %>% dplyr::mutate(weights = weights_assigned$weights[[.y]][n_obs]) %>%
          dplyr::mutate(weights = weights / sum(weights)),
        weights = "weights"
      ),
      .y = .x
    ) %>%
      dplyr::bind_rows(.id = "b") %>%
      dplyr::mutate(b = as.integer(b)) %>%
      dplyr::mutate(term_rwgt_center = weights_assigned$center[.x]))

  return(out)
}

#' Obtain estimates of a GLM or OLS estimates under reweighting of one or multiple regressors
#'
#' Obtain estimates of a GLM or OLS estimates under reweighting of one or multiple regressor.
#' The function returns a set of estimates for each regressor that is reweighted,
#' for each bootstrapped data set, and for each center of reweighting.
#'
#' @param mod_fit An object of class "lm" or "glm" to fit on the data. This object
#' should contain the formula, the data, and, in case of "glm", the family
#' @param terms_to_rwgt A vector of characters corresponding to the regressors
#' used in the reweighting
#' @param B an integer corresponding to the number of bootstrap repetitions or number of bootstrap samples to be drawn
#' @param m an integer corresponding to the number of observations to be sampled with replacement from the dataset
#' for each bootstrap repetition
#' @param grid_centers Dataframe containing the regressors and reweighting centers.
#' Each column corresponds to a different regressor (specified in the column's name).
#' @param grid_method Method to construct the grid ("regular" or "quantiles")
#' @param n_grid Number of centers present in the grid
#'
#'
#' @return A tibble containing the number of the bootstrapped data set (b),
#' the size of each bootstrapped data set (m),
#' the names of the regressors under reweighting,
#' and the sets of model estimates
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get OLS estimates under reweighting of all regressors with default grid
#' n <- 1e3
#' X1 <- stats::rnorm(n, 0, 1)
#' X2 <- stats::rnorm(n, 0, 3)
#' y <- 2 + X1 + X2 * 0.3 + stats::rnorm(n, 0, 1)
#' df <- tibble::tibble(y = y, X1 = X1, X2 = X2, n_obs = 1:length(X1))
#' mod_fit <- stats::lm(y ~ X1 + X2, df)
#' ols_rwgt <- comp_coef_rwgt(mod_fit, c("X1", "X2"))
#'
#' # Display the output
#' print(ols_rwgt)
#'
#' # Get OLS estimates under reweighting of all regressors by feeding grid of centers into the function
#' ols_rwgt_grid_fixed <- comp_coef_rwgt(mod_fit, "X1", grid_centers = data.frame(X1 = c(0,1)))
#'
#' #' # Display the output
#' print(ols_rwgt_grid_fixed)
#' }
comp_coef_rwgt <- function(mod_fit,
                           terms_to_rwgt,
                           B = 100,
                           m = NULL,
                           grid_centers = NULL,
                           grid_method = "quantiles",
                           n_grid = 9) {
  assertthat::assert_that(grid_method == 'regular' | grid_method == 'quantiles',
                          msg = glue::glue('The method to construct the grid must be either "regular" or "quantiles". It is currently {grid_method}.'))
  assertthat::assert_that(all("lm" == class(mod_fit)) | any("glm" == class(mod_fit)),
                          msg = glue::glue("mod_fit must only be of class lm or glm"))
  assertthat::assert_that((n_grid > 1) & (n_grid == as.integer(n_grid)),
                          msg = glue::glue('The number of points in the grid needs to be an integer larger than 1. It is currently {n_grid}.'))
  assertthat::assert_that(B == as.integer(B),
                          msg = glue::glue("B must be an integer e.g. 100, it is currently {B}"))
  assertthat::assert_that(B > 0,
                          msg = glue::glue("B must be positive e.g. 100, it is currently {B}"))
  assertthat::assert_that(all(terms_to_rwgt %in% names(model.frame(mod_fit))),
                          msg = glue::glue("All terms used in the reweighting procedure must be included in the variables present in the data on which the model was fitted."))
  if(!is.null(m)){
    assertthat::assert_that(m == as.integer(m),
                            msg = glue::glue("m must be an integer e.g. 100, it is currently {m}"))
    assertthat::assert_that(m > 0,
                            msg = glue::glue("m must be positive e.g. 100, it is currently {m}"))
  }
  if(!is.null(grid_centers)){
    assertthat::assert_that(is.data.frame(grid_centers),
                            msg = glue::glue("grid_centers must be a data frame where column names correspond to the names of the regressors and the centers values"))
  }

  data <- model.frame(mod_fit)
  terms_to_rwgt <- as.list(terms_to_rwgt)
  names(terms_to_rwgt) <- unlist(terms_to_rwgt)

  if (is.null(m)) {
    m <- nrow(data)
  }

  boot_samples <- maar::comp_empirical_bootstrap_samples(
    data = data %>% tibble::add_column(n_obs = 1:nrow(data)),
    B = B,
    m = m
  )

  if (is.null(grid_centers)) {
    grid_centers <- data %>%
      dplyr::summarise_all(function(x) comp_grid_centers(x, grid_method = grid_method, n_grid = n_grid))
  }

  out <- terms_to_rwgt %>%
    purrr::map(~ comp_coef_rwgt_single(
      mod_fit = mod_fit,
      term_to_rwgt = .x[[1]],
      boot_samples = boot_samples,
      term_to_rwgt_centers = grid_centers %>% dplyr::pull(!!.x[[1]])
    )) %>%
    dplyr::bind_rows(.id = "term_rwgt") %>%
    dplyr::mutate(m = m) %>%
    dplyr::mutate(n = nrow(data)) %>%
    tidyr::nest(boot_out = c(term, estimate))

  return(out)
}



#' Obtain the "focal slope" model diagnostic
#'
#' Obtain the "focal slope" model diagnostic as described in
#' \insertCite{@see @buja2019modelsasapproximationspart2;textual}{maar}.
#' The diagnostic is useful to investigate interaction terms that have not been included
#' in the model in the first place.
#'
#' @param coef_rwgt A tibble containing the number of the bootstrapped data set (b),
#' the names of the regressors under reweighting, and the sets of model estimates
#' @param term_chosen Character corresponding to the coefficient to be analysed
#'
#' @return A ggplot2 object which shows how the coefficient of one regressor of interest (term_chosen)
#' varies under reweighting of the regressors.
#' Vertical axes = estimates of the regression coefficient of interest.
#' Horizontal axes and panels titles = values and names of the regressors respectively.
#' Grey lines correspond to the traces of bootstrapped estimates forming the "spaghetti plot".
#' The black vertical lines indicate 95% confidence intervals for the estimates on the
#' bootstrapped data sets for each of the centers of reweighting (term_rwgt_center).
#' The black line in the middle corresponds to the mean of the estimates (which is
#' approximately equal to the OLS estimates on the original data).
#'
#' @export
#'
#' @importFrom Rdpack reprompt
#'
#' @references \insertAllCited{}
#'
#' @examples
#' \dontrun{
#' # Get focal slope of X1
#' n <- 1e3
#' X1 <- stats::rnorm(n, 0, 1)
#' X2 <- stats::rnorm(n, 0, 3)
#' y <- 2 + X1 + X2 * 0.3 + stats::rnorm(n, 0, 1)
#' df <- tibble::tibble(y = y, X1 = X1, X2 = X2, n_obs = 1:length(X1))
#' mod_fit <- stats::lm(y ~ X1 + X2, df)
#' ols_rwgt <- comp_coef_rwgt(mod_fit, c("X1", "X2"), B = 300)
#'
#' # Display the output
#' focal_slope(ols_rwgt, "X1")
#' }
focal_slope <- function(coef_rwgt, term_chosen) {
  coef_rwgt_conf_int <- comp_conf_int_bootstrap(
    boot_out = coef_rwgt,
    probs = c(0.025, 0.975),
    group_vars = c("term", "term_rwgt", "term_rwgt_center")
  ) %>%
    tidyr::pivot_wider(names_from = q, values_from = x) %>%
    dplyr::filter(term == term_chosen)

  coef_rwgt <- coef_rwgt %>%
    tidyr::unnest(boot_out) %>%
    dplyr::filter(term == term_chosen)

  coef_rwgt_means <- coef_rwgt %>%
    dplyr::group_by(term_rwgt, term_rwgt_center) %>%
    dplyr::summarise(estimate = mean(estimate), .groups = "keep")

  # tilt_test <- coef_rwgt %>%
  #   dplyr::group_by(term_rwgt) %>%
  #   dplyr::mutate(center = dplyr::case_when(
  #     term_rwgt_center == max(term_rwgt_center) ~ "Max_center",
  #     term_rwgt_center == min(term_rwgt_center) ~ "Min_center"
  #   )) %>%
  #   tidyr::drop_na() %>%
  #   dplyr::select(term_rwgt, b, center, estimate) %>%
  #   tidyr::pivot_wider(names_from = center, values_from = estimate) %>%
  #   dplyr::group_by(term_rwgt) %>%
  #   dplyr::summarise(d = mean(Max_center - Min_center),
  #                    p = t.test(Max_center, Min_center)$p.value,
  #                    .groups = "keep") %>%
  # dplyr::mutate(text_test = paste0("Tilt test: p=", round(p, 2), " d=", round(d, 2)))

  out <- coef_rwgt %>%
    ggplot2::ggplot(mapping = ggplot2::aes(term_rwgt_center, estimate, group = b)) +
    ggplot2::geom_line(alpha = 0.3, col = "grey") +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "Value of regressor", y = paste("Coeffient of", term_chosen)) +
    ggplot2::facet_wrap(~term_rwgt, ncol = 3, scales = "free_x") +
    ggplot2::geom_segment(
      data = coef_rwgt_conf_int,
      mapping = ggplot2::aes(
        x = term_rwgt_center, xend = term_rwgt_center,
        y = `0.025`, yend = `0.975`, group = NULL
      )
    ) +
    ggplot2::geom_line(
      data = coef_rwgt_means,
      mapping = ggplot2::aes(x = term_rwgt_center, y = estimate, group = NULL)
    )
  # ) +
  # ggplot2::geom_text(
  #   data = tilt_test %>% dplyr::mutate(term_rwgt_center = -Inf, estimate = Inf),
  #   ggplot2::aes(term_rwgt_center, estimate, label = text_test, group = NULL),
  #   hjust = 0, vjust = 1
  # )
  return(out)
}


#' Obtain the "nonlinearity detection" model diagnostic
#'
#' Obtain the "nonlinearity detection" model diagnostic described in
#' \insertCite{@see @buja2019modelsasapproximationspart2;textual}{maar}.
#' This tool provides insights into marginal nonlinear behavior of response surfaces.
#'
#' @param coef_rwgt A tibble containing the number of the bootstrapped data set (b),
#' the names of the regressors under reweighting, and the sets of model estimates
#'
#' @return A ggplot2 object which shows how the coefficients estimates
#' vary under reweighting of their own regressors.
#' Vertical axes = estimates of the regression coefficients.
#' Horizontal axes and panels titles = regressors.
#' Grey lines correspond to the traces of bootstrapped estimates forming the "spaghetti plot".
#' The black vertical lines indicate 95% confidence intervals for the estimates on the
#' bootstrapped data sets for each of the centers of reweighting (term_rwgt_center).
#' The black line in the middle corresponds to the mean of the estimates (which is
#' approximately equal to the OLS estimates on the original data).
#'
#' @export
#'
#' @importFrom Rdpack reprompt
#'
#' @references \insertAllCited{}
#'
#' @examples
#' \dontrun{
#' # Get focal slope of X1
#' n <- 1e3
#' X1 <- stats::rnorm(n, 0, 1)
#' X2 <- stats::rnorm(n, 0, 3)
#' y <- 2 + X1 + X2 * 0.3 + stats::rnorm(n, 0, 1)
#' df <- tibble::tibble(y = y, X1 = X1, X2 = X2, n_obs = 1:length(X1))
#' mod_fit <- stats::lm(y ~ X1 + X2, df)
#' ols_rwgt <- comp_coef_rwgt(mod_fit, c("X1", "X2"), B = 300)
#'
#' # Display the output
#' nonlinearity_detection(ols_rwgt)
#' }
nonlinearity_detection <- function(coef_rwgt) {
  coef_rwgt_conf_int <- comp_conf_int_bootstrap(
    boot_out = coef_rwgt,
    probs = c(0.025, 0.975),
    group_vars = c("term", "term_rwgt", "term_rwgt_center")
  ) %>%
    tidyr::pivot_wider(names_from = q, values_from = x) %>%
    dplyr::filter(term == term_rwgt)

  coef_rwgt <- coef_rwgt %>%
    tidyr::unnest(boot_out) %>%
    dplyr::filter(term == term_rwgt)

  coef_rwgt_means <- coef_rwgt %>%
    dplyr::group_by(term_rwgt, term_rwgt_center) %>%
    dplyr::summarise(estimate = mean(estimate), .groups = "keep")

  out <- coef_rwgt %>%
    ggplot2::ggplot(mapping = ggplot2::aes(term_rwgt_center, estimate, group = b)) +
    ggplot2::geom_line(alpha = 0.3, col = "grey") +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "Value of regressor", y = paste("Coefficient")) +
    ggplot2::facet_wrap(~term_rwgt, ncol = 3, scales = "free") +
    ggplot2::geom_segment(
      data = coef_rwgt_conf_int,
      mapping = ggplot2::aes(
        x = term_rwgt_center, xend = term_rwgt_center,
        y = `0.025`, yend = `0.975`, group = NULL
      )
    ) +
    ggplot2::geom_line(
      data = coef_rwgt_means,
      mapping = ggplot2::aes(x = term_rwgt_center, y = estimate, group = NULL)
    )
  return(out)
}


#' Obtain the "focal reweighting variable" model diagnostic
#'
#' Obtain the "focal reweighting variable" model diagnostic described in
#' \insertCite{@see @buja2019modelsasapproximationspart2;textual}{maar}.
#'
#' @param coef_rwgt A tibble containing the number of the bootstrapped data set (b),
#' the names of the regressors under reweighting, and the sets of model estimates
#' @param term_chosen Character corresponding to the coefficient to be analysed
#'
#' @return A ggplot2 object which shows how the coefficients estimates (vertical axes)
#' of the different regressors (title of each panel) vary under
#' reweighting of one regressor (horizontal axis).
#' Vertical axes and panels titles = estimates of the regression coefficients and
#' names of the corresponding regressors.
#' Horizontal axes = value of the regressor of interest.
#' Grey lines correspond to the traces of bootstrapped estimates forming the "spaghetti plot".
#' The black vertical lines indicate 95% confidence intervals for the estimates on the
#' bootstrapped data sets for each of the centers of reweighting (term_rwgt_center).
#' The black line in the middle corresponds to the mean of the estimates (which is
#' approximately equal to the OLS estimates on the original data).
#'
#' @export
#'
#' @importFrom Rdpack reprompt
#'
#' @references \insertAllCited{}
#'
#' @examples
#' \dontrun{
#' # Get focal slope of X1
#' n <- 1e3
#' X1 <- stats::rnorm(n, 0, 1)
#' X2 <- stats::rnorm(n, 0, 3)
#' y <- 2 + X1 + X2 * 0.3 + stats::rnorm(n, 0, 1)
#' df <- tibble::tibble(y = y, X1 = X1, X2 = X2, n_obs = 1:length(X1))
#' mod_fit <- stats::lm(y ~ X1 + X2, df)
#' ols_rwgt <- comp_coef_rwgt(mod_fit, c("X1", "X2"), B = 300)
#'
#' # Display the output
#' focal_reweighting_variable(ols_rwgt, "X1")
#' }
#'
focal_rwgt_var <- function(coef_rwgt, term_chosen) {
  coef_rwgt_conf_int <- comp_conf_int_bootstrap(
    boot_out = coef_rwgt,
    probs = c(0.025, 0.975),
    group_vars = c("term", "term_rwgt", "term_rwgt_center")
  ) %>%
    tidyr::pivot_wider(names_from = q, values_from = x) %>%
    dplyr::filter(term_rwgt == term_chosen)

  coef_rwgt <- coef_rwgt %>%
    tidyr::unnest(boot_out) %>%
    dplyr::filter(term_rwgt == term_chosen)

  coef_rwgt_means <- coef_rwgt %>%
    dplyr::group_by(term_rwgt, term, term_rwgt_center) %>%
    dplyr::summarise(estimate = mean(estimate), .groups = "keep")

  out <- coef_rwgt %>%
    ggplot2::ggplot(mapping = ggplot2::aes(term_rwgt_center, estimate, group = b)) +
    ggplot2::geom_line(alpha = 0.3, col = "grey") +
    ggplot2::theme_bw() +
    ggplot2::labs(x = term_chosen, y = "Coeffient") +
    ggplot2::facet_wrap(~term, ncol = 3, scales = "free") +
    ggplot2::geom_segment(
      data = coef_rwgt_conf_int,
      mapping = ggplot2::aes(
        x = term_rwgt_center, xend = term_rwgt_center,
        y = `0.025`, yend = `0.975`, group = NULL
      )
    ) +
    ggplot2::geom_line(
      data = coef_rwgt_means,
      mapping = ggplot2::aes(x = term_rwgt_center, y = estimate, group = NULL)
    )
  return(out)
}

