#' Create grid of centers for the reweighting of the the distribution of the regressor
#'
#' Create grid of centers for the reweighting of the distribution of the regressor.
#' The function creates either a grid of evenly spaced values between the
#' minimum and the maximum of the regressor's values or based on the quantiles.
#' If based on the quantiles, then the values of the grid consists
#' of the quantiles corresponding
#' to seq(0.1,0.9,length=n_grid).
#'
#' @param x Vector of values taken by the regressor.
#' @param grid_method Method to construct the grid of reweighting centers
#' that are either evenly spaced values between the maximum and the minimum ("regular")
#' or based on the quantiles between the second and the tenth deciles ("quantiles").
#' @param n_grid Number of centers present in the grid.
#'
#' @return A vector of values corresponding to the centers for the reweighting
#'
#' @export
#'
#'
#' @importFrom rlang .data
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
  assertthat::assert_that(grid_method == "regular" | grid_method == "quantiles",
    msg = glue::glue('The method to construct the grid needs to be either "regular" or "quantiles".')
  )
  assertthat::assert_that((n_grid > 1) & (n_grid %% 1 == 0),
    msg = glue::glue("The number of points in the grid needs to be an integer larger than 1.")
  )
  if (grid_method == "regular") {
    lb <- stats::quantile(x, probs = 0)
    ub <- stats::quantile(x, probs = 1)
    out <- base::seq(lb, ub, length = n_grid)
  } else if (grid_method == "quantiles") {
    out <- stats::quantile(x, probs = base::seq(0.1, 0.9, length = n_grid))
  }
  return(unname(out))
}


#' Obtain estimates for GLM of OLS under reweighting of one regressor.
#'
#' `comp_coef_rwgt_single` provides estimate for GLM and OLS under reweighting of one regressor.
#' The function returns a set of estimates for each bootstrapped data set and
#' each center of reweighting.
#' @details The GLM or OLS model extracted from `mod_fit` is fitted on each
#' `boot_samples` under reweighting of every reweighting center ` term_to_rwgt_centers` of the
#' regressor `term_to_rwgt`.
#'
#' @param mod_fit An object of class `lm` or `glm` to fit on the data. This object
#' should contain the formula, the data, and, in case of `glm`, the family.
#' @param term_to_rwgt A character corresponding to the regressor to be reweighted.
#' @param boot_samples A list of bootstrapped data sets.
#' Each data set must include a column "n_obs" which contains
#' the order of each observation as in the original data.
#' @param term_to_rwgt_centers A vector of numeric values corresponding to the
#' centers used for the reweighting.
#'
#' @return A tibble containing the number of the bootstrapped data set (`b`),
#' the value of the reweighting centers (`term_rwgt_center`),
#' and estimates of the regression coefficients (`term` and `estimate`).
#'
#' @export
#'
#' @importFrom rlang .data
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
#' boot_samples <- maar::comp_empirical_bootstrap_samples(df, B = 100)
#' ols_rwgt_X1 <- comp_coef_rwgt_single(mod_fit, "X1", boot_samples, c(-1, 0, 1, 2))
#'
#' # Display the output
#' print(ols_rwgt_X1)
#' }
comp_coef_rwgt_single <- function(mod_fit, term_to_rwgt, boot_samples, term_to_rwgt_centers) {
  data <- stats::model.frame(mod_fit)

  alpha <- 1
  gamma <- stats::sd(data[[term_to_rwgt]]) * alpha
  weights_assigned <- tibble::tibble(
    center = sort(unique(term_to_rwgt_centers)),
    weights = as.list(center) %>%
      purrr::map(~ exp(-(.x[[1]] - data[[term_to_rwgt]])^2 / (2 * gamma^2)))
  )

  out <- as.list(1:length(weights_assigned$center)) %>%
    purrr::map_df(~ purrr::map(boot_samples$data,
      ~ maar::comp_cond_model(
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

#' Obtain estimates of a GLM or OLS estimates under reweighting of one or multiple regressors.
#'
#' `comp_coef_rwgt` provides estimate for GLM and OLS under reweighting of multiple regressors.
#' The function returns a set of estimates for each regressor that is reweighted,
#' for each bootstrapped data set, and for each center of reweighting.
#' @details The model extracted from `mod_fit` is fitted on `B` bootstrapped data
#' sets via `m`-out-of-n empirical bootstrap using weighted regressions where
#' the coefficients to be reweighted are specified in `terms_to_rwgt` and
#' and have reweighting centers given by `grid_centers`.
#'
#' @param mod_fit An object of class `lm` or `glm` to fit on the data. This object
#' should contain the formula, the data, and, in case of `glm`, the family.
#' @param terms_to_rwgt A vector of characters corresponding to the names of the regressors
#' to be reweighted.
#' @param B an integer corresponding to the number of bootstrap repetitions or number of bootstrap samples to be drawn.
#' @param m an integer corresponding to the number of observations to be sampled
#' with replacement from the data set in each bootstrap repetition.
#' @param grid_centers Data frame containing the names of the regressors as columns
#' and the corresponding reweighting centers.
#' Each column corresponds to a different regressor (specified in the column's name).
#' @param grid_method Method to construct the grid of reweighting centers
#' that are either evenly spaced values between the maximum and the minimum ("regular")
#' or based on the quantiles between the second and the tenth deciles ("quantiles").
#' @param n_grid Number of centers present in the grid.
#'
#'
#' @return A tibble containing the number of the bootstrapped data set (`b`),
#' the size of each bootstrapped data set (`m`),
#' the value of the reweighting centers (`term_rwgt_center`) and the name of the regressor under reweighting (`term_rwgt`),
#' and the estimates of the regression coefficients (`term` and `estimate`).
#'
#' @export
#'
#' @importFrom rlang .data
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
#' ols_rwgt_grid_fixed <- comp_coef_rwgt(mod_fit, "X1", grid_centers = data.frame(X1 = c(0, 1)))
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
  assertthat::assert_that(grid_method == "regular" | grid_method == "quantiles",
    msg = glue::glue('The method to construct the grid must be either "regular" or "quantiles". It is currently {grid_method}.')
  )
  assertthat::assert_that(all("lm" == class(mod_fit)) | any("glm" == class(mod_fit)),
    msg = glue::glue("mod_fit must only be of class lm or glm")
  )
  assertthat::assert_that((n_grid > 1) & (n_grid == as.integer(n_grid)),
    msg = glue::glue("The number of points in the grid needs to be an integer larger than 1. It is currently {n_grid}.")
  )
  assertthat::assert_that(B == as.integer(B) & B > 0,
    msg = glue::glue("B must be an integer e.g. 100, it is currently {B}")
  )
  assertthat::assert_that(all(terms_to_rwgt %in% names(model.frame(mod_fit))),
    msg = glue::glue("All terms used in the reweighting procedure must be included in the variables present in the data on which the model was fitted.")
  )
  if (!is.null(m)) {
    assertthat::assert_that(m == as.integer(m) & m > 0,
      msg = glue::glue("m must be an integer e.g. 100, it is currently {m}")
    )
  }
  if (!is.null(grid_centers)) {
    assertthat::assert_that(is.data.frame(grid_centers),
      msg = glue::glue("grid_centers must be a data frame where column names correspond to the names of the regressors and the centers values")
    )
  }

  data <- stats::model.frame(mod_fit)
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
      dplyr::summarise_all(function(x) {
        comp_grid_centers(x,
          grid_method = grid_method,
          n_grid = n_grid
        )
      })
  }

  out <- terms_to_rwgt %>%
    purrr::map(~ maar::comp_coef_rwgt_single(
      mod_fit = mod_fit,
      term_to_rwgt = .x[[1]],
      boot_samples = boot_samples,
      term_to_rwgt_centers = grid_centers %>% dplyr::pull(!!.x[[1]])
    )) %>%
    dplyr::bind_rows(.id = "term_rwgt") %>%
    dplyr::mutate(m = m) %>%
    dplyr::mutate(n = nrow(data)) %>%
    tidyr::nest(
      .data = .,
      boot_out = c(.data$term, .data$estimate)
    )

  return(out)
}



#' Obtain the `focal slope` model diagnostic.
#'
#' Obtain the `focal slope` model diagnostic as described in
#' \insertCite{@see @buja2019modelsasapproximationspart2;textual}{maar}.
#' @details Through this tool, one can investigate the presence of interaction terms
#' that have not been included in the regression model in the first place.
#' The function shows how the estimates of `term_chosen` vary under reweighting of all other
#' regressors as in `coef_rwgt` and compares them with the original estimates
#' from `mod_fit`.
#'
#' @param mod_fit An object of class `lm` or `glm` to fit on the data. This object
#' should contain the formula, the data, and, in case of `glm`, the family.
#' @param coef_rwgt A tibble containing the number of the bootstrapped data set (`b`),
#' the size of each bootstrapped data set (`m`),
#' the value of the reweighting centers (`term_rwgt_center`) and of the reweighted term (`term_rwgt`),
#' and the estimates of the regression coefficients (`term` and `estimate`).
#' @param term_chosen Character corresponding to the coefficient to be analysed.
#'
#' @return A ggplot2 object which shows how the coefficient of one regressor of interest (`term_chosen`)
#' varies under reweighting of the regressors.
#' The vertical axes indicate the magnitude of the estimates of the coefficient of the regressor of interest.
#' Horizontal axes and panels titles show the values and names of the regressors respectively.
#' Grey lines correspond to the traces of bootstrapped estimates forming the "spaghetti plot".
#' The black vertical lines indicate 95% confidence intervals computed via the percentile method
#' for the estimates on the
#' bootstrapped data sets for each of the centers of reweighting (term_rwgt_center).
#' The black line in the middle corresponds to the mean of the estimates (which is
#' approximately equal to the estimates on the original reweighted data).
#' The blue dashed lines correspond exactly to the original estimate of the coefficients from `mod_fit`.
#'
#' @export
#'
#' @importFrom Rdpack reprompt
#' @importFrom rlang .data
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
#' focal_slope(mod_fit, ols_rwgt, "X1")
#' }
focal_slope <- function(mod_fit, coef_rwgt, term_chosen) {
  coef_rwgt_conf_int <- comp_conf_int_bootstrap(
    boot_out = coef_rwgt,
    probs = c(0.025, 0.975),
    group_vars = c("term", "term_rwgt", "term_rwgt_center")
  ) %>%
    tidyr::pivot_wider(
      data = .,
      names_from = .data$q,
      values_from = .data$x,
      names_prefix = "q_"
    ) %>%
    dplyr::filter(
      .data = .,
      .data$term == term_chosen
    )

  coef_rwgt <- coef_rwgt %>%
    tidyr::unnest(
      data = .,
      .data$boot_out
    ) %>%
    dplyr::filter(
      .data = .,
      .data$term == term_chosen
    )

  coef_rwgt_means <- coef_rwgt %>%
    dplyr::group_by(
      .data = .,
      .data$term_rwgt,
      .data$term_rwgt_center
    ) %>%
    dplyr::summarise(
      .data = .,
      estimate = mean(.data$estimate),
      .groups = "keep"
    )

  p_spaghetti <- coef_rwgt %>%
    ggplot2::ggplot(
      data = .,
      mapping = ggplot2::aes(.data$term_rwgt_center,
        .data$estimate,
        group = .data$b
      )
    ) +
    ggplot2::geom_line(alpha = 0.3, col = "grey") +
    ggplot2::labs(x = "Value of regressor", y = paste("Coeffient of", term_chosen)) +
    ggplot2::facet_wrap(~term_rwgt, ncol = 3, scales = "free_x")

  p_confint <- coef_rwgt_conf_int %>%
    ggplot2::geom_segment(
      data = .,
      mapping = ggplot2::aes(
        x = .data$term_rwgt_center,
        xend = .data$term_rwgt_center,
        y = .data$q_0.025,
        yend = .data$q_0.975,
        group = NULL
      )
    )

  p_meanest <- coef_rwgt_means %>%
    ggplot2::geom_line(
      data = .,
      mapping = ggplot2::aes(
        x = .data$term_rwgt_center,
        y = .data$estimate,
        group = NULL
      )
    )

  p_oriest <- broom::tidy(mod_fit) %>%
    dplyr::filter(
      .data = .,
      .data$term == term_chosen
    ) %>%
    ggplot2::geom_hline(
      data = .,
      mapping = ggplot2::aes(
        yintercept = .data$estimate,
        group = NULL
      ),
      linetype = "dashed",
      col = "blue"
    )
  out <- (p_spaghetti + p_confint + p_meanest + p_oriest) %>%
    set_ggplot2_theme(ggplot_obj = .)

  return(out)
}


#' Obtain the `nonlinearity_detection` model diagnostic.
#'
#' Obtain the `nonlinearity_detection` model diagnostic as described in
#' \insertCite{@see @buja2019modelsasapproximationspart2;textual}{maar}.
#' This tool provides insights into marginal nonlinear behavior of response surfaces.
#' @details Through this tool, one can investigate the presence of interaction terms
#' that have not been included in the regression model in the first place.
#' The function shows how the estimates of each regression coefficient vary
#' under reweighting of their own regressors and compares them with the original estimates
#' from `mod_fit`.
#'
#' @param mod_fit An object of class `lm` or `glm` to fit on the data. This object
#' should contain the formula, the data, and, in case of `glm`, the family.
#' @param coef_rwgt A tibble containing the number of the bootstrapped data set (`b`),
#' the size of each bootstrapped data set (`m`),
#' the value of the reweighting centers (`term_rwgt_center`) and of the reweighted term (`term_rwgt`),
#' and the estimates of the regression coefficients (`term` and `estimate`).
#'
#'
#' #' @return A ggplot2 object which shows how coefficients estimates vary
#' under reweighting of their own regressors.
#' The vertical axes represent the the estimates of the coefficient of the regressors reweighted,
#' whose names appear in the panels titles.
#' Horizontal axes shows the values and names of the regressors.
#' Grey lines correspond to the traces of bootstrapped estimates forming the "spaghetti plot".
#' The black vertical lines indicate 95% confidence intervals computed via the percentile method
#' for the estimates on the
#' bootstrapped data sets for each of the centers of reweighting (`term_rwgt_center`).
#' The black line in the middle corresponds to the mean of the estimates (which is
#' approximately equal to the estimates on the original reweighted data).
#' The blue dashed lines correspond exactly to the original estimate of the coefficients from `mod_fit`.
#'
#' @export
#'
#' @importFrom Rdpack reprompt
#' @importFrom rlang .data
#'
#' @references \insertAllCited{}
#'
#' @examples
#' \dontrun{
#' # Get nonlinearity detection plot
#' n <- 1e3
#' X1 <- stats::rnorm(n, 0, 1)
#' X2 <- stats::rnorm(n, 0, 3)
#' y <- 2 + X1 + X2 * 0.3 + stats::rnorm(n, 0, 1)
#' df <- tibble::tibble(y = y, X1 = X1, X2 = X2, n_obs = 1:length(X1))
#' mod_fit <- stats::lm(y ~ X1 + X2, df)
#' ols_rwgt <- comp_coef_rwgt(mod_fit, c("X1", "X2"), B = 300)
#'
#' # Display the output
#' nonlinearity_detection(mod_fit, ols_rwgt)
#' }
nonlinearity_detection <- function(mod_fit, coef_rwgt) {
  coef_rwgt_conf_int <- comp_conf_int_bootstrap(
    boot_out = coef_rwgt,
    probs = c(0.025, 0.975),
    group_vars = c("term", "term_rwgt", "term_rwgt_center")
  ) %>%
    tidyr::pivot_wider(
      data = .,
      names_from = .data$q,
      values_from = .data$x,
      names_prefix = "q_"
    ) %>%
    dplyr::filter(
      .data = .,
      term == .data$term_rwgt
    )

  coef_rwgt <- coef_rwgt %>%
    tidyr::unnest(
      data = .,
      .data$boot_out
    ) %>%
    dplyr::filter(
      .data = .,
      term == .data$term_rwgt
    )

  coef_rwgt_means <- coef_rwgt %>%
    dplyr::group_by(.data = ., .data$term_rwgt, .data$term_rwgt_center) %>%
    dplyr::summarise(
      .data = .,
      estimate = mean(.data$estimate),
      .groups = "keep"
    )

  p_spaghetti <- coef_rwgt %>%
    ggplot2::ggplot(
      data = .,
      mapping = ggplot2::aes(.data$term_rwgt_center,
        .data$estimate,
        group = .data$b
      )
    ) +
    ggplot2::geom_line(alpha = 0.3, col = "grey") +
    ggplot2::labs(x = "Value of regressor", y = paste("Coefficient")) +
    ggplot2::facet_wrap(~term_rwgt, ncol = 3, scales = "free")

  p_confint <- coef_rwgt_conf_int %>%
    ggplot2::geom_segment(
      data = .,
      mapping = ggplot2::aes(
        x = .data$term_rwgt_center,
        xend = .data$term_rwgt_center,
        y = .data$q_0.025,
        yend = .data$q_0.975,
        group = NULL
      )
    )

  p_meanest <- coef_rwgt_means %>%
    ggplot2::geom_line(
      data = .,
      mapping = ggplot2::aes(
        x = .data$term_rwgt_center,
        y = .data$estimate,
        group = NULL
      )
    )

  p_oriest <- broom::tidy(mod_fit) %>%
    dplyr::rename(
      .data = .,
      term_rwgt = term
    ) %>%
    dplyr::inner_join(
      x = .,
      y = coef_rwgt %>%
        dplyr::select(
          .data = .,
          .data$term_rwgt
        ),
      by = "term_rwgt"
    ) %>%
    ggplot2::geom_hline(
      data = .,
      mapping = ggplot2::aes(
        yintercept = .data$estimate,
        group = NULL
      ),
      col = "blue",
      linetype = "dashed"
    )

  out <- (p_spaghetti + p_confint + p_meanest + p_oriest) %>%
    set_ggplot2_theme(ggplot_obj = .)

  return(out)
}

#' Obtain the "focal reweighting variable" model diagnostic.
#'
#' Obtain the "focal reweighting variable"  model diagnostic as described in
#' \insertCite{@see @buja2019modelsasapproximationspart2;textual}{maar}.
#' @details The function shows how the estimates of all regressors vary under reweighting
#' of one regressor, specified in `term_chosen`, and compare with the original
#' estimates from `mod_fit`.
#'
#' @param mod_fit An object of class `lm` or `glm` to fit on the data. This object
#' should contain the formula, the data, and, in case of `glm`, the family.
#' @param coef_rwgt A tibble containing the number of the bootstrapped data set (`b`),
#' the size of each bootstrapped data set (`m`),
#' the value of the reweighting centers (`term_rwgt_center`) and of the reweighted term (`term_rwgt`),
#' and the estimates of the regression coefficients (`term` and `estimate`).
#' @param term_chosen Character corresponding to the coefficient to be analysed.
#'
#' @return A ggplot2 object which shows how the coefficients estimates
#' vary under reweighting of the regressor `term_chosen`.
#' The vertical axes represent the estimates of the coefficient of the regressors,
#' whose names appear in the panels titles, under reweighting of `term_chosen`.
#' Horizontal axes shows the values of the regressors.
#' Grey lines correspond to the traces of bootstrapped estimates forming the "spaghetti plot".
#' The black vertical lines indicate 95% confidence intervals computed via the percentile method
#' for the estimates on the
#' bootstrapped data sets for each of the centers of reweighting (`term_rwgt_center`).
#' The black line in the middle corresponds to the mean of the estimates (which is
#' approximately equal to the estimates on the original reweighted data).
#' The blue dashed lines correspond exactly to the original estimate of the coefficients from `mod_fit`.
#'
#' @export
#'
#' @importFrom Rdpack reprompt
#'
#' @references \insertAllCited{}
#'
#' @examples
#' \dontrun{
#' # Get focal reweighting variable plot of X1
#' n <- 1e3
#' X1 <- stats::rnorm(n, 0, 1)
#' X2 <- stats::rnorm(n, 0, 3)
#' y <- 2 + X1 + X2 * 0.3 + stats::rnorm(n, 0, 1)
#' df <- tibble::tibble(y = y, X1 = X1, X2 = X2, n_obs = 1:length(X1))
#' mod_fit <- stats::lm(y ~ X1 + X2, df)
#' ols_rwgt <- comp_coef_rwgt(mod_fit, c("X1", "X2"), B = 300)
#'
#' # Display the output
#' focal_rwgt_var(mod_fit, ols_rwgt, "X1")
#' }
#'
focal_rwgt_var <- function(mod_fit, coef_rwgt, term_chosen) {
  coef_rwgt_conf_int <- comp_conf_int_bootstrap(
    boot_out = coef_rwgt,
    probs = c(0.025, 0.975),
    group_vars = c("term", "term_rwgt", "term_rwgt_center")
  ) %>%
    tidyr::pivot_wider(
      data = .,
      names_from = .data$q,
      values_from = .data$x,
      names_prefix = "q_"
    ) %>%
    dplyr::filter(.data$term_rwgt == term_chosen)

  coef_rwgt <- coef_rwgt %>%
    tidyr::unnest(
      data = .,
      .data$boot_out
    ) %>%
    dplyr::filter(
      .data = .,
      .data$term_rwgt == term_chosen
    )

  coef_rwgt_means <- coef_rwgt %>%
    dplyr::group_by(
      .data = .,
      .data$term_rwgt,
      .data$term,
      .data$term_rwgt_center
    ) %>%
    dplyr::summarise(
      .data = .,
      estimate = mean(.data$estimate),
      .groups = "keep"
    )

  p_spaghetti <- coef_rwgt %>%
    ggplot2::ggplot(
      data = .,
      mapping = ggplot2::aes(.data$term_rwgt_center,
        .data$estimate,
        group = .data$b
      )
    ) +
    ggplot2::geom_line(alpha = 0.3, col = "grey") +
    ggplot2::labs(x = term_chosen, y = "Coeffient") +
    ggplot2::facet_wrap(~term, ncol = 3, scales = "free")

  p_confint <- coef_rwgt_conf_int %>%
    ggplot2::geom_segment(
      data = .,
      mapping = ggplot2::aes(
        x = .data$term_rwgt_center,
        xend = .data$term_rwgt_center,
        y = .data$q_0.025,
        yend = .data$q_0.975,
        group = NULL
      )
    )

  p_meanest <- coef_rwgt_means %>%
    ggplot2::geom_line(
      data = .,
      mapping = ggplot2::aes(
        x = .data$term_rwgt_center,
        y = .data$estimate,
        group = NULL
      )
    )

  p_oriest <- broom::tidy(x = mod_fit) %>%
    dplyr::inner_join(
      x = .,
      y = coef_rwgt %>%
        dplyr::select(
          .data = .,
          .data$term
        ),
      by = "term"
    ) %>%
    ggplot2::geom_hline(
      data = .,
      mapping = ggplot2::aes(
        yintercept = .data$estimate,
        group = NULL
      ),
      col = "blue",
      linetype = "dashed"
    )

  out <- (p_spaghetti + p_confint + p_meanest + p_oriest) %>%
    set_ggplot2_theme(ggplot_obj = .)

  return(out)
}

