#' Create grid of centers for reweighting the distribution of the regressor
#'
#' Create grid of centers for reweighting the distribution of the regressor.
#' The function creates either a grid of evenly spaced values between the
#' minimum and the maximum of the regressor's values or based on the quantiles.
#'
#' @param x Vector of values of the regressor
#' @param grid_method Method to construct the grid
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
  if (grid_method == "evenly spaced") {
    lb <- stats::quantile(x, probs = 0)
    ub <- stats::quantile(x, probs = 1)
    out <- base::seq(lb, ub, length = n_grid)
  } else if (grid_method == "quantiles") {
    out <- stats::quantile(x, probs = base::seq(0.1, 0.9, length = n_grid))
  }
  return(unname(out))
}


#' Fit OLS or GLM on the data
#'
#' Fit OLS or GLM on the data
#'
#' @param mod_fit An object of class "lm" or "glm" to fit on the data. This object
#' should contain the formula, the data, and, in case of "glm", the family
#' @param data A tibble or data frame containing the data set on which the model
#' will be trained
#' @param weights A character corresponding to the name of the weights
#' feature name in the data
#'
#' @return A tibble containing the estimated coefficients of the model
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Estimate OLS from a data set with weights
#' n <- 1e3
#' X <- stats::rnorm(n, 0, 1)
#' y <- 2 + X * 1 + stats::rnorm(n, 0, 1)
#' mod_fit <- stats::lm(y ~ X)
#' df <- tibble::tibble(y = y, X = X, weights = 1:length(X))
#' mod <- comp_cond_model(mod_fit, df, 'weights')
#'
#' # Display the output
#' print(mod)
#' }
comp_cond_model <- function(mod_fit, data, weights=NULL){
  if (all("lm" == class(mod_fit))) {
    if(is.null(weights)){
      out <- stats::lm(formula = stats::formula(mod_fit), data = data)
    } else{
      out <- stats::lm(formula = stats::formula(mod_fit), data = data, weights = weights)
    }
  } else {
    if(is.null(weights)){
      out <- stats::glm(formula = stats::formula(mod_fit), data = data, family = stats::family(mod_fit))
    } else{
      out <- stats::glm(formula = stats::formula(mod_fit), data = data, family = stats::family(mod_fit), weights = weights)
    }
  }
  out <- tibble::tibble(term = names(stats::coef(out)), estimate = stats::coef(out))
  return(out)
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
#' y <- 2 + X1 + X2*0.3 + stats::rnorm(n, 0, 1)
#' df <- tibble::tibble(y = y, X1 = X1, X2 = X2, n_obs = 1:length(X1))
#' mod_fit <- stats::lm(y ~ X1+X2, df)
#' boot_samples <- comp_empirical_bootstrap_samples(df, B = 100)
#' ols_rwgt_X1 <- comp_coef_rwgt_single(mod_fit, 'X1', boot_samples, c(-1,0,1,2))
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
#' @param terms_to_rwgt A character corresponding to the regressor to be reweighted
#' @param boot_samples A list of data sets that have been bootstrapped from the
#' original data set. Each data set needs to contain a column "n_obs" which indicates
#' the order of each observation in the original data
#' @param term_to_rwgt_centers A vector of numeric values corresponding to the
#' centers for the reweighting procedure
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
#' # Get OLS estimates under reweighting of all regressors
#' n <- 1e3
#' X1 <- stats::rnorm(n, 0, 1)
#' X2 <- stats::rnorm(n, 0, 3)
#' y <- 2 + X1 + X2*0.3 + stats::rnorm(n, 0, 1)
#' df <- tibble::tibble(y = y, X1 = X1, X2 = X2, n_obs = 1:length(X1))
#' mod_fit <- stats::lm(y ~ X1+X2, df)
#' ols_rwgt <- comp_coef_rwgt(mod_fit, c('X1', 'X2'))
#'
#' # Display the output
#' print(ols_rwgt)
#' }
comp_coef_rwgt <- function(mod_fit,
                           terms_to_rwgt,
                           B = 100,
                           m = NULL,
                           grid_bins_centers = NULL,
                           grid_method = "quantiles",
                           n_grid = 9) {
  data <- model.frame(mod_fit)
  terms_to_rwgt <- as.list(terms_to_rwgt)
  names(terms_to_rwgt) <- unlist(terms_to_rwgt)

  if(is.null(m)){m <- nrow(data)}

  boot_samples <- maar::comp_empirical_bootstrap_samples(
    data = data %>% tibble::add_column(n_obs = 1:nrow(data)),
    B = B,
    m = m)

  if (is.null(grid_bins_centers)) {
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
#' Vertical axis = regression coefficient of PercVacant (in all frames);
#' horizontal axes and panels titles = regressors.
#' The vertical axis corresponds the size of the coefficient of
#' the regressor of interest.
#' Grey lines correspond to the traces of bootstrapped estimates forming the "spaghetti plot".
#' The black vertical lines indicate 95% confidence intervals for the estimates on the
#' bootstrapped data sets for each of the centers of reweighting (term_rwgt_center).
#' The black horizontal line corresponds to the medians of the estimates.
#' The text in the top left corner of each panel shows the results
#' of a t-test for testing the statistical significance of the difference of the means
#' of the distributions of boostrap estimates between the two most extreme centers.
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
#' y <- 2 + X1 + X2*0.3 + stats::rnorm(n, 0, 1)
#' df <- tibble::tibble(y = y, X1 = X1, X2 = X2, n_obs = 1:length(X1))
#' mod_fit <- stats::lm(y ~ X1+X2, df)
#' ols_rwgt <- comp_coef_rwgt(mod_fit, c('X1', 'X2'), B=300)
#'
#' # Display the output
#' focal_slope(ols_rwgt, 'X1')
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

  tilt_test <- coef_rwgt %>%
    dplyr::group_by(term_rwgt) %>%
    dplyr::mutate(center = dplyr::case_when(
      term_rwgt_center == max(term_rwgt_center) ~ "Max_center",
      term_rwgt_center == min(term_rwgt_center) ~ "Min_center"
    )) %>%
    tidyr::drop_na() %>%
    dplyr::select(term_rwgt, b, center, estimate) %>%
    tidyr::pivot_wider(names_from = center, values_from = estimate) %>%
    dplyr::group_by(term_rwgt) %>%
    dplyr::summarise(d = mean(Max_center - Min_center),
                     p = t.test(Max_center, Min_center)$p.value,
                     .groups = "keep") %>%
  dplyr::mutate(text_test = paste0("Tilt test: p=", round(p, 2), " d=", round(d, 2)))

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
    ) +
    ggplot2::geom_text(
      data = tilt_test %>% dplyr::mutate(term_rwgt_center = -Inf, estimate = Inf),
      ggplot2::aes(term_rwgt_center, estimate, label = text_test, group = NULL),
      hjust = 0, vjust = 1
    )
  return(out)
}


#
# nonlinearity_detection <- function(coef_rwgt) {
#   coef_rwgt_conf_int <- conf_int_boot(
#     boot_out = coef_rwgt,
#     probs = c(0.025, 0.5, 0.975),
#     grouping = c("term", "term_rwgt", "term_rwgt_center")
#   ) %>%
#     tidyr::pivot_wider(names_from = q, values_from = x) %>%
#     dplyr::filter(term == term_rwgt)
#
#   out <- coef_rwgt %>%
#     tidyr::unnest(boot_out) %>%
#     dplyr::filter(term == term_rwgt) %>%
#     ggplot2::ggplot(mapping = ggplot2::aes(term_rwgt_center, estimate, group = b)) +
#     ggplot2::geom_line(alpha = 0.3, col = "grey") +
#     ggplot2::theme_bw() +
#     ggplot2::labs(x = "Value of regressor", y = "Estimate of coefficient") +
#     ggplot2::facet_wrap(~term_rwgt, ncol = 3, scale = "free") +
#     ggplot2::geom_segment(
#       data = coef_rwgt_conf_int,
#       mapping = ggplot2::aes(
#         x = term_rwgt_center, xend = term_rwgt_center,
#         y = `0.025`, yend = `0.975`, group = NULL
#       )
#     ) +
#     ggplot2::geom_line(
#       data = coef_rwgt_conf_int,
#       mapping = ggplot2::aes(x = term_rwgt_center, y = `0.5`, group = NULL)
#     )
#   return(out)
# }
#
#
# focal_reweighting_variable <- function(coef_rwgt, term_chosen) {
#   coef_rwgt_conf_int <- conf_int_boot(
#     boot_out = coef_rwgt,
#     probs = c(0.025, 0.5, 0.975),
#     grouping = c("term", "term_rwgt", "term_rwgt_center")
#   ) %>%
#     tidyr::pivot_wider(names_from = q, values_from = x) %>%
#     dplyr::filter(term_rwgt == term_chosen)
#
#   out <- coef_rwgt %>%
#     tidyr::unnest(boot_out) %>%
#     dplyr::filter(term_rwgt == term_chosen) %>%
#     ggplot2::ggplot(mapping = ggplot2::aes(term_rwgt_center, estimate, group = b)) +
#     ggplot2::geom_line(alpha = 0.3, col = "grey") +
#     ggplot2::theme_bw() +
#     ggplot2::labs(x = term_chosen, y = "Estimate of coefficient") +
#     ggplot2::facet_wrap(~term, ncol = 3, scale = "free") +
#     ggplot2::geom_segment(
#       data = coef_rwgt_conf_int,
#       mapping = ggplot2::aes(
#         x = term_rwgt_center, xend = term_rwgt_center,
#         y = `0.025`, yend = `0.975`, group = NULL
#       )
#     ) +
#     ggplot2::geom_line(
#       data = coef_rwgt_conf_int,
#       mapping = ggplot2::aes(x = term_rwgt_center, y = `0.5`, group = NULL)
#     )
#   return(out)
# }




