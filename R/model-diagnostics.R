#' Create grid of centers for the reweighting of the regressor's distribution
#'
#' \code{gen_grid_cent_rwgt} returns a grid of values based on the empirical
#' distribution of the regressor in the data.
#'
#' @details The function creates either a grid of evenly spaced values between
#'   the minimum and the maximum of the regressor's values or based on the
#'   quantiles. If the construction of the grid is based on the quantiles,
#'   then the values of the grid consists of the quantiles corresponding
#'   to the probabilities \code{seq(0.1,0.9,length=n_grid)}.
#'
#' @param x A vector of values taken by the regressor.
#' @param grid_method A character indicating the method to use for the
#'   construction of the grid of reweighting centers.
#'   The grid consists either of evenly spaced values between the maximum and
#'   the minimum (\code{grid_method='regular'}) or based on the quantiles
#'   between the second and the tenth deciles (\code{grid_method='quantiles'}).
#' @param n_grid An integer indicating the number of reweighting centers
#'   present in the grid.
#'
#' @return A vector of values corresponding to the centers for the reweighting.
#'
#' @keywords internal
#'
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' # Construct a grid of centers for the reweighting of the regressor
#' set.seed(162632)
#' n <- 100
#' x <- rnorm(n, 0, 1)
#' centers <- gen_grid_cent_rwgt(x, "quantiles", 9)
#'
#' # Display the output
#' print(centers)
#' }
gen_grid_cent_rwgt <- function(x, grid_method, n_grid) {
  assertthat::assert_that(grid_method == "regular" | grid_method == "quantiles",
    msg = glue::glue('The method to construct the grid needs to be either "regular" or "quantiles".')
  )
  assertthat::assert_that((n_grid > 1) & (n_grid %% 1 == 0),
    msg = glue::glue("The number of points in the grid needs to be an integer larger than 1.")
  )
  if (grid_method == "regular") {
    lb <- stats::quantile(x, probs = 0)
    ub <- stats::quantile(x, probs = 1)
    out <- seq(lb, ub, length = n_grid)
  } else if (grid_method == "quantiles") {
    out <- stats::quantile(x, probs = seq(0.1, 0.9, length = n_grid))
  }
  return(unname(out))
}


#' Obtain estimates for GLM of OLS under reweighting of one regressor
#'
#' \code{diag_fit_reg_rwgt_single} provides estimate for GLM and OLS under
#' reweighting of one regressor. The function returns a set of estimates
#' for each bootstrapped data set and each center of reweighting.
#'
#' @details The GLM or OLS model extracted from \code{mod_fit} is fitted on each
#'   bootstrapped data set in \code{boot_samples} under the reweighting of every
#'   reweighting center in \code{term_to_rwgt_centers} of the regressor
#'   specified by \code{term_to_rwgt}. The weight for the j-th regressor
#'   with center \eqn{c_{k}(j)} and the i-th observation is proportional to
#'   \eqn{\exp\{-(X(j)_i - X(j))^2 / (2 * \hat{\sigma}(X(j))))\}} where
#'   \eqn{\hat{\sigma}} is the standard deviation of \eqn{X(j)}.
#'
#' @param mod_fit An object of class \code{\link[stats]{lm}} or \code{\link[stats]{glm}} to fit on the data.
#'   This object should contain the formula, the data, and, in case of
#'   \code{\link[stats]{glm}}, the family.
#' @param term_to_rwgt A character corresponding to the regressor to be
#'   reweighted.
#' @param boot_samples A list of bootstrapped data sets. Each data set in the
#'   column "data" must
#'   include a column "n_obs" which contains the order of each observation as
#'   in the original data.
#' @param term_to_rwgt_centers A vector of numeric values corresponding to the
#'   centers used for the reweighting.
#'
#' @return A tibble containing the number of the bootstrapped data set
#'   (\code{b}), the value of the reweighting centers (\code{term_rwgt_center}),
#'   and estimates of the regression coefficients (\code{term} and
#'   \code{estimate}).
#'
#' @keywords internal
#'
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' set.seed(12323)
#' # Get OLS estimates under reweighting of regressor X1
#' n <- 1e3
#' X1 <- stats::rnorm(n, 0, 1)
#' X2 <- stats::rnorm(n, 0, 3)
#' y <- 2 + X1 + X2 * 0.3 + stats::rnorm(n, 0, 1)
#' df <- tibble::tibble(y = y, X1 = X1, X2 = X2, n_obs = 1:length(X1))
#' mod_fit <- stats::lm(y ~ X1 + X2, df)
#' boot_samples <- comp_boot_emp_samples(df, B = 100)
#' ols_rwgt_X1 <- diag_fit_reg_rwgt_single(mod_fit, "X1", boot_samples, c(-1, 0, 1, 2))
#'
#' # Display the output
#' print(ols_rwgt_X1)
#' }
diag_fit_reg_rwgt_single <- function(mod_fit, term_to_rwgt, boot_samples, term_to_rwgt_centers) {
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
      ~ fit_reg(
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

#' Obtain estimates of GLM or OLS under reweighting of one or
#' multiple regressors
#'
#' \code{diag_fit_reg_rwgt} returns estimates for GLM and OLS under reweighting of
#' multiple regressors. The function estimates a set of coefficients for several
#' configurations of the reweighting of the reweighting of each regressor.
#'
#' @details The model extracted from \code{mod_fit} is fitted on \code{B}
#'   data sets sampled via \code{m}-out-of-n empirical bootstrap
#'   using weighted regression where the predictors to be reweighted are
#'   specified in \code{terms_to_rwgt} and have reweighting centers given by
#'   \code{grid_centers}.
#'   Using the default parameters, the function will compute the estimates for
#'   a grid based on the second to first to the ninth deciles of each regressor.
#'
#' @param mod_fit An object of class \code{\link[stats]{lm}} or \code{\link[stats]{glm}} to fit on the data.
#'   This object should contain the formula, the data, and, in case of
#'   \code{\link[stats]{glm}}, the family.
#' @param terms_to_rwgt A vector of characters corresponding to the names of the
#'   regressors to be reweighted.
#' @param B An integer corresponding to the number of bootstrap repetitions or
#'   number of bootstrap samples to be drawn. Default is set to \code{100}.
#' @param m An integer corresponding to the number of observations to be sampled
#'   with replacement from the data set in each bootstrap repetition.
#'   Default is set to the size of the data set.
#' @param grid_centers A data frame containing the names of the regressors as
#'   columns and the corresponding reweighting centers.
#'   Each column corresponds to a different regressor, whose name is specified
#'   in the name of the column. Default is set to \code{NULL}.
#' @param grid_method A chracter which specifies the method used to construct
#'   the grid of reweighting centers. The grid can consist either of evenly
#'   spaced values between the maximum and the minimum
#'   (\code{grid_method='regular'}) or based on the quantiles between the first
#'   and the ninth deciles (\code{grid_method='quantiles'}). Default is set
#'   to \code{'quantile'}.
#' @param n_grid An integer corresponding to the number of reweighting centers
#'   for the grid. Default is set to \code{9}.
#'
#'
#' @return A tibble containing the number of the bootstrapped data set (\code{b}),
#'   the size of each bootstrapped data set (\code{m}),
#'   the value of the reweighting centers (\code{term_rwgt_center}) and the name of
#'   the regressor under reweighting (\code{term_rwgt}), and the estimates of the
#'   regression coefficients (\code{term} and \code{estimate}).
#'
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' set.seed(1321312)
#' # Get OLS estimates under reweighting of all regressors with default grid
#' n <- 1e3
#' X1 <- stats::rnorm(n, 0, 1)
#' X2 <- stats::rnorm(n, 0, 3)
#' y <- 2 + X1 + X2 * 0.3 + stats::rnorm(n, 0, 1)
#' df <- tibble::tibble(y = y, X1 = X1, X2 = X2, n_obs = 1:length(X1))
#' mod_fit <- stats::lm(y ~ X1 + X2, df)
#' ols_rwgt <- diag_fit_reg_rwgt(mod_fit, c("X1", "X2"))
#'
#' # Display the output
#' print(ols_rwgt)
#'
#' # Get OLS estimates under reweighting of all regressors by feeding grid of centers into the function
#' ols_rwgt_grid_fixed <- diag_fit_reg_rwgt(mod_fit, "X1", grid_centers = data.frame(X1 = c(0, 1)))
#'
#' #' # Display the output
#' print(ols_rwgt_grid_fixed)
#' }
diag_fit_reg_rwgt <- function(mod_fit,
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
  check_fn_args(B=B, m=m)

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

  boot_samples <- comp_boot_emp_samples(
    data = data %>% tibble::add_column(n_obs = 1:nrow(data)),
    B = B,
    m = m
  )

  if (is.null(grid_centers)) {
    grid_centers <- data %>%
      dplyr::summarise_all(function(x) {
        gen_grid_cent_rwgt(x,
          grid_method = grid_method,
          n_grid = n_grid
        )
      })
  }

  out <- terms_to_rwgt %>%
    purrr::map(~ diag_fit_reg_rwgt_single(
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



#' Obtain the "focal slope" model diagnostic
#'
#' \code{diag_foc_slope} returns the "focal slope" model diagnostics described in
#' \insertCite{@see @buja2019modelsasapproximationspart2;textual}{maars}.
#' This graphical tool provides insights into the interactions between the
#' regressor specified in \code{term_chosen} and all other regressors.
#' More specifically, based on the estimates under reweighting of the regressors
#' returned by \code{\link{diag_fit_reg_rwgt}} and specified in \code{coef_rwgt}, this
#' function shows how the estimates of \code{term_chosen} vary under
#' reweighting of all other regressors and compares them with the original
#' estimates from \code{mod_fit}.
#'
#' @param mod_fit An object of class \code{\link[stats]{lm}} or \code{\link[stats]{glm}} to fit on the data.
#'   This object should contain the formula, the data, and, in case of
#'   \code{\link[stats]{glm}}, the family.
#' @param coef_rwgt A tibble containing the number of the bootstrapped data set
#'   (\code{b}), the size of each bootstrapped data set (\code{m}), the value of
#'   the reweighting centers (\code{term_rwgt_center}) and of the reweighted
#'   term (\code{term_rwgt}), and the estimates of the regression coefficients
#'   (\code{term} and \code{estimate}). This tibble can be created via the
#'   \code{diag_fit_reg_rwgt} function.
#' @param term_chosen A character corresponding to the coefficient of interest
#'   to be analysed.
#'
#' @return A ggplot2 object which shows how the coefficient of one regressor of
#'   interest (\code{term_chosen}) varies under reweighting of the regressors.
#'   The vertical axis indicates the magnitude of the estimates of the coefficient
#'   of the regressor of interest. The horizontal axis and panels titles show the
#'   values and names of the regressors respectively. The grey
#'   lines correspond to the traces of bootstrapped estimates forming the
#'   "spaghetti plot". The black vertical lines indicate 95% confidence
#'   intervals computed via the percentile method for the estimates on the
#'   bootstrapped data sets for the estimates based on each of the centers of
#'   reweighting (\code{term_rwgt_center}). The black line in the middle
#'   corresponds to the mean of the estimates and is approximately equal to the
#'   estimates on the original data (under reweighting). The blue dashed lines
#'   correspond exactly to the original estimate of the coefficients from
#'   \code{mod_fit}.
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
#' set.seed(13123412)
#' # Get focal slope of X1
#' n <- 1e3
#' X1 <- stats::rnorm(n, 0, 1)
#' X2 <- stats::rnorm(n, 0, 3)
#' y <- 2 + X1 + X2 * 0.3 + stats::rnorm(n, 0, 1)
#' df <- tibble::tibble(y = y, X1 = X1, X2 = X2, n_obs = 1:length(X1))
#' mod_fit <- stats::lm(y ~ X1 + X2, df)
#' ols_rwgt <- diag_fit_reg_rwgt(mod_fit, c("X1", "X2"), B = 300)
#'
#' # Display the output
#' diag_foc_slope(mod_fit, ols_rwgt, "X1")
#' }
diag_foc_slope <- function(mod_fit, coef_rwgt, term_chosen) {
  coef_rwgt_conf_int <- comp_ci_boot(
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
    set_mms_ggplot2_theme(ggplot_obj = .)

  return(out)
}


#' Obtain the "nonlinearity detection" model diagnostic
#'
#' \code{diag_nl_detect} returns the "nonlinearity detection" model
#' diagnostic as described in
#' \insertCite{@see @buja2019modelsasapproximationspart2;textual}{maars}.
#' This graphical tool provides insights into the marginal nonlinear behavior
#' of response surfaces for each of the regressors.
#' More specifically, based on the estimates under reweighting of the regressors
#' returned by \code{\link{diag_fit_reg_rwgt}} and specified in \code{coef_rwgt}, this
#' function shows how the estimate of each coefficient varies under
#' reweighting of its own regressor and compares it with the original
#' estimates from \code{mod_fit}.
#'
#' @param mod_fit An object of class \code{\link[stats]{lm}} or \code{\link[stats]{glm}} to fit on the data.
#'   This object should contain the formula, the data, and, in case of
#'   \code{\link[stats]{glm}}, the family.
#' @param coef_rwgt A tibble containing the number of the bootstrapped data set
#'   (\code{b}), the size of each bootstrapped data set (\code{m}), the value of
#'   the reweighting centers (\code{term_rwgt_center}) and of the reweighted
#'   term (\code{term_rwgt}), and the estimates of the regression coefficients
#'   (\code{term} and \code{estimate}). This tibble can be created via the
#'   \code{\link{diag_fit_reg_rwgt}} function.
#'
#'
#' @return A ggplot2 object which shows how the estimates of all coefficients
#'   vary under reweighting of their own regressors.
#'   The vertical axis represents the estimates of the coefficients under
#'   reweighting of tthe one regressor, whose names appear in the panels
#'   titles. The horizontal axis shows the values of the regressors. The grey
#'   lines correspond to the traces of bootstrapped estimates forming the
#'   "spaghetti plot". The black vertical lines indicate 95% confidence
#'   intervals computed via the percentile method for the estimates on the
#'   bootstrapped data sets for the estimates based on each of the centers of
#'   reweighting (\code{term_rwgt_center}). The black line in the middle
#'   corresponds to the mean of the estimates and is approximately equal to the
#'   estimates on the original data (under reweighting). The blue dashed lines
#'   correspond exactly to the original estimate of the coefficients from
#'   \code{mod_fit}.
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
#' set.seed(1332423)
#' # Get nonlinearity detection plot
#' n <- 1e3
#' X1 <- stats::rnorm(n, 0, 1)
#' X2 <- stats::rnorm(n, 0, 3)
#' y <- 2 + X1 + X2 * 0.3 + stats::rnorm(n, 0, 1)
#' df <- tibble::tibble(y = y, X1 = X1, X2 = X2, n_obs = 1:length(X1))
#' mod_fit <- stats::lm(y ~ X1 + X2, df)
#' ols_rwgt <- diag_fit_reg_rwgt(mod_fit, c("X1", "X2"), B = 300)
#'
#' # Display the output
#' diag_nl_detect(mod_fit, ols_rwgt)
#' }
diag_nl_detect <- function(mod_fit, coef_rwgt) {
  coef_rwgt_conf_int <- comp_ci_boot(
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
    set_mms_ggplot2_theme(ggplot_obj = .)

  return(out)
}

#' Obtain the "focal reweighting variable" model diagnostic.
#'
#' \code{diag_foc_rwgt} returns the "focal reweighting variable" model
#' diagnostic described in
#' \insertCite{@see @buja2019modelsasapproximationspart2;textual}{maars}.
#' More specifically, based on the estimates under reweighting of the regressors
#' returned by \code{diag_fit_reg_rwgt} and specified in \code{coef_rwgt}, this
#' function shows how the estimates of all coefficients vary under
#' reweighting of only one regressor specified in \code{term_chosen} and
#' compares them with the original estimates from \code{mod_fit}.
#'
#' @param mod_fit An object of class \code{\link[stats]{lm}} or \code{\link[stats]{glm}} to fit on the data.
#'   This object should contain the formula, the data, and, in case of
#'   \code{\link[stats]{glm}}, the family.
#' @param coef_rwgt A tibble containing the number of the bootstrapped data set
#'   (\code{b}), the size of each bootstrapped data set (\code{m}), the value of
#'   the reweighting centers (\code{term_rwgt_center}) and of the reweighted
#'   term (\code{term_rwgt}), and the estimates of the regression coefficients
#'   (\code{term} and \code{estimate}). This tibble can be created via the
#'   \code{\link{diag_fit_reg_rwgt}} function.
#' @param term_chosen A character corresponding to the coefficient of interest
#'   to be analysed.
#'
#' @return A ggplot2 object which shows how coefficients estimates vary
#'   under reweighting of only one regressor (\code{term_chosen}).
#'   The vertical axis represents the estimates of the coefficient under
#'   reweighting of their own regressors, whose names appear in the panels
#'   titles. The horizontal axis shows the values of the regressors. The grey
#'   lines correspond to the traces of bootstrapped estimates forming the
#'   "spaghetti plot". The black vertical lines indicate 95% confidence
#'   intervals computed via the percentile method for the estimates on the
#'   bootstrapped data sets for the estimates based on each of the centers of
#'   reweighting (\code{term_rwgt_center}). The black line in the middle
#'   corresponds to the mean of the estimates and is approximately equal to the
#'   estimates on the original data (under reweighting). The blue dashed lines
#'   correspond exactly to the original estimate of the coefficients from
#'   \code{mod_fit}.
#'
#' @export
#'
#' @importFrom Rdpack reprompt
#'
#' @references \insertAllCited{}
#'
#' @examples
#' \dontrun{
#' set.seed(1232312)
#' # Get focal reweighting variable plot of X1
#' n <- 1e3
#' X1 <- stats::rnorm(n, 0, 1)
#' X2 <- stats::rnorm(n, 0, 3)
#' y <- 2 + X1 + X2 * 0.3 + stats::rnorm(n, 0, 1)
#' df <- tibble::tibble(y = y, X1 = X1, X2 = X2, n_obs = 1:length(X1))
#' mod_fit <- stats::lm(y ~ X1 + X2, df)
#' ols_rwgt <- diag_fit_reg_rwgt(mod_fit, c("X1", "X2"), B = 300)
#'
#' # Display the output
#' diag_foc_rwgt(mod_fit, ols_rwgt, "X1")
#' }
#'
diag_foc_rwgt <- function(mod_fit, coef_rwgt, term_chosen) {
  coef_rwgt_conf_int <- comp_ci_boot(
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
    set_mms_ggplot2_theme(ggplot_obj = .)

  return(out)
}

