#' Set the custom \code{\link[ggplot2]{theme}} for our plots
#'
#' \code{set_mms_ggplot2_theme} is a wrapper to set a standard custom
#' \code{\link[ggplot2]{theme}} for all of our output plots. It contains
#' only the necessary parameters we require to change in the
#' \code{\link[ggplot2]{theme}} function, and takes the remaining arguments
#' as the default values.
#'
#' @param ggplot_obj A \code{\link[ggplot2]{ggplot2}} object
#' @param axis_text_size A positive integer to define the the size of the
#'   x and y axis text in the \code{\link[ggplot2]{ggplot2}} object.
#' @param axis_title_size A positive integer to define the the size of the
#'   x and y axis title text in the \code{\link[ggplot2]{ggplot2}} object.
#' @param strip_text_x_size A positive integer to define the the size of the
#'   facet labels text in the \code{\link[ggplot2]{ggplot2}} object.
#' @param legend_text_size A positive integer to define the the size of the
#'   legend text in the \code{\link[ggplot2]{ggplot2}} object.
#' @param legend_title_size A positive integer to define the the size of the
#'   legend title text in the \code{\link[ggplot2]{ggplot2}} object.
#' @param legend_position A character value the position of legends
#' ("none", "left", "right", "bottom", "top", or two-element numeric vector)
#'
#' @return (\code{ggplot2}) : A \code{ggplot2} object with the custom theme
#'   applied
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' p1 <- ggplot2::ggplot(mtcars, aes(x = wt, y = mpg, size = disp)) +
#'     ggplot2::geom_point()
#'
#' # Set the theme for a ggplot2 object with the default options
#' out <- p1 %>% maars::set_mms_ggplot2_theme(ggplot_obj = .)
#' }
set_mms_ggplot2_theme <- function(ggplot_obj,
                              axis_text_size = 17,
                              axis_title_size = 17,
                              strip_text_x_size = 17,
                              legend_text_size = 17,
                              legend_title_size = 17,
                              legend_position = "bottom") {
    out <- ggplot_obj +
            ggplot2::theme_bw() +
            ggplot2::theme(
                axis.text = ggplot2::element_text(size = axis_text_size),
                axis.title = ggplot2::element_text(size = axis_title_size),
                strip.text.x = ggplot2::element_text(size = strip_text_x_size),
                legend.text = ggplot2::element_text(size = legend_text_size),
                legend.title = ggplot2::element_text(size = legend_title_size),
                legend.position = legend_position
            )

    return(out)
}


#' Check whether the input argument is a positive integer
#'
#' @param inp_arg Input function argument. Should be of type numeric, and a
#'   positive integer
#'
#' @return (logical) returns \code{TRUE} if input is a positive integer
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' B <- 100
#' assertthat::assert_that(check_fn_args_posint(inp_arg = B)) # Pass assertion
#' }
check_fn_args_posint <- function(inp_arg){
  return(is.numeric(inp_arg)
               && inp_arg == as.integer(inp_arg)
               && as.integer(inp_arg) > 0)
}

#' Check whether the arguments in the function are correctly specified
#'
#' \code{check_fn_args} is used to assess whether the arguments
#' are correctly specified and returns an error message if
#' they do not match the correct specification
#'
#' @param n Sample size.
#' @param B Bootstrap repetitions or number of bootstrap samples to be drawn.
#' @param m Number of observations to be sampled with replacement from the
#'   dataset for each bootstrap repetition.
#'
#' @return (logical) returns \code{TRUE} if the logical assertions are satisfied
#' @keywords internal
check_fn_args <- function(n = NULL, B = NULL, m = NULL) {
  if (!is.null(B)) {
    assertthat::assert_that(check_fn_args_posint(inp_arg = B),
                            msg = glue::glue("B must be a positive integer")
    )
  }

  if (!is.null(m)) {
    assertthat::assert_that(check_fn_args_posint(inp_arg = m),
                            msg = glue::glue("m must be a positive integer")
    )
  }

  if (!is.null(n)) {
    assertthat::assert_that(check_fn_args_posint(inp_arg = n),
                            msg = glue::glue("n must be a positive integer")
    )
  }
}
