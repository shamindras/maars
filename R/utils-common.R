#' Set the custom \code{\link[ggplot2]{theme}} for our plots
#'
#' \code{set_ggplot2_theme} is a wrapper to set a standard custom
#' \code{\link[ggplot2]{theme}} for all of our output plots. It contains
#' only the necessary parameters we require to change in the
#' \code{\link[ggplot2]{theme}} function, and takes the remaining arguments
#' as the default values.
#'
#' @param ggplot_obj A \code{\link[ggplot2]{ggplot2}} object
#' @param axis_text_size A positive integer to define the the size of the x and
#'   y axis text in our \code{\link[ggplot2]{ggplot2}} object
#' @param axis_title_size A positive integer to define the the size of the x and
#'   y axis text in our \code{\link[ggplot2]{ggplot2}} object
#' @param strip_text_x_size A positive integer to define the the size of the x and
#'   y axis text in our \code{\link[ggplot2]{ggplot2}} object
#' @param legend_text_size A positive integer to define the the size of the x and
#'   y axis text in our \code{\link[ggplot2]{ggplot2}} object
#' @param legend_title_size A positive integer to define the the size of the x and
#'   y axis text in our \code{\link[ggplot2]{ggplot2}} object
#' @param legend_position A character value the position of legends
#' ("none", "left", "right", "bottom", "top", or two-element numeric vector)
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' p1 <- ggplot2::ggplot(mtcars, aes(x = wt, y = mpg, size = disp)) +
#'     ggplot2::geom_point()
#'
#' # Set the theme for a ggplot2 object with the default options
#' out <- p1 %>% maars::set_ggplot2_theme(ggplot_obj = .)
#' }
set_ggplot2_theme <- function(ggplot_obj,
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
