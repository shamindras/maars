#' Assertion Checks for individual \code{\link{get_boot_summary}} function inputs
#'
#' \code{check_fn_args_comp_mms_var_boot_ind} is used to assess whether the arguments
#' are correctly specified in \code{list} format and returns an error message if
#' they do not match the correct specification
#'
#' @param mod_fit (maars_lm, lm) A fitted OLS \code{maars_lm, lm} class object
#' @param sand (logical) : \code{TRUE} if sandwich estimator output is required,
#'   \code{FALSE} to exclude this output from the request
#' @param boot_emp (logical) : \code{TRUE} if empirical bootstrap standard error
#'   output is required, \code{FALSE} to exclude this output from the request
#' @param boot_res (logical) : \code{TRUE} if residual bootstrap standard error
#'   output is required, \code{FALSE} to exclude this output from the request
#' @param boot_mul (logical) : \code{TRUE} if multiplier bootstrap standard error
#'   output is required, \code{FALSE} to exclude this output from the request
#' @param well_specified (logical) : \code{TRUE} if lm standard errors
#'   (well specified) output is required, \code{FALSE} to exclude this output
#'   from the request
#'
#' @return : A character vector of  \code{TRUE} if assertions pass, or an
#'   error if there is an assertion failure.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' set.seed(1243434)
#'
#' # generate data
#' n <- 1e3
#' X_1 <- stats::rnorm(n, 0, 1)
#' X_2 <- stats::rnorm(n, 10, 20)
#' eps <- stats::rnorm(n, 0, 1)
#'
#' # OLS data and model
#' y <- 2 + X_1 * 1 + X_2 * 5 + eps
#' lm_fit <- stats::lm(y ~ X_1 + X_2)
#'
#' # Create a maars_lm model with empirical bootstrap, sandwich, and
#' # well-specified variance estimates
#' comp_var1 <- comp_var(mod_fit = lm_fit, boot_emp = list(B = 20, m = 200),
#'                     boot_res = NULL,
#'                     boot_mul = NULL)
#'
#' # Let the user request only the empirical bootstrap variance estimates
#' # This function should return "boot_emp" since the user only requested it
#' # and it is available in the fitted maars_lm model
#' check_fn_args_summary(mod_fit = comp_var1, sand = FALSE,
#'                       boot_emp = TRUE, boot_res = FALSE, boot_mul = FALSE,
#'                       well_specified = FALSE)
#' }
check_fn_args_summary <- function(mod_fit,
                                  sand,
                                  boot_emp,
                                  boot_mul,
                                  boot_res,
                                  well_specified) {

    # Assertion checking for mod_fit is of class "maars_lm", "lm"
    assertthat::assert_that(all(c("maars_lm", "lm") == class(mod_fit)),
                            msg = glue::glue("mod_fit must only be of class: ['maars_lm', 'lm']")
    )

    # Input variance parameter names
    var_param_nms <- c("sand", "boot_emp", "boot_res", "boot_mul", "well_specified")
    # Get the combined variance related parameter values
    var_param_vals <- purrr::map(.x = var_param_nms, ~get(x = .x))
    # Rename the list values to correspond to the input names
    names(var_param_vals) <- var_param_nms
    # var_param_vals <- c(sand, boot_emp, boot_res, boot_mul, well_specified)

    # Check all input parameters, other than mod_fit are of class logical
    assertthat::assert_that(
        all(purrr::map_lgl(.x = var_param_vals, ~is.logical(.x))),
        msg = glue::glue(
            "All input variance parameters: [sand, boot_emp, boot_res, boot_mul, well_specified]",
            "must be of class logical",
            .sep = " "
        ))

    # Now that we have assertion checked all variance input parameters are of
    # class logical, let's convert them into a logical vector (not list) to
    # make future assertion checking easier
    var_param_vals_lgl <- var_param_vals %>%
        purrr::map_lgl(.x = ., ~ .x) %>%
        base::unname(obj = .)

    # If all parameters are FALSE, return a warning, and the table containing
    # only the sandwich variance
    if(!any(var_param_vals_lgl)){
        warning(glue::glue("You have passed in FALSE for all input variance parameters:",
                           "\n[sand, boot_emp, boot_res, boot_mul, well_specified].",
                           "\n\nReturning the default sandwich variance estimator...\n",
                           .sep = " "))
        comm_nms <- c("var_sand")
    } else{
        # Filter to only the selected variance params i.e. those with TRUE values
        var_param_nms_filt <- var_param_nms[var_param_vals_lgl]

        # Get the comp_mms_var output from the fitted maars_lm object
        out_comp_mms_var <- mod_fit$var
        # out_comp_mms_var_nms <- names(out_comp_mms_var)
        # Get the abbreviated names i.e. remove the "var_" prefix to align with summary
        # inputs
        # out_comp_mms_var_nms_abb <- names(out_comp_mms_var) %>%
        #     stringr::str_replace_all(string = ., pattern = "var_", "")

        # Filter out the non-NULL variance values
        out_comp_mms_var_filt <- out_comp_mms_var %>% purrr::compact(.x = .)
        # Get the abbreviated names i.e. with the "_var" prefix removed
        out_comp_mms_var_filt_nms_abb <- names(out_comp_mms_var_filt) %>%
            stringr::str_replace_all(string = ., pattern = "var_", "")

        # Get the common variance estimates from the the model and the user
        # requested variance estimates
        # Here intersect first uses the model variance estimate names to ensure
        # that the ordering of variance estimates is preserved from comp_mms_var.
        # This will ensure the correct default ordering in the consolidated
        # variance summary terms in other functions that use this assertion
        # check
        comm_nms_abb <- intersect(out_comp_mms_var_filt_nms_abb, var_param_nms_filt)

        # Check that the user requested cterms are
        assertthat::assert_that(
            length(comm_nms_abb) == length(var_param_nms_filt),
            msg = glue::glue("You have requested variance summary for:",
                             "\n[{glue::glue_collapse(var_param_nms_filt, sep = ', ')}]",
                             "\n\nHowever your model only has the following variance computations:",
                             "\n[{glue::glue_collapse(out_comp_mms_var_filt_nms_abb, sep = ', ')}]",
                             "\n\nPlease refit your maars_lm model again with your requested variance computations and re-run...",
                             .sep = " "
            )
        )

        # Add the "var_" prefix to the requested variance terms, so that we
        # match the names in the variance estimates from the model i.e. from the
        # comp_mms_var output
        comm_nms <- comm_nms_abb %>% stringr::str_c("var_", ., sep = "")
    }

    return(comm_nms)
}


#' An individual variance table summary function that adds the variance type
#' suffix to the table columns
#'
#' @param comp_mms_var_dat TODO Add
#'
#' @return TODO Add
#'
#' @keywords internal
get_summary_ind <- function(comp_mms_var_dat){
    # Riccardo for Shamindra: this is an internal function so you do not need assertion checking
    # TODO: Add assertion error that comp_mms_var_dat is a list and is non_null
    # TODO: Add assertion Check that the comp_mms_var_dat has the correct names

    # Get the abbreviated variance type name
    var_type_name_abb <- purrr::pluck(.x = comp_mms_var_dat, "var_type_abb")

    # Get the abbreviated variance summary table
    var_summ <- comp_mms_var_dat %>% purrr::pluck("var_summary")

    # Append suffix to variable names
    cols_common <- c("term", "estimate")
    cols_with_suffix <- setdiff(colnames(var_summ), cols_common) %>%
        stringr::str_c(., var_type_name_abb, sep = ".")
    cols_renamed <- c(cols_common, cols_with_suffix)

    # Apply new names
    var_summ <- var_summ %>%
        dplyr::rename_with(~ cols_renamed, dplyr::everything())

    return(var_summ)
}

#' Get the tidy variance summary from a fitted OLS \code{maars_lm, lm}
#' class object
#'
#' Get the tidy variance summary from a fitted OLS \code{maars_lm, lm}
#' class object
#'
#' @param mod_fit (maars_lm, lm) A fitted OLS \code{maars_lm, lm} class object
#' @param sand (logical) : \code{TRUE} if sandwich estimator output is required,
#'   \code{FALSE} to exclude this output from the request
#' @param boot_emp (logical) : \code{TRUE} if empirical bootstrap standard error
#'   output is required, \code{FALSE} to exclude this output from the request
#' @param boot_res (logical) : \code{TRUE} if residual bootstrap standard error
#'   output is required, \code{FALSE} to exclude this output from the request
#' @param boot_mul (logical) : \code{TRUE} if multiplier bootstrap standard error
#'   output is required, \code{FALSE} to exclude this output from the request
#' @param well_specified (logical) : \code{TRUE} if lm standard errors
#'   (well specified) output is required, \code{FALSE} to exclude this output
#'   from the request
#'
#' @return (tibble) : Combined standard error summary from a fitted
#' OLS \code{maars_lm, lm} class object
#'
#' @export
#'
#' @examples
#' \dontrun{
#' set.seed(1243434)
#'
#' # generate data
#' n <- 1e3
#' X_1 <- stats::rnorm(n, 0, 1)
#' X_2 <- stats::rnorm(n, 10, 20)
#' eps <- stats::rnorm(n, 0, 1)
#'
#' # OLS data and model
#' y <- 2 + X_1 * 1 + X_2 * 5 + eps
#' lm_fit <- stats::lm(y ~ X_1 + X_2)
#'
#' # DEFINE common column names - these stay the same across all
#' # reported error types
#' common_vars <- c("term", "estimate")
#'
#' # Empirical Bootstrap check
#' set.seed(454354534)
#' comp_var1 <- comp_var(mod_fit = lm_fit, boot_emp = list(B = 20, m = 200),
#'                     boot_res = list(B = 30),
#'                     boot_mul = NULL)
#'
#' # This returns everything but boot_mul, since we didn't run it in the original
#' # original maars_lm model
#' get_summary(mod_fit = comp_var1, sand = TRUE,
#'                      boot_emp = TRUE, boot_res = TRUE, boot_mul = FALSE,
#'                      well_specified = TRUE)
#' }
get_summary <- function(mod_fit,
                                 sand = TRUE,
                                 boot_emp = FALSE,
                                 boot_mul = FALSE,
                                 boot_res = FALSE,
                                 well_specified = FALSE) {
    # Get the variance types the user has requested. This performs assertion
    # Checking, so if there is no error it will return the required names,
    # else it will throw an error
    req_var_nms <- check_fn_args_summary(mod_fit = mod_fit, sand = sand,
                                         boot_emp = boot_emp, boot_res = boot_res,
                                         boot_mul = boot_mul,
                                         well_specified = well_specified)

    # Filter the comp_mms_var output from the fitted maars_lm object for the
    # requested variance types from the user
    # out_comp_mms_var_filt <- out_comp_mms_var %>% purrr::pluck(req_var_nms)
    out_comp_mms_var_filt <- req_var_nms %>%
        purrr::map(.x = ., ~purrr::pluck(mod_fit$var, .x))

    # Now for each individual variance type get the modified summary table
    # with the abbreviated variance type added as a suffix
    # e.g. for sandwich estimator, the 'p.value' column becomes 'p.value.sand'
    out_comp_mms_var_filt_mod <- out_comp_mms_var_filt %>%
        purrr::map(.x = ., .f = ~get_summary_ind(comp_mms_var_dat = .x))

    # Now return the left_join output of all tables
    out <- out_comp_mms_var_filt_mod %>%
        purrr::reduce(.x = ., dplyr::left_join, by = c("term", "estimate"))

    return(out)

}

#' Retrieve the assumptions for the variance estimators to be consistent for a
#' a fitted OLS \code{maars_lm, lm} class object
#'
#' Retrieve the assumptions for the variance estimators to be consistent for a
#' a fitted OLS \code{maars_lm, lm} class object.
#'
#' @param mod_fit (maars_lm, lm) A fitted OLS \code{maars_lm, lm} class object
#' @param sand (logical) : \code{TRUE} if sandwich estimator output is required,
#'   \code{FALSE} to exclude this output from the request
#' @param boot_emp (logical) : \code{TRUE} if empirical bootstrap standard error
#'   output is required, \code{FALSE} to exclude this output from the request
#' @param boot_res (logical) : \code{TRUE} if residual bootstrap standard error
#'   output is required, \code{FALSE} to exclude this output from the request
#' @param boot_mul (logical) : \code{TRUE} if multiplier bootstrap standard error
#'   output is required, \code{FALSE} to exclude this output from the request
#' @param well_specified (logical) : \code{TRUE} if lm standard errors
#'   (well specified) output is required, \code{FALSE} to exclude this output
#'   from the request
#'
#' @return (vector) : Vectors containing the assumptions under which each estimator
#'   of the variance is consistent.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' set.seed(1243434)
#'
#' # generate data
#' n <- 1e3
#' X_1 <- stats::rnorm(n, 0, 1)
#' X_2 <- stats::rnorm(n, 10, 20)
#' eps <- stats::rnorm(n, 0, 1)
#'
#' # OLS data and model
#' y <- 2 + X_1 * 1 + X_2 * 5 + eps
#' lm_fit <- stats::lm(y ~ X_1 + X_2)
#'
#' # DEFINE common column names - these stay the same across all
#' # reported error types
#' common_vars <- c("term", "estimate")
#'
#' # Empirical Bootstrap check
#' set.seed(454354534)
#' comp_var1 <- comp_var(mod_fit = lm_fit, boot_emp = list(B = 20, m = 200),
#'                     boot_res = list(B = 30),
#'                     boot_mul = NULL)
#'
#' # This returns everything but boot_mul, since we didn't run it in the original
#' # original maars_lm model
#' get_assumptions(mod_fit = comp_var1, sand = TRUE,
#'                      boot_emp = TRUE, boot_res = TRUE, boot_mul = FALSE,
#'                      well_specified = TRUE)
#' }
get_assumptions <- function(mod_fit,
                            sand = sand,
                            boot_emp = boot_emp,
                            boot_res = boot_res,
                            boot_mul = boot_mul,
                            well_specified = well_specified) {

    req_var_nms <- check_fn_args_summary(mod_fit = mod_fit, sand = sand,
                                         boot_emp = boot_emp, boot_res = boot_res,
                                         boot_mul = boot_mul,
                                         well_specified = well_specified)

    assumptions <- req_var_nms %>%
        purrr::map(.x = ., ~paste0(purrr::pluck(mod_fit$var, .x, 'var_type_abb'),
                                   ': ',
                                   purrr::pluck(mod_fit$var, .x, 'var_assumptions')))

    assumptions <- unlist(assumptions)

    return(assumptions)
}
