# Comp var examples ----
devtools::load_all()
set.seed(1243434)

# generate modeling data ----
n <- 1e3
X_1 <- stats::rnorm(n, 0, 1)
X_2 <- stats::rnorm(n, 10, 20)
eps <- stats::rnorm(n, 0, 100)

# Let's generate data and fit a well-specified OLS data and model ----
y <- 2 + X_1 * 1 + X_2 * 5 + eps
lm_fit <- stats::lm(y ~ X_1 + X_2)
summary(lm_fit)

# Fit our first maars_lm object i.e. mms_fit1 ----
# Here are essentially retaining our OLS fitted estimates but adding on
# revised model-misspecified standard errors for: [emp, mul], we did not
# run [res] bootstrap errors. Sandwich and well specified OLS errors get
# run by default
set.seed(454354534)
mms_fit1 <- comp_var(
    mod_fit = lm_fit,
    boot_emp = list(B = 50, m = 200),
    boot_res = list(B = 70),
    boot_mul = list(B = 60)
)

# summary
print(mms_fit1)
summary(mms_fit1)

# Run experiments: Emoji and Titles ----
# Get emoji's, need to pass another parameter i.e. req_type = "emoji"
all_emoji <- mms_fit1$var %>%
    purrr::map(.x = ., .f = ~ fetch_mms_comp_var_attr(
        comp_var_ind = .x,
        req_type = "emoji"
    ))
all_emoji

all_emoji_titles <- mms_fit1$var %>%
    purrr::map(.x = ., .f = ~ fetch_mms_comp_var_attr(
        comp_var_ind = .x,
        req_type = "emoji_title"
    ))
all_emoji_titles

# Run experiments: Tidy summary ind ----
all_summary <- mms_fit1$var %>%
    purrr::map(.x = ., .f = ~ fetch_mms_comp_var_attr(
        comp_var_ind = .x,
        req_type = "var_summary"
    ))
all_summary

# Run experiments: Tidy group summary as a row bind ----
all_summary_tidy <- mms_fit1$var %>%
    purrr::map_df(.x = ., .f = ~ fetch_mms_comp_var_attr(
        comp_var_ind = .x,
        req_type = "var_summary_mod"
    )) %>%
    dplyr::select(-var_type_abb)
all_summary_tidy

# Run experiments: Tidy summary ind, with new variance types in colnames ----
all_summary_nmd <- mms_fit1$var %>%
    purrr::map(.x = ., .f = ~ fetch_mms_comp_var_attr(
        comp_var_ind = .x,
        req_type = "var_summary_nmd"
    ))
all_summary_nmd

all_summary_nmd_joined <- all_summary_nmd %>%
    purrr::reduce(.x = ., dplyr::left_join, by = c("term", "estimate"))
all_summary_nmd_joined

get_summary2(mod_fit = mms_fit1,
             sand = TRUE,
             boot_emp = TRUE,
             boot_mul = TRUE,
             boot_res = TRUE,
             well_specified = TRUE,
             tidy = TRUE)


all_summary_nmd_joined

# Run experiments: Get all of the assumptions ----
all_assumptions <- mms_fit1$var %>%
    purrr::map(.x = ., .f = ~ fetch_mms_comp_var_attr(
        comp_var_ind = .x,
        req_type = "var_assumptions"
    ))

# Run experiments: Get Get the tidy group summary and re-split it ----
# Not sure if we will need this function, since we already have everything
# in split format
# NOTE: The list gets unnamed during the split, so that the original names need
#       to be reset e.g. using purrr::set_names
mms_fit1$var %>%
    purrr::map_df(.x = ., .f = ~ fetch_mms_comp_var_attr(
        comp_var_ind = .x,
        req_type = "var_summary_mod"
    )) %>%
    dplyr::group_by(.data = ., var_type_abb) %>%
    dplyr::group_split(.tbl = ., .keep = FALSE)

mms_fit1$var$var_sand

# Run experiments: cli related ----
# The following are the key data sources needed to build CLI outputs
# all_emoji_titles
# all_summary
# all_assumptions

# Run experiments: Get assumptions code working ----
get_mms_assumptions_cli <- function(title, assumptions) {
    cli::cli_end()
    cli::cli_h1(cli::col_green(glue::glue("{title} Assmp.")))
    cli::cli_ul()
    cli::cli_li(assumptions)
    cli::cli_end()
}

# Get the printed assumptions
purrr::iwalk(
    all_emoji_titles,
    ~ get_mms_assumptions_cli(
        title = all_emoji_titles[[.y]],
        assumptions = all_assumptions[[.y]]
    )
)

# We need to get this working for a model of class lm
get_mms_print_cli <- function(mod_fit,
                              all_emoji_titles,
                              all_assumptions) {
    cli::cli_end()
    cli::cli_h1(cli::col_yellow(glue::glue("Fitted OLS Model:")))
    mod_fit_lm <- mod_fit
    class(mod_fit_lm) <- c("lm")
    # print(class(mod_fit_lm))
    # print(class(mod_fit))
    print(mod_fit_lm)
    cli::cli_text(glue::glue_collapse("\n\nClass:\n{class(mod_fit)}\n", sep = ", ", last = ""))
    purrr::iwalk(
        all_emoji_titles,
        ~ get_mms_assumptions_cli(
            title = all_emoji_titles[[.y]],
            assumptions = all_assumptions[[.y]]
        )
    )
    cli::cli_end()
}

# get_mms_print_cli(mod_fit = lm_fit)
get_mms_print_cli(mod_fit = mms_fit1,
                  all_emoji_titles = all_emoji_titles,
                  all_assumptions = all_assumptions)

# Run experiments: Get interleaved summary code working ----
# TODO: Pass in digits as in input to the function
get_mms_summary_cli <- function(title, summary, assumptions, digits = 3) {
    cli::cli_end()
    cli::cli_h1(cli::col_yellow(glue::glue("{title} Standard Errors")))
    cli::cli_text("\n")
    cli::cli_end()
    print.data.frame(x = summary, digits = digits)
    cli::cli_end()
    cli::cli_ul()
    cli::cli_h2(cli::col_green(glue::glue("{title} Assumptions")))
    cli::cli_ul()
    cli::cli_li(assumptions)
    cli::cli_end()
}

# Get the printed summary table interleaved with assumptions
purrr::iwalk(
    all_emoji_titles,
    ~ get_mms_summary_cli(
        title = all_emoji_titles[[.y]],
        summary = all_summary[[.y]],
        assumptions = all_assumptions[[.y]],
        digits = 3
    )
)

# Run experiments: Get joined summary code working ----
# TODO: Combine this with above function get_mms_summary_cli perhaps?
get_mms_summary_join_cli <- function(summary_joined, digits = 3) {
    cli::cli_end()
    cli::cli_h1(cli::col_yellow(glue::glue("Fitted OLS Model:")))
    print.data.frame(x = summary_joined, digits = digits)
    cli::cli_h1(cli::col_cyan(glue::glue("Assumptions")))
    purrr::iwalk(
        all_emoji_titles,
        ~ get_mms_assumptions_cli(
            title = all_emoji_titles[[.y]],
            assumptions = all_assumptions[[.y]]
        )
    )
    cli::cli_end()
}

# Get the printed joined summary table with assumptions
get_mms_summary_join_cli(summary_joined = all_summary_nmd_joined, digits = 3)


# get_summary()
# get_confint()
