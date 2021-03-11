# Comp var examples ----
devtools::load_all()
set.seed(1243434)

# generate data ----
n <- 1e3
X_1 <- stats::rnorm(n, 0, 1)
X_2 <- stats::rnorm(n, 10, 20)
eps <- stats::rnorm(n, 0, 100)

# Let's generate data and fit a well-specified OLS data and model ----
y <- 2 + X_1 * 1 + X_2 * 5 + eps
lm_fit <- stats::lm(y ~ X_1 + X_2)
summary(lm_fit)

# Fit our first maars_lm object i.e. comp_var1 ----
# Here are essentially retaining our OLS fitted estimates but adding on
# revised model-misspecified standard errors for: [emp, mul], we did not
# run [res] bootstrap errors. Sandwich and well specified OLS errors get
# run by default
set.seed(454354534)
comp_var1 <- comp_var(mod_fit = lm_fit,
                      boot_emp = list(B = 50, m = 200),
                      boot_res = list(B = 70),
                      boot_mul = list(B = 60))

# summary
print(comp_var1)
summary(comp_var1)


get_emoji <- function(x) {
  out <- dplyr::case_when(
    x == "lm" ~ "\U1F4C9\U1F4C8",
    x == "sand" ~ "\U1F969\U1F35E", #\U1F96A
    x == "emp" ~ "\U1F9EE\U1F45F",
    x == "res" ~ "\U2696\U1F45F",
    x == "mul" ~ "\U274C\U1F45F" #\U2716
  )
  return(out)
}

get_title <- function(x) {
    out <- dplyr::case_when(
        x == "lm" ~ "Well Specified Model Standard Errors",
        x == "sand" ~ "Sandwich Estimator Standard Errors",
        x == "emp" ~ "Empirical Bootstrap Standard Errors",
        x == "res" ~ "Residual Bootstrap Standard Errors",
        x == "mul" ~ "Multiplier Bootstrap Standard Errors"
    )
    return(out)
}

# TODO: "assumptions" in comp_var$var should be "var_assumptions"

# Get the individual variance summary
get_full_summary <- function(comp_var_ind, req_type){
    # Raw data variables
    cv_type <- purrr::pluck(.x = comp_var_ind, "var_type")
    cv_type_abb <- purrr::pluck(.x = comp_var_ind, "var_type_abb")
    cv_summary <- purrr::pluck(.x = comp_var_ind, "var_summary")
    cv_assumptions <- purrr::pluck(.x = comp_var_ind, "var_assumptions") # Should be "var_assumptions"

    # Derived variables
    cv_emoji <- get_emoji(x = cv_type_abb)
    cv_title <- get_title(x = cv_type_abb)
    cv_emoji_title <- glue::glue("{cv_emoji}: {cv_title}:")
    cv_summary_mod <- cv_summary %>%
        dplyr::mutate(var_type_abb = cv_type_abb)

    # Get the specific type of individual variance variable
    out <- switch(req_type,
                  "emoji" = cv_emoji,
                  "emoji_title" = cv_emoji_title,
                  "var_type" = cv_type,
                  "var_type_abb" = cv_type_abb,
                  "var_summary" = cv_summary,
                  "var_summary_mod" = cv_summary_mod,
                  "var_assumptions" = cv_assumptions)
    return(out)
}
# comp_var1$var[[1]]
# get_full_summary(comp_var_ind = comp_var1$var[[1]])

# Get emoji's, need to pass another parameter in get_full_summary(comp_var_ind, type = "emoji")
comp_var1$var %>%
    purrr::map_chr(.x = ., .f = ~get_full_summary(comp_var_ind = .x,
                                                  req_type = "emoji")) %>%
    base::unname(obj = .)

comp_var1$var %>%
    purrr::map_chr(.x = ., .f = ~get_full_summary(comp_var_ind = .x,
                                                  req_type = "emoji_title")) %>%
    base::unname(obj = .)

# Get the tidy group summary
comp_var1$var %>%
    purrr::map_df(.x = ., .f = ~get_full_summary(comp_var_ind = .x,
                                                 req_type = "var_summary_mod")) %>%
    dplyr::select(-var_type_abb)

# Get all of the assumptions
# TODO: Check whether to unname the vector
# TODO: Check whether to just return a list?
comp_var1$var %>%
    purrr::map_chr(.x = ., .f = ~get_full_summary(comp_var_ind = .x,
                                                  req_type = "var_assumptions"))


# Get the tidy group summary and re-split it. This does drop the var_type_abb
comp_var1$var %>%
    purrr::map_df(.x = ., .f = ~get_full_summary(comp_var_ind = .x)) %>%
    dplyr::group_by(.data = ., var_type_abb) %>%
    dplyr::group_split(.tbl = ., .keep = FALSE)


comp_var1$var$var_sand

test_title <- c("\U274C\U1F45F: Multiplier Bootstrap Assumptions")
test_assumptions <- c("Observations are i.n.i.d", "B = 500", "m = 20")
glue::glue_collapse(x = c(test_title, test_assumptions), sep = "\n* ")

out_cli <- function() {
    cli::cli_ul()
    cli::cli_h1(cli::col_yellow(test_title))
    cli::cli_ul()
    cli::cli_li(test_assumptions)
}
out_cli()
