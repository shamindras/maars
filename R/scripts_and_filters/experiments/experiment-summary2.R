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

# FUNCTIONS: emoji and title helper functions ----
get_emoji <- function(var_type_abb) {
  out <- dplyr::case_when(
    var_type_abb == "lm" ~ "\U1F4C9\U1F4C8",
    var_type_abb == "sand" ~ "\U1F969\U1F35E", #\U1F96A
    var_type_abb == "emp" ~ "\U1F9EE\U1F45F",
    var_type_abb == "res" ~ "\U2696\U1F45F",
    var_type_abb == "mul" ~ "\U274C\U1F45F" #\U2716
  )
  return(out)
}

get_title <- function(var_type_abb) {
    out <- dplyr::case_when(
      var_type_abb == "lm" ~ "Well Specified Model",
      var_type_abb == "sand" ~ "Sandwich Estimator",
      var_type_abb == "emp" ~ "Empirical Bootstrap",
      var_type_abb == "res" ~ "Residual Bootstrap",
      var_type_abb == "mul" ~ "Multiplier Bootstrap"
    )
    return(out)
}

get_mms_title <- function(var_type_abb, title_type) {
  # Get the title only for the specific variance type
  var_title <- dplyr::case_when(
    var_type_abb == "lm" ~ "Well Specified Model",
    var_type_abb == "sand" ~ "Sandwich Estimator",
    var_type_abb == "emp" ~ "Empirical Bootstrap",
    var_type_abb == "res" ~ "Residual Bootstrap",
    var_type_abb == "mul" ~ "Multiplier Bootstrap"
  )

  # Get the emoji only for the specific variance type
  var_emoji <- dplyr::case_when(
    var_type_abb == "lm" ~ "\U1F4C9\U1F4C8",
    var_type_abb == "sand" ~ "\U1F969\U1F35E", #\U1F96A
    var_type_abb == "emp" ~ "\U1F9EE\U1F45F",
    var_type_abb == "res" ~ "\U2696\U1F45F",
    var_type_abb == "mul" ~ "\U274C\U1F45F" #\U2716
  )

  # Get the combined emoji: title for the specific variance type
  var_emoji_title <- glue::glue("{var_emoji}: {var_title}:")

  # Get the specific string type based on user specification
  out <- dplyr::case_when(
    title_type == "title" ~ var_title,
    title_type == "emoji" ~ var_emoji,
    title_type == "emoji_title" ~ var_emoji_title
  )

  return(out)
}


# FUNCTIONS: Get the individual variance summary ----
get_full_summary <- function(comp_var_ind, req_type){
    # Raw data variables
    cv_type <- purrr::pluck(.x = comp_var_ind, "var_type")
    cv_type_abb <- purrr::pluck(.x = comp_var_ind, "var_type_abb")
    cv_summary <- purrr::pluck(.x = comp_var_ind, "var_summary")
    cv_assumptions <- purrr::pluck(.x = comp_var_ind, "var_assumptions") # Should be "var_assumptions"

    # Derived variables
    # cv_emoji <- get_emoji(var_type_abb = cv_type_abb)
    # cv_title <- get_title(var_type_abb = cv_type_abb)
    cv_emoji_title <- get_mms_title(var_type_abb = cv_type_abb,
                                    title_type = "emoji_title")
    # cv_emoji_title <- glue::glue("{cv_emoji}: {cv_title}:")
    cv_summary_mod <- cv_summary %>%
        dplyr::mutate(var_type_abb = cv_type_abb)

    # Append suffix to variable names
    cols_common <- c("term", "estimate")
    cols_with_suffix <- setdiff(colnames(cv_summary), cols_common) %>%
      stringr::str_c(., cv_type_abb, sep = ".")
    cols_renamed <- c(cols_common, cols_with_suffix)

    # Apply new names
    cv_summary_nmd <- cv_summary %>%
      dplyr::rename_with(~ cols_renamed, dplyr::everything())

    # Get the specific type of individual variance variable
    out <- switch(req_type,
                  "emoji" = cv_emoji,
                  "emoji_title" = cv_emoji_title,
                  "var_type" = cv_type,
                  "var_type_abb" = cv_type_abb,
                  "var_summary" = cv_summary,
                  "var_summary_mod" = cv_summary_mod,
                  "var_summary_nmd" = cv_summary_nmd,
                  "var_assumptions" = cv_assumptions)
    return(out)
}
# comp_var1$var[[1]]
# get_full_summary(comp_var_ind = comp_var1$var[[1]])

# Run experiments: Emoji and Titles ----
# Get emoji's, need to pass another parameter i.e. req_type = "emoji"
all_emoji <- comp_var1$var %>%
  purrr::map(.x = ., .f = ~get_full_summary(comp_var_ind = .x,
                                            req_type = "emoji"))
all_emoji
# Test change of case with emoji's
etitle <- stringr::str_c(all_emoji[[1]], "Cool Title", sep = " ")
stringr::str_to_title(string = all_emoji[[1]], locale = "en") # Title Case, unchanged emoji and text
stringr::str_to_sentence(string = all_emoji[[1]], locale = "en") # Title Case, unchanged emoji, changed text

all_emoji_title <- comp_var1$var %>%
    purrr::map(.x = ., .f = ~get_full_summary(comp_var_ind = .x,
                                              req_type = "emoji_title"))
all_emoji_title

# Run experiments: Tidy summary ind ----
all_summary <- comp_var1$var %>%
  purrr::map(.x = ., .f = ~get_full_summary(comp_var_ind = .x,
                                            req_type = "var_summary"))
all_summary

# Run experiments: Tidy group summary as a row bind ----
all_summary_mod <- comp_var1$var %>%
    purrr::map_df(.x = ., .f = ~get_full_summary(comp_var_ind = .x,
                                                 req_type = "var_summary_mod")) %>%
    dplyr::select(-var_type_abb)
all_summary_mod

# Run experiments: Tidy summary ind, with new variance types in colnames ----
all_summary_nmd <- comp_var1$var %>%
  purrr::map(.x = ., .f = ~get_full_summary(comp_var_ind = .x,
                                            req_type = "var_summary_nmd"))
all_summary_nmd

all_summary_nmd %>%
  purrr::reduce(.x = ., dplyr::left_join, by = c("term", "estimate"))

# Run experiments: Get all of the assumptions ----
all_assumptions <- comp_var1$var %>%
  purrr::map(.x = ., .f = ~get_full_summary(comp_var_ind = .x,
                                            req_type = "var_assumptions"))

# Run experiments: Get Get the tidy group summary and re-split it ----
# Not sure if we will need this function, since we already have everything
# in split format
# NOTE: The list gets unnamed during the split, so that the original names need
#       to be reset e.g. using purrr::set_names
comp_var1$var %>%
    purrr::map_df(.x = ., .f = ~get_full_summary(comp_var_ind = .x,
                                                 req_type = "var_summary_mod")) %>%
    dplyr::group_by(.data = ., var_type_abb) %>%
    dplyr::group_split(.tbl = ., .keep = FALSE)

comp_var1$var$var_sand

# Run experiments: Get cli template working ----
test_title <- c("\U274C\U1F45F: Multiplier Bootstrap Assumptions")
test_assumptions <- c("Observations are i.n.i.d", "B = 500", "m = 20")
glue::glue_collapse(x = c(test_title, test_assumptions), sep = "\n* ")

out_cli <- function(title, assumptions) {
    cli::cli_end()
    cli::cli_ul()
    cli::cli_h1(cli::col_yellow(test_title))
    cli::cli_ul()
    cli::cli_li(test_assumptions)
    cli::cli_end()
}
out_cli(title = test_title,
        assumptions = test_assumptions)

# Run experiments: Get interleaved summary code working ----
get_mms_summary_cli <- function(title, summary, assumptions) {
  cli::cli_end()
  cli::cli_end()
  # cli::cli_h1(cli::col_yellow(title))
  cli::cli_h1(cli::col_yellow(glue::glue("{title} Standard Errors")))
  cli::cli_text("\n")
  cli::cli_end()
  # cli::cli_text("\n")
  print.data.frame(summary)
  cli::cli_end()
  cli::cli_ul()
  # cli::cli_text("\n")
  # cli::cli_h2(cli::col_green("Assumptions"))
  cli::cli_h2(cli::col_green(glue::glue("{title} Assumptions")))
  cli::cli_ul()
  cli::cli_li(assumptions)
  cli::cli_end()
}

purrr::iwalk(all_emoji_title,
             ~get_mms_summary_cli(title = all_emoji_title[[.y]],
                                  summary = all_summary[[.y]],
                                  assumptions = all_assumptions[[.y]]))

# all_emoji_title
# all_summary
# all_assumptions
