# Uncomment and install cli, utf8 packages if needed
# install.packages(pkgs = c("cli", "utf8"))
# install.packages(pkgs = c("tidyverse"))
library(tidyverse) # load the `%>%` and purrr

# Test encoding in base R -------------------------------------------------
# Take a given string
x <- "fa\xE7ile"

# Convert it to `latin1` encoding
Encoding(x) <- "latin1"
# Print it
x
# Confirm the encoding is `latin1`
Encoding(x)

# Convert it to utf8 encoding
x <- enc2utf8(x)
# Print it
x
# Confirm the encoding is `utf8`
Encoding(x)

# Check if the system is currently using utf8 encoding --------------------
# Get environment variables including encoding information
env_info <- base::l10n_info()

# Print out whether the system is supporting utf8
if (env_info[["UTF-8"]]) {
  print(glue::glue("UTF-8 is supported, current code set is",
    "{env_info[['codeset']]}",
    .sep = " "
  ))
} else {
  print(glue::glue("UTF-8 is NOT supported, current code set is",
    "{env_info[['codeset']]}",
    .sep = " "
  ))
}

# Test this out in a reprex -----------------------------------------------
create_emoji_str <- function(var_type_abb, is_utf_ind) {

  # Get the variance type abbreviation as a glue string
  var_type_abb <- glue::glue("{var_type_abb}")

  # Get the title only for the specific variance type
  var_title <- dplyr::case_when(
    var_type_abb == "lm" ~ "Well Specified Model",
    var_type_abb == "sand" ~ "Sandwich",
    var_type_abb == "emp" ~ "Empirical Bootstrap",
    var_type_abb == "sub" ~ "Subsampling",
    var_type_abb == "res" ~ "Residual Bootstrap",
    var_type_abb == "mul" ~ "Multiplier Bootstrap"
  ) %>%
    glue::as_glue(x = .)

  # Get the title only for the specific variance type
  var_type <- dplyr::case_when(
    var_type_abb == "lm" ~ "well_specified",
    var_type_abb == "sand" ~ "sand",
    var_type_abb == "emp" ~ "boot_emp",
    var_type_abb == "sub" ~ "boot_sub",
    var_type_abb == "res" ~ "boot_res",
    var_type_abb == "mul" ~ "boot_mul"
  ) %>%
    glue::as_glue(x = .)

  # Get environment variables including encoding information
  env_info <- base::l10n_info()

  # Get check of utf8 environmental suppport
  is_utf8 <- env_info[["UTF-8"]]

  # Manually override the utf8 control - just for testing
  # TODO: remove this in the final version
  is_utf8 <- is_utf_ind

  # Return the utf8 compatible title for printing depending on the
  # local environment support for utf8
  if (is_utf8) {
    # Get the emoji only for the specific variance type
    var_emoji <- dplyr::case_when(
      var_type_abb == "lm" ~ "\U1F4C9\U1F4C8",
      var_type_abb == "sand" ~ "\U1F969\U1F35E", # \U1F96A
      var_type_abb == "emp" ~ "\U1F9EE\U1F45F",
      var_type_abb == "sub" ~ "\U1F9EE\U1F45F",
      var_type_abb == "res" ~ "\U2696\U1F45F",
      var_type_abb == "mul" ~ "\U274C\U1F45F" # \U2716
    ) %>%
      glue::as_glue(x = .)

    # Get the combined emoji: title for the specific variance type
    var_emoji_title <- glue::glue("{var_emoji}: {var_title}:")
  } else {
    # If no utf8 support then just get the title for the specific variance
    # type
    var_emoji_title <- glue::glue("{var_title}:")
  }

  return(var_emoji_title)
}

# Check results for utf8 and non-utf8 options -----------------------------
test_var_type_abb <- c("lm", "sand", "emp", "sub", "res", "mul")

# Manually ensure utf8 version
# This will attempt to print all titles with emojis. Will be an
# issue if there is no utf8 support
test_var_type_abb %>%
  purrr::iwalk(.x = ., .f = ~ print(create_emoji_str(var_type_abb = .x,
                                                     is_utf_ind = TRUE)))
# Manually ensure non-utf8 version
# This will not print any emojis in the title. Will not be an issue if there is
# no utf8 support
test_var_type_abb %>%
    purrr::iwalk(.x = ., .f = ~ print(create_emoji_str(var_type_abb = .x,
                                                       is_utf_ind = FALSE)))


# Final proposed version --------------------------------------------------
create_emoji_str_fix <- function(var_type_abb) {

  # Get environment variables including encoding information
  env_info <- base::l10n_info()

  # Get check of utf8 environmental suppport
  is_utf8 <- env_info[["UTF-8"]]

  # Get the variance type abbreviation as a glue string
  var_type_abb <- glue::glue("{var_type_abb}")

  # Get the title only for the specific variance type
  var_title <- dplyr::case_when(
    var_type_abb == "lm" ~ "Well Specified Model",
    var_type_abb == "sand" ~ "Sandwich",
    var_type_abb == "emp" ~ "Empirical Bootstrap",
    var_type_abb == "sub" ~ "Subsampling",
    var_type_abb == "res" ~ "Residual Bootstrap",
    var_type_abb == "mul" ~ "Multiplier Bootstrap"
  ) %>%
    glue::as_glue(x = .)

  # Get the title only for the specific variance type
  var_type <- dplyr::case_when(
    var_type_abb == "lm" ~ "well_specified",
    var_type_abb == "sand" ~ "sand",
    var_type_abb == "emp" ~ "boot_emp",
    var_type_abb == "sub" ~ "boot_sub",
    var_type_abb == "res" ~ "boot_res",
    var_type_abb == "mul" ~ "boot_mul"
  ) %>%
    glue::as_glue(x = .)

  # Return the utf8 compatible title for printing depending on the
  # local environment support for utf8
  if (is_utf8) {
    # Get the emoji only for the specific variance type
    var_emoji <- dplyr::case_when(
      var_type_abb == "lm" ~ "\U1F4C9\U1F4C8",
      var_type_abb == "sand" ~ "\U1F969\U1F35E", # \U1F96A
      var_type_abb == "emp" ~ "\U1F9EE\U1F45F",
      var_type_abb == "sub" ~ "\U1F9EE\U1F45F",
      var_type_abb == "res" ~ "\U2696\U1F45F",
      var_type_abb == "mul" ~ "\U274C\U1F45F" # \U2716
    ) %>%
      glue::as_glue(x = .)

    # Get the combined emoji: title for the specific variance type
    var_emoji_title <- glue::glue("{var_emoji}: {var_title}:")
  } else {
    # If no utf8 support then just get the title for the specific variance
    # type
    var_emoji_title <- glue::glue("{var_title}:")
  }
  return(var_emoji_title)
}

# Check results for utf8 and non-utf8 options -----------------------------
test_var_type_abb <- c("lm", "sand", "emp", "sub", "res", "mul")

# Manually ensure utf8 version
# This will attempt to print all titles with emojis. Will be an
# issue if there is no utf8 support
test_var_type_abb %>%
  purrr::iwalk(.x = ., .f = ~ print(create_emoji_str_fix(var_type_abb = .x)))

