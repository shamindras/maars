library(fs)
library(tidyverse)
library(here)
library(yaml)

#' Read yaml of keywords to rename
#'
#' Read yaml file of keywords to rename. The yaml keyword is the original
#' keyword name, and the associated value is the replacement keyword name.
#'
#' @param fpath_yml : A character vector containing the file path to the
#'   keywords renaming yaml file.
#'
#' @return : A tibble containing the original keyword names, and the replacement
#'   keyword names.
read_keyword_rnm_tbl <- function(fpath_yml){
    read_yml <- yaml::read_yaml(file = fpath_yml)
    orig_names <- names(x = read_yml)
    replacement_names <- read_yml %>%
                            unname(obj = .) %>%
                            unlist(x = .)
    keyword_rnm_tbl <- tibble::tibble(pattern = orig_names,
                                      replacement = replacement_names)
    return(keyword_rnm_tbl)
}

#' Rename specified keyword in a specific file
#'
#' @param fpath : A character file path in which keywords are to be replaced.
#' @param pattern : The keyword pattern to replace.
#' @param replacement : The replacement keyword pattern.
#'
#' @return The original file with keywords replaced as specified. No object
#'   is returned.
file_keyword_rnm <- function(fpath, pattern, replacement){

    x <- readr::read_lines(file = fpath)
    y <- stringr::str_replace_all(string = x,
                                  pattern = pattern,
                                  replacement = replacement)
    cat(y, file = fpath, sep="\n", append = FALSE)
}

#' Replace all keywords in a directory for specific files
#'
#' @param path : A character directory path in which files are stored.
#' @param type : A character value of the type of objects to replace in the
#'   directory. The default value is "type".
#' @param glob : A character glob value of filenames to replace.
#' @param keyword_rnm_tbl : The table output from \code{\link{file_keyword_rnm}}
#'   of keywords to rename.
#'
#' @return
dir_files_keyword_rnm <- function(path,
                                  type = "file", glob,
                                  keyword_rnm_tbl){
    fs::dir_ls(path = path,
               type = type,
               glob = glob,
               recurse = FALSE) %>%
        tibble::enframe(x = ., name = NULL, value = "fpath") %>%
        tidyr::crossing(., keyword_rnm_tbl) %>%
        purrr::pwalk(.l = ., .f = file_keyword_rnm)
}

# Read the keyword renaming table
keyword_rnm_tbl <- read_keyword_rnm_tbl(fpath_yml =
                            here::here("R", "scripts_and_filters",
                                       "replace-keywords.yaml"))

# Rename all keywords in all our R files in the main/test directories
# DIRS_TO_REPLACE <- c(here::here("R"))
# DIRS_TO_REPLACE <- c(here::here("R"), here::here("tests", "testthat"))
DIRS_TO_REPLACE <- c(here::here("R"),
                     here::here("tests", "testthat"),
                     here::here("R", "scripts_and_filters", "experiments"))

DIRS_TO_REPLACE %>%
  purrr::walk(
    .x = .,
    .f = ~dir_files_keyword_rnm(
      path = .x,
      type = "file",
      glob = "*.R",
      keyword_rnm_tbl = keyword_rnm_tbl
    )
  )
