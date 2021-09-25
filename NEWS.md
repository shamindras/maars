# maars 1.3.0

- Improve citation of `maars` in `README.md` and `inst/CITATION`.
- Thanks to [@bwiernik](https://github.com/bwiernik) for flagging and testing
  utf8 title printing issues.

# maars 1.2.0

- Write vignette on effect of increasing `B` in (`n`-out-of-`n`) empirical /
  multiplier / subsampling bootstrap samples on coverage when compared to
  sandwich.

# maars 1.1.0

- Change `Significance` heading in our summary output to be abbreviated to `Signif:` to be consistent with `lm()` output.
- Change column ordering for bootstrap standard errors and t-statistics rigorously.
- Change p-value format to be `2`/`3` digits
- Clean `R/scripts_and_filters/experiments/` dir, remove old experiments.
- Clean up script that gets metadata of `maars` function **metadata** from `pkgdown`.
- Remove all `base::` prefix use e.g. `base::return()`.
- Change `dplyr::summarise` to `dplyr::summarize` for spelling consistency.
- Ensure all `stats` functions use the `stats::` prefix.
- Fix `.data` related `rlang` issues.
- We shouldn't name a variable `df` since this conflicts with `stats::df`. We should change this after the demo.
- Clean vignette. Leave the plot in the vignette (to be moved elsewhere).
- Updated the corrected `Boston Housing` Dataset with citations. Add unit tests
  for the corrections.
- Clean up spelling notes.
- Change multiplier weights code to use `switch` based approach.
- Switch to `model.matrix` in sandwich variance and use residuals in the computation.
- Update `maars` to have a package level doc. Add `@importFrom` statements.
- Fix `NOTE` by adding `.gitkeep` in vignettes to `.RBuildignore`.
- Fix `NOTE` by DESCRIPTION meta-information by making it a couple of sentences. This is a placeholder and we should refine it before official `CRAN` release.
- Consolidate `boston-housing.R` and `la-county.R` files into a single `data-maars.R` files. Consolidate `test` files accordingly.
- Make some minor changes to the vignette.
- Add `styling` to our code using the `Makefile` and `styler::style_dir(here::here('R'))`
  and for `tests`.
- Have a `make style` which does both `R` and `tests`.
- Make sure styling does not include vignettes.
- Ensure that `url_check()` are all resolved for `CRAN`.
- Remove DOI entries from `inst/REFERENCES.bib` since they can cause `CRAN` url issues.
- Add search functionality to `pkgdown` our site.
- Use MIT License.
- Fix the no visible binding for global variable errors in our code.
- Remove mixture of `%>%` and base code, and just break pipes into variables.
- Change all `attr(obj, "class") <- c("obj_class_name")` to be of the form `class(obj) <- "obj_class_name"` for consistency.
- Change `dplyr::_all` scoped words using the superseded `across` function.

# maars 1.0.0

- Set the default `digits = 3` formatting in `summary.maars_lm` printed output.
- Move `summary.maars_lm`, `print.maars_lm`, `plot.maars_lm` methods into `maars-lm.R` file.
- Merge `lm-var.R` code into into `sandwich-var.R` since they are both default estimators.
- Use the `la_county` tibble directly from `maars` in our vignette.
- Re-write vignette to produce Table 1 from Buja et. al. 1 to include new `get_summary()` function.
- Write `get_plot()`, `get_summary()`, `summary.maars_lm()`, and `get_confint()`, `confint.maars_lm()` functionality for `maars_lm()` objects.
- Change the statistic based on the F-distribution to one based on the Chi-square.
- Write `plots` function with default options and wrap around `get_plot()`.
- Add more Cook's distance plots similar to `lm()` output.
- Add confidence interval plots to the `maars_lm` plot output.
- Print assumptions i.e. for all run `maars` standard errors. So `sand`, `well_specified` assumptions are printed by default, and other standard errors.
- Add assumption string for `comp_lm_var` using `glue::glue()` and improve formatting.
- Amend `get_confint()` to return tidy tibble output.
- Just show `term`, `conf.low`, `conf.high` for `confint(maars_lm)`. Fix it to be the same as `confint(lm)`.
- Add `la_county` `tibble` to our package.
- Change `README.Rmd` to use `devtools` rather than `remotes` as the preferred package installation.
- Add `devtools` to `Suggests` in `DESCRIPTION` so that vignettes use our latest code.
- Ensure that the `la_county` `tibble` is documented
  method.
- Write residual bootstrap assumptions to be the same as the well specified `lm()` model.
- Include `weights` type in assumptions for multiplier bootstrap.
- Include `n` in assumptions for empirical bootstrap.

# maars 0.7.0

- Write function to create `maars_lm` object from `comp_var`.
- write a `as.maars_lm` function to be run on an `lm` object.
- write `confint.maars_lm` method.
- write `print.maars_lm` method.
- Write `summary.maars_lm` method.
- Write `plot.maars_lm`, currently for `lm` objects.
- Correct typo in `maars` release `0.6.0` in `NEWS.md`.
- Use `GPL-2` and `GPL-3` `LICENCE`.
- Remove the `MIT LICENCE` files.
- We would like to thank [@capnrefsmmat](https://github.com/capnrefsmmat) for
  his kind assistance in various aspects of this release.

# maars 0.6.0

- update the documentation of `comp_var` and of the estimators of the variance.
- Adapt tests to handle the new function `comp_var`.
- create a function to nicely return a string containing the assumptions behind each computation of the variance (e.g., call it `get_assumptions`).
- compute covariance matrix `V` of coefficients estimates for the bootstrap.
- Make `weights_type` for `comp_boot_mul` to be default `rademacher`.
- Handle assertions for multiplier bootstrap.
- rewrite `comp_var` to handle the list outputs generated by the estimators of the variance.
- adapt tests to handle the list outputs generated by the variance estimators.
- change the bootstrap functions such that they return a list and compute `get_summary` within the function.
- make the names of the lists generated by the estimators of the variance consistent.
- rewrite the sandwich function to return a list.
- write documentation for `get_summary`.
- Check that we define/design inputs consistently for `comp_var`.
- Ensure that default values for all inputs is set to `NULL` for empirical bootstrap, multiplier bootstrap, residual bootstrap.
- Perform input assertion checking for all inputs consistently for `comp_var`.
- Create an `if-then-else` skeleton for `comp_var`.
- Add `residual` bootstrap to `comp_var`.
- Update tests for `comp_var` for empirical bootstrap, multiplier bootstrap, residual bootstrap.

# maars 0.5.0

- Rename function names to be more consistent e.g. remove `qr` from `empirical` bootstrap names.
- Ensure renamed function names are also correctly changed in the corresponding test files.
- Split our `pkgdown` references into suitable categories [like usethis](https://usethis.r-lib.org/reference/index.html), here is [an example](https://github.com/r-lib/usethis/blob/45d0fef414fd69ac7ed468d196e93db7f2008e27/_pkgdown.yml).
- Change specific functions to be private by removing `#' @export` in `roxygen2` and replacing it with `#' @keywords internal`.

# maars 0.4.0

- Include `.gitattributes` file.
- We need to `set.seed` in all our vignette chunks. This is for.
  reproducibility, but to also avoid merge conflicts every time we rebuild the
  package using `make build_package`
- **roxygen2:** Rewrite documentation of the functions making the style more.
  consistent across functions. For example, we could adopt the style used for
  the [quantile function](https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/quantile)
- **roxygen2:** insert dots at end of sentences in roxygen2..
  [See below](https://github.com/shamindras/maars/issues/24?fbclid=IwAR0JsSrXZGUpYyuizsTFIL8Q0dUvOy_WVbUWBuvZlygg4rmjuo0rU5xNGvM#issuecomment-751913442).
- **roxygen2:** use `@details` responsibly..
  [See below](https://github.com/shamindras/maars/issues/24?fbclid=IwAR0JsSrXZGUpYyuizsTFIL8Q0dUvOy_WVbUWBuvZlygg4rmjuo0rU5xNGvM#issuecomment-751913442).
- **roxygen2:** replace `var_name` with `\code{var_name}`..
  [See below](https://github.com/shamindras/maars/issues/24?fbclid=IwAR0JsSrXZGUpYyuizsTFIL8Q0dUvOy_WVbUWBuvZlygg4rmjuo0rU5xNGvM#issuecomment-751913442).
- **ggplot2:** Replace hardcoded values `1.96` with appropriate outputs.
  from statistics functions, in `vignettes`
- Check the order in which we have our functions written in each file.
- **ggplot2:** We should break up our `ggplot2` code into separate plots.
  and then combine them using `+`. This will make it much easier to manage the
  code and make it readable
- **ggplot2:** Need to be consistent with our `ggplot2` themes used in.
  our plots.
  [See ggplot2 theme wrapper below](https://github.com/shamindras/maars/issues/24?fbclid=IwAR0JsSrXZGUpYyuizsTFIL8Q0dUvOy_WVbUWBuvZlygg4rmjuo0rU5xNGvM#issuecomment-751917061).
- **ggplot2:** Need to be consistent with the `ggplot2` font and.
  other settings used in our plots. As a preference we should only
  use `labs` for example.
  [See ggplot2 theme wrapper below](https://github.com/shamindras/maars/issues/24?fbclid=IwAR0JsSrXZGUpYyuizsTFIL8Q0dUvOy_WVbUWBuvZlygg4rmjuo0rU5xNGvM#issuecomment-751917061).
- **ggplot2:** Add `names_prefix = "q"` to `pivot_wider` for.
  the `ggplot2` code i.e. to avoid **.data$`0.275`** issue. Note
  the `"q"` here stands for `quantile`.
- Create a `utils-common.R` similar to the.
  [selectiveInference package](https://github.com/selective-inference/R-software/blob/master/selectiveInference/R/funs.common.R#L87-L123), ensure that common functions are put in this file.
- clean up the `tests` directory i.e. delete unused `R` test files.
- clean up the `R` directory i.e. delete unused `R` files.
- We should add `#' @importFrom rlang .data` in all our functions under `#' @export`.
- Fix `R CMD CHECK` error caused in `vignette`, namely [this one](https://github.com/shamindras/maars/pull/21/checks?check_run_id=1555124560).
- Add `stringr` to `DESCRIPTION`.
- Change `multiplier` bootstrap weights to be applied inside `purrr` e.g using `purrr::map2` rather than generating the `e` matrix up front. Conduct benchmarks of this to test speed, so that current version still has it's place in benchmarking.
- For `vignette` should `knitr::kable` package instead of `DT` package and just need to ensure Table 1 is correctly populated.
- For `vignette` we just need to ensure Table 1 is correctly formatted i.e. use `LaTeX` for headings, consistent width, and captions. See [here](https://bookdown.org/yihui/rmarkdown-cookbook/kable.html#escape-special-characters) for the use of LaTeX in `knitr::kable` heading rows.
- Add `Rmd` line wrap hooks similar to [Yihui Xie's approach](https://github.com/yihui/knitr-examples/blob/master/077-wrap-output.Rmd#L14-L27) and the [oxforddown approach](https://github.com/ulyngs/oxforddown/blob/master/index.Rmd#L69-L72).
- Add doc for `weight_type` in `multiplier` bootstrap.
- Remove default value of `B` in `multiplier` variance.
- Allow for 5 types of weights for multiplier bootstrap i.e. `std_gaussian`, `rademacher`, and `mammen`, `webb`, `gamma`.
- Update credits in `Licence` and [License.md](https://github.com/shamindras/maars/blob/main/LICENSE.md).

# maars 0.3.0

- Implement an efficient `multiplier` bootstrap for `lm` standard errors.
- Implement the `empirical` bootstrap for `lm` standard errors.
- Implement the `empirical` bootstrap for `glm` standard errors.
- Change the input `lm_fit` to be `mod_object` in all of our functions.
- Check that we have referenced functions using `stats::`, `dplyr::`, etc.
- Add the `t-test` values in Table 1 of the `Models As Approximations - Part 1` paper.
- Add input validation for our bootstrap and variance estimation functions.
  e.g. `B` must be a positive integer
- Set a seed for the unit tests.
- Update `Readme.Rmd` to add installation instructions and link to official package.

# maars 0.2.0

- Create the `comp_sandwich_qr_var` function with documentation, tests, benchmarking.

# maars 0.1.0

# maars 0.0.0.9000

- Added a `NEWS.md` file to track changes to the package.
