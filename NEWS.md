# maar 0.3.0

* Implement an efficient `multiplier` bootstrap for `lm` standard errors
* Implement the `empirical` bootstrap for `lm` standard errors
* Implement the `empirical` bootstrap for `glm` standard errors
* Change the input `lm_fit` to be `mod_object` in all of our functions
* Check that we have referenced functions using `stats::`, `dplyr::`, etc
* Add the `t-test` values in Table 1 of the MAA paper
* Add input validation for our bootstrap and variance estimation functions 
e.g. `B` must be a positive integer
* Set a seed for the unit tests
* Update `Readme.Rmd` to add installation instructions and link to official package

# maar 0.2.0

* Create the `comp_sandwich_qr_var` function with documentation, tests, benchmarking

# maar 0.1.0

# maar 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
