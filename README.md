
<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/shamindras/maars/branch/master/graph/badge.svg)](https://codecov.io/gh/shamindras/maars?branch=master)
[![R build
status](https://github.com/shamindras/maars/workflows/R-CMD-check/badge.svg)](https://github.com/shamindras/maars/actions)
[![MIT
License](https://img.shields.io/apm/l/atomic-design-ui.svg?)](https://github.com/tterb/atomic-design-ui/blob/master/LICENSEs)
<!-- badges: end -->

# `maars` - an `R` implementation of Models As Approximations

The goal of the `maars` package is to implement the *Models As
Approximations* series of statistics papers (Buja, Brown, Berk, et al.
2019) and (Buja, Brown, Kuchibhotla, et al. 2019). This package was
inspired by the fantastic series of lectures by [Prof. Arun Kumar
Kuchibhotla](https://arun-kuchibhotla.github.io/) and [Prof. Andreas
Buja](http://www-stat.wharton.upenn.edu/~buja/), as part of the **“STAT
36761: Modern Linear Regression”** course at Carnegie Mellon University
(CMU) in Fall 2020.

## Installation and User Guide

To get a bug fix or to use a feature from the development version, you
can install the development version of `maars` from `GitHub`, as
follows:

``` r
# install.packages("devtools")
devtools::install_github("shamindras/maars")
```

More detailed instructions and user guides can be found at the [official
package website](https://shamindras.github.io/maars/). The source code
for the `maars` package can be [found on
github](https://github.com/shamindras/maars).

## Code of Conduct

Please note that the `maars` project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

## Related Packages

While `maars` has it’s own approach and API for performing valid
inference under model misspecification for OLS, it may not meet your
particular needs. Here is a listing of other leading `R` packages in
this field which you may want to try, with links to their project pages
(listed alphabetically):

-   [`{car}`](https://cran.r-project.org/web/packages/car/index.html)
-   [`{clubSandwich}`](https://github.com/jepusto/clubSandwich)
-   [`{estimatr}`](https://declaredesign.org/r/estimatr/articles/estimatr-in-the-tidyverse.html)
-   [`{lmtest}`](https://cran.r-project.org/web/packages/lmtest/index.html)
-   [`{sandwich}`](http://sandwich.r-forge.r-project.org/)

## Credits

This package is developed and maintained by:

-   [Riccardo Fogliato](http://www.stat.cmu.edu/~rfogliat/)
-   [Shamindra Shrotriya](https://www.shamindras.com/)
-   [Arun Kumar Kuchibhotla](https://arun-kuchibhotla.github.io/)

We want this to be a community project, so please feel free to contact
us, or file an issue if you would like to contribute to it.

## References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-buja2019modelsasapproximationspart1" class="csl-entry">

Buja, Andreas, Lawrence Brown, Richard Berk, Edward George, Emil Pitkin,
Mikhail Traskin, Kai Zhang, and Linda Zhao. 2019. “Models as
Approximations I: Consequences Illustrated with Linear Regression.”
*Statist. Sci.* 34 (4): 523–44.

</div>

<div id="ref-buja2019modelsasapproximationspart2" class="csl-entry">

Buja, Andreas, Lawrence Brown, Arun Kumar Kuchibhotla, Richard Berk,
Edward George, and Linda Zhao. 2019. “Models as Approximations II: A
Model-Free Theory of Parametric Regression.” *Statist. Sci.* 34 (4):
545–65.

</div>

</div>
