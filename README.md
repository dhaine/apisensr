
<!-- README.md is generated from README.Rmd. Please edit that file -->

# apisensr <img src="man/figures/logo.png" align="right" width=120 />

<!-- badges: start -->

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/apisensr)](https://cran.r-project.org/package=apisensr)
[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.org/dhaine/apisensr.svg?branch=master)](https://travis-ci.org/dhaine/apisensr)<!-- badges: end -->

**apisensr** provides an interactive web app for the R package
[**episensr**](https://cran.r-project.org/package=episensr). The R
package **episensr** allows to do basic sensitivity analysis of
epidemiological results as described in **Applying Quantitative Bias
Analysis to Epidemiological Data** by Timothy L. Lash, Matthew P. Fox,
and Aliza K. Fink (ISBN: 978-0-387-87960-4,
[bias.analysis](https://sites.google.com/site/biasanalysis/)).

## License

This package is free and open source software, licensed under MIT.

## Installation

You can install the master version from CRAN or the development version
from [GitHub](https://github.com/dhaine/apisensr) with:

``` r
#install.packages("remotes")
remotes::install_github("dhaine/apisensr", ref = "develop")
```

## How to run apisensr

``` r
library(apisensr)
run_app()
```

Please note that the ‘apisensr’ project is released with a [Contributor
Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project,
you agree to abide by its terms.
