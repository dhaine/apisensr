
<!-- README.md is generated from README.Rmd. Please edit that file -->

# apisensr <img src="man/figures/logo.png" align="right" width=120 />

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/dhaine/apisensr.svg?branch=master)](https://travis-ci.org/dhaine/apisensr)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/apisensr)](https://cran.r-project.org/package=apisensr)
[![DOI](https://zenodo.org/badge/243598636.svg)](https://zenodo.org/badge/latestdoi/243598636)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

<!-- badges: end -->

**apisensr** provides an interactive web app for the R package
[**episensr**](https://cran.r-project.org/package=episensr). The R
package **episensr** allows to do basic sensitivity analysis of
epidemiological results as described in **Applying Quantitative Bias
Analysis to Epidemiological Data** by Timothy L. Lash, Matthew P. Fox,
and Aliza K. Fink (ISBN: 978-0-387-87960-4,
[bias.analysis](https://sites.google.com/site/biasanalysis/)).

## License

This package is free and open source software, licensed under GPL-2.

## Citation

To cite **apisensr**, please use:

``` r
citation("apisensr")
#> 
#> To cite apisensr in publications use:
#> 
#>   Haine, Denis (2021). The apisensr Shiny app package: interface to
#>   episensr for sensitivity analysis of epidemiological results. R
#>   package version 0.2.0. https://github.com/dhaine/apisensr/. doi:
#>   10.5281/zenodo.4592620.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Misc{,
#>     title = {The apisensr Shiny app package: interface to episensr for sensitivity analysis of epidemiological results},
#>     author = {Denis Haine},
#>     year = {2021},
#>     note = {R package version 0.2.0},
#>     doi = {10.5281/zenodo.4592620},
#>     url = {https://github.com/dhaine/apisensr/},
#>   }
```

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

Please note that the apisensr project is released with a Contributor
Code of Conduct. By contributing to this project, you agree to abide by
its terms.
