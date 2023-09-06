
<!-- README.md is generated from README.Rmd. Please edit that file -->

# apisensr <img src="man/figures/logo.png" align="right" width=120 />

<!-- badges: start -->

[![R-CMD-check](https://github.com/dhaine/apisensr/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/dhaine/apisensr/actions/workflows/check-standard.yaml)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/apisensr)](https://cran.r-project.org/package=apisensr)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4592620.svg)](https://doi.org/10.5281/zenodo.4592620)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Total CRAN
downloads](https://cranlogs.r-pkg.org/badges/grand-total/apisensr)](https://cran.r-project.org/package=apisensr)

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
#> To cite package 'apisensr' in publications use:
#> 
#>   Haine, Denis (2023). The apisensr Shiny app package: interface to
#>   episensr for sensitivity analysis of epidemiological results. R
#>   package version 1.0.0. https://github.com/dhaine/apisensr/. doi:
#>   10.5281/zenodo.4592620.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Misc{,
#>     title = {The apisensr Shiny app package: interface to episensr for sensitivity analysis of epidemiological results},
#>     author = {Denis Haine},
#>     year = {2023},
#>     note = {R package version 1.0.0},
#>     doi = {10.5281/zenodo.4592620},
#>     url = {https://github.com/dhaine/apisensr/},
#>   }
```

## Installation

You can get the latest release from **CRAN**:

``` r
install.packages('apisensr')
```

Or install the development version from **GitHub** with **remotes**
package:

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
