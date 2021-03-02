# Building a Prod-Ready, Robust Shiny Application.
#
# Each step is optional.
#

# 2. All along your project

## 2.0 Add functions and utils
##
golem::add_fct("draw_dag")
golem::add_utils("helpers")

## 2.1 Add modules
##
golem::add_module( name = "parms" ) # Name of the module

## 2.2 Add dependencies

#usethis::use_package( "thinkr" ) # To call each time you need a new package
usethis::use_package( "episensr", min_version = "1.0.0" ) # To call each time you need a new package
usethis::use_package("shinymaterial")
usethis::use_package("shinyjs")
usethis::use_package("shinyalert")
usethis::use_package("rhandsontable")
usethis::use_package("shinyWidgets")
usethis::use_package("rmarkdown")
usethis::use_package("ggplot2")
usethis::use_package("ggraph")
usethis::use_package("igraph")

usethis::use_dev_package("episensr")

## 2.3 Add tests

usethis::use_test( "app" )

## 2.4 Add a browser button

golem::browser_button()

## 2.5 Add external files

golem::add_js_file( "script" )
golem::add_js_handler( "handlers" )
golem::add_css_file( "custom" )

# 3. Documentation

## 3.1 Vignette
usethis::use_vignette("apisensr")
devtools::build_vignettes()

## 3.2 Code coverage
## You'll need GitHub there
usethis::use_github()
usethis::use_travis()
usethis::use_appveyor()

# You're now set!
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")
