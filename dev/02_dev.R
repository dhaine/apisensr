# Building a Prod-Ready, Robust Shiny Application.
# 
# Each step is optional. 
# 

# 2. All along your project

## 2.1 Add modules
## 
golem::add_module( name = "parms" ) # Name of the module
golem::add_module( name = "my_other_module" ) # Name of the module

## 2.2 Add dependencies

#usethis::use_package( "thinkr" ) # To call each time you need a new package
usethis::use_package( "episensr", min_version = "0.9.5" ) # To call each time you need a new package
usethis::use_package("shinymaterial")
usethis::use_package("shinyjs")
usethis::use_package("shinyalert")

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
