# Building a Prod-Ready, Robust Shiny Application.
# 
# README: each step of the dev files is optional, and you don't have to 
# fill every dev scripts before getting started. 
# 01_start.R should be filled at start. 
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
# 
# 
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Add one line by package you want to add as dependency
usethis::use_package( "dplyr" )
usethis::use_package( "ggplot2" )
usethis::use_package( "tibble" )
usethis::use_package( "forcats" )
usethis::use_package( "shinyBS" )
usethis::use_package( "tidyr" )
usethis::use_package( "stringr" )
usethis::use_package( "finalfit" )
usethis::use_package( "dplyr" )
usethis::use_package( "DT" )

## Add modules ----
## Create a module infrastructure in R/
golem::add_module( name = "subset" ) # Name of the module
golem::add_module( name = "summary" ) # Name of the module
golem::add_module( name = "barplot" ) # Name of the module
golem::add_module( name = "render_barplot_ui" ) # Name of the module

## Add helper functions ----
## Creates ftc_* and utils_*
golem::add_fct( "summary" ) 
golem::add_utils( "palettes" )
golem::add_fct( "barplot" )

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file( "script" )
golem::add_js_handler( "handlers" )
golem::add_css_file( "custom" )

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw( name = "appdata", open = FALSE ) 

## Tests ----
## Add one line by test you want to create
usethis::use_test( "app" )

# Documentation

## Vignette ----
usethis::use_vignette("covidviz")
devtools::build_vignettes()

## Code coverage ----
## (You'll need GitHub there)
usethis::use_github()
usethis::use_travis()
usethis::use_appveyor()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")

