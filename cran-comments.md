## Resubmission
This is a resubmission. In this version I have:

* Export of the functions 'mod_about', 'mod_analysis', 'mod_notable',
  'mod_parms', 'mod_prob' was removed as they are not needed outside of the
  package. The Rd files are also no longer necessary.
* Description, returned value (i.e. none) and example were added to the function
  'run_app'.
* Seed was removed from the function 'mod_prob'. As such, I felt version number
  for the package should go from 0.1.0 to 0.2.0 (updated in Description file).

## Test environments
* Local machine
  * Running under: Ubuntu 20.04.3 LTS
  * Platform: x86_64-pc-linux-gnu (64-bit)
  * R version 4.1.1 (2021-08-10)
* Github Actions:
  * Windows-latest (R release)
  * MacOS-latest (R release)
  * Ubuntu 20.04 (R release, devel)
* win-builder (devel and release)

## R CMD check results
There was one NOTE:

checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Denis Haine <denis.haine@gmail.com>’
New submission

## Downstream dependencies
There are no downstream dependencies.

## Note
Checking with 'devtools::check_win_devel()' reports a single NOTE, the same as
with R CMD check, i.e. New submission
