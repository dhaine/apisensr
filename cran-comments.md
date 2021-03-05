## Test environments
* x86_64-pc-linux-gnu (64-bit), R 4.0.4
* x86_64-pc-linux-gnu (64-bit) (on travis-ci), R 3.6.3
* win-builder (devel and release)

## R CMD check results
There was one NOTE:

checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Denis Haine <denis.haine@gmail.com>’
New submission

## Downstream dependencies
There are no downstream dependencies.

## Note
Checking with 'devtools::check_rhub()' returned one error.
The error is about failed installation. However the link received by email shows
success test.

Checking with 'devtools::check_win_devel()' reports a single NOTE, the same as
with R CMD check, i.e. New submission

Spelling of 'episensr' was fixed in DESCRIPTION file, using lower case and
single quotes.
Link for code of conduct was omitted.
