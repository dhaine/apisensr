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
Checking with 'devtools::check_rhub()' returned one error and one note.
The note is about possibly mis-spelled words in DESCRIPTION: Episensr and
episensr. These two words are correct (R package episensr).

The error is about failed installation. However the link received by email shows
success test
(https://builder.r-hub.io/status/apisensr_0.1.0.tar.gz-2a06ac5d319e4c2c87268fb14bf6cdfa).
