
## v0.2.3
This is a patch update, in response to an automated CRAN check for the upcoming release of ggplot2 (v3.3.0).


## Test environments
* local OS X install (macOS Catalina; release)
* ubuntu 16.04 (on travis-ci; devel and release)
* win-builder (oldrelese, release, and devel)


## R CMD check results
There was 1 NOTE:
installed size is  6.5Mb
  sub-directories of 1Mb or more:
    doc   5.7Mb

This is a graphical plotting package; hence the use of images in the docs
is greater than a conventional R package. Future releases will endeavor
to reduce the image size.
  

## Downstream dependencies
There is one downstream package: permubiome. There were no errors or issues when `devtools::revdep()` was run.
