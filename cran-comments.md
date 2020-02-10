
## v0.2.3

This is a resbumission. I have reduced the tarball size by removing a vignette.

This is a patch update, in response to an automated CRAN check for the upcoming 
release of ggplot2 (v3.3.0).


## Test environments
* macOS 10.15 Catalina (local install; release)
* ubuntu 18.04 (on travis-ci; release and devel)
* ubuntu 16.04 (on travis-ci; release and devel)
* win-builder (release and devel)


## R CMD check results
There was 1 NOTE:
installed size is  6.5Mb
  sub-directories of 1Mb or more:
    doc   5.7Mb

This is a graphical plotting package; hence the use of images in the docs
is greater than a conventional R package. Future releases will endeavor
to reduce the image size.
  

## Downstream dependencies
There is one downstream package: permubiome. 
There were no errors or issues when `devtools::revdep()` was run.
