## Test environments
* local OS X install, R 3.5.0
* ubuntu 14.04.5 (on travis-ci, devel and release)
* win-builder (devel and release)


## R CMD check results
There were no ERRORs or WARNINGs.

There were 3 NOTEs:

* The installed package size is over 5 MB because the vignettes have a large number of plots. Future releases will progressively reduce the filesize of the vignettes.

* Non-FOSS package license (file LICENSE)
  The LICENSE does not match a FOSS template license because it contains the original copyright license for "flat_violin.R".

* Found the following (possibly) invalid URLs:
    URL: https://www.jstor.org/stable/2246110
      From: man/dabest.Rd
      Status: 403
      Message: Forbidden
      
    DOI: https://doi.org/10.1101/377978
      From: inst/CITATION
      Message: Invalid DOI
      
  Both the DOI and URL are valid, and can be accessed by modern browsers.


## Downstream dependencies
There are currently no downstream dependencies for this package.
