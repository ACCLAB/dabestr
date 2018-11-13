## Test environments
* local OS X install, R 3.5.0
* ubuntu 14.04.5 (on travis-ci, devel and release)
* win-builder (devel and release)


## R CMD check results
There were no ERRORs or WARNINGs.

There were 2 NOTEs:
* checking installed package size ... NOTE
  installed size is  6.2Mb
  sub-directories of 1Mb or more:
    doc   5.6Mb
    
  COMMENTS:
  The size of the installed package is over 5 MB the vignettes produce a large 
  number of plots. Future versions will gradually reduce the vignettes' sizes. 

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Joses W. Ho <joseshowh@gmail.com>'
  
  New submission
  
  Non-FOSS package license (file LICENSE)
  
  Possibly mis-spelled words in DESCRIPTION:
    ESTimation (12:52)
    swarmplot (19:40)
  
  Found the following (possibly) invalid DOIs:
    DOI: https://doi.org/10.1101/377978
      From: inst/CITATION
      Message: Invalid DOI

  COMMENTS: 
  The LICENSE does not match a FOSS template license because it contains the 
  original copyright license for "flat_violin.R".
  
  The words are not misspelled, and are valid.
  
  The DOI is valid, and can be accessed by modern browsers.


## Downstream dependencies
There are currently no downstream dependencies for this package.
