## Resubmission
This is a resubmission. In this version I have:

* Contacted the maintainers of affected package(s) >= 2 weeks ago.

## R CMD check results

0 errors | 0 warnings | 3 note

1. checking CRAN incoming feasibility ... [29s] NOTE
```
Maintainer: 'Yishan Mai <maiyishan@u.duke.nus.edu>'
New maintainer:
 Yishan Mai <maiyishan@u.duke.nus.edu>
Old maintainer(s):
 Joses W. Ho <joseshowh@gmail.com>
CRAN repository db overrides:

 License_is_FOSS: yes 
```
Changing of maintainer.

2. checking for non-standard things in the check directory ... NOTE
```
Found the following files/directories:
  ''NULL''
```
As noted in [R-hub issue #560](https://github.com/r-hub/rhub/issues/560), this seems to be an Rhub issue and so can likely be ignored.

3. checking for detritus in the temp directory ... NOTE
```
Found the following files/directories:
  'lastMiKTeXException'
```
As noted in [R-hub issue #503](https://github.com/r-hub/rhub/issues/503), this could be due to a bug/crash in MiKTeX and can likely be ignored.

## revdepcheck results

We checked 1 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 1 packages

Issues with CRAN packages are summarised below.

### Failed to check

* permubiome (NA)

Maintainers of affected package(s) above have been informed for >= 2 weeks ago. No further changes to be made.