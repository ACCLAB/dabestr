# Contributing to dabestr

The guide is divided into three main pieces:

1. Filing a bug in an issue.
1. Filing a feature request in an issue.
1. Suggesting a change via a pull request.

Please note that dabestr is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project, you agree to abide by its terms.

## Issues

Ensure the bug was not already reported by searching in [Issues](https://github.com/ACCLAB/dabestr/issues). Be sure to also check that the bug hasn't been addressed in a closed issue.

If the bug isn't being addressed, open a new one. 

Be sure to include a title and clear description, and a [minimally reproducible code sample](http://adv-r.had.co.nz/Reproducibility.html) demonstrating the expected behavior that is not occurring. 

## Feature Requests

You may suggest by opening an issue and adding an Enhancement tag.

## Pull requests

To contribute a change to dabestr, you follow these steps:

1. Create a fork and make your changes.
1. Issue a PR to the dev branch of dabestr.
1. Discuss the PR.
1. Iterate until either we accept the PR or decide it's not a good fit for dabestr.

Each of these steps are described in more detail below.

If you're not familiar with git or github, please start by reading http://r-pkgs.had.co.nz/git.html

### Guidelines
1. **Clarity**. Ensure PR description clearly describes the problem and the solution. Include the relevant issue number if applicable.
1. **Use dabestr coding style**. dabestr is currently following the [official tidyverse style](http://style.tidyverse.org/). You may choose to do so using the `styler` package by running the following on the code before submitting:

```r
# install.packages("styler")
styler::style_pkg()
```
3. If you're adding new parameters or a new function, you'll also need to document them with [roxygen](https://github.com/klutometis/roxygen). Make sure to re-run `devtools::document()` on the code before submitting.
3. If fixing a bug or adding a new feature, please add a [testthat](https://github.com/r-lib/testthat) unit test.

All PRs require review and approval from at least one member of the dabestr development team before merge. 

Lastly, dabestr is a **community** tool for estimation statistics and analysis. We look forward to more robust and more elegant data visualizations from you all!

## Attribution
This Contributing guide is adapted from [ggplot2](https://ggplot2.tidyverse.org)'s [CONTRIBUTING.md](https://github.com/tidyverse/ggplot2/blob/main/CONTRIBUTING.md?plain=1).

