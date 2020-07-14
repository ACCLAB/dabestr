
# dabestr

[![Travis CI build status](https://img.shields.io/travis/com/ACCLAB/dabestr/master.svg)](https://travis-ci.com/ACCLAB/dabestr/) [![minimal R version](https://img.shields.io/badge/R%3E%3D-3.5.0-6666ff.svg)](https://cran.r-project.org/)   [![CRAN Status Badge](https://www.r-pkg.org/badges/version-last-release/dabestr?color=green)](https://cran.r-project.org/package=dabestr) [![CRAN Download Count](https://cranlogs.r-pkg.org/badges/grand-total/dabestr?color=brightgreen)](https://cran.r-project.org/package=dabestr) [![Free-to-view citation](https://zenodo.org/badge/DOI/10.1038/s41592-019-0470-3.svg)](https://rdcu.be/bHhJ4) [![License](https://img.shields.io/badge/License-BSD%203--Clause--Clear-orange.svg)](https://spdx.org/licenses/BSD-3-Clause-Clear.html)

## Contents

- [About](#about)
- [Requirements](#requirements)
- [Installation](#installation)
- [Usage](#usage)
- [How to cite](#how-to-cite)
- [Bugs](#bugs)
- [Contributing](#contributing)
- [dabestr in other languages](#dabestr-in-other-languages)

## About

dabestr is a package for **D**ata **A**nalysis using **B**ootstrap-Coupled **EST**imation.


[Estimation statistics](https://en.wikipedia.org/wiki/Estimation_statistics "Estimation Stats on Wikipedia") is a [simple framework](https://thenewstatistics.com/itns/ "Introduction to the New Statistics") that avoids the [pitfalls](https://www.nature.com/articles/nmeth.3288 "The fickle P value generates irreproducible results, Halsey et al 2015") of significance testing. It uses familiar statistical concepts: means, mean differences, and error bars. More importantly, it focuses on the effect size of one's experiment/intervention, as opposed to a false dichotomy engendered by *P* values.

An estimation plot has two key features.

1.  It **presents all datapoints** as a swarmplot, which orders each point to display the underlying distribution.

2.  It presents the **effect size** as a **bootstrap 95% confidence interval** on a **separate but aligned axes**.

![](man/figures/README-gardner.altman.showpieces-1.png)![](man/figures/README-gardner.altman.showpieces-2.png)

![](man/figures/README-cumming.showpieces-1.png)![](man/figures/README-cumming.showpieces-2.png)![](man/figures/README-cumming.showpieces-3.png)

## Requirements

Your version of R must be 3.5.0 or higher.


## Installation

``` r
install.packages("dabestr")

# To install the latest development version on Github,
# use the line below.
devtools::install_github("ACCLAB/dabestr")
```

## Usage

``` r
library(dabestr)

# Performing unpaired (two independent groups) analysis.
unpaired_mean_diff <- dabest(iris, Species, Petal.Width,
                             idx = c("setosa", "versicolor", "virginica"),
                             paired = FALSE) %>% 
                      mean_diff()

# Display the results in a user-friendly format.
unpaired_mean_diff
#> DABEST (Data Analysis with Bootstrap Estimation in R) v0.3.0
#> ============================================================
#> 
#> Good morning!
#> The current time is 11:10 AM on Monday July 13, 2020.
#> 
#> Dataset    :  iris
#> X Variable :  Species
#> Y Variable :  Petal.Width
#> 
#> Unpaired mean difference of versicolor (n=50) minus setosa (n=50)
#>  1.08 [95CI  1.01; 1.14]
#> 
#> Unpaired mean difference of virginica (n=50) minus setosa (n=50)
#>  1.78 [95CI  1.69; 1.85]
#> 
#> 
#> 5000 bootstrap resamples.
#> All confidence intervals are bias-corrected and accelerated.

# Produce a Cumming estimation plot.
plot(unpaired_mean_diff)
```

![](man/figures/README-usage-1.png)

You will find more useful code snippets in this [vignette](http://bit.ly/using-dabestr).

## How to cite

**Moving beyond P values: Everyday data analysis with estimation plots**

*Joses Ho, Tayfun Tumkaya, Sameer Aryal, Hyungwon Choi, Adam Claridge-Chang*

Nature Methods 2019, 1548-7105. [10.1038/s41592-019-0470-3](http://dx.doi.org/10.1038/s41592-019-0470-3)

[Paywalled publisher site](https://www.nature.com/articles/s41592-019-0470-3); [Free-to-view PDF](https://rdcu.be/bHhJ4)


## Bugs

Please open a [new issue](https://github.com/ACCLAB/dabestr/issues/new). Include a reproducible example (aka [reprex](https://www.tidyverse.org/help/)) so anyone can copy-paste your code and move quickly towards helping you out!


## Contributing

All contributions are welcome; please read the [Guidelines for contributing](https://github.com/ACCLAB/dabestr/blob/master/CONTRIBUTING.md) first.

We also have a [Code of Conduct](https://github.com/ACCLAB/dabestr/blob/master/CODE_OF_CONDUCT.md) to foster an inclusive and productive space.


## dabestr in other languages

dabestr is also available in [Python](https://github.com/ACCLAB/DABEST-python "DABEST-Python on Github") and [Matlab](https://github.com/ACCLAB/DABEST-Matlab "DABEST-Matlab on Github").
