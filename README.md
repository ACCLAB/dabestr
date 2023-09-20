
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dabestr <img src="man/figures/logo.png" align="right" height="139" /></a>

<!-- badges: start -->

[![minimal R
version](https://img.shields.io/badge/R%3E%3D-2.10-6666ff.svg)](https://cran.r-project.org/)
[![CRAN Download
Count](https://cranlogs.r-pkg.org/badges/grand-total/dabestr?color=brightgreen)](https://cran.r-project.org/package=dabestr)
[![Free-to-view
citation](https://zenodo.org/badge/DOI/10.1038/s41592-019-0470-3.svg)](https://www.nature.com/articles/s41592-019-0470-3.epdf?author_access_token=Euy6APITxsYA3huBKOFBvNRgN0jAjWel9jnR3ZoTv0Pr6zJiJ3AA5aH4989gOJS_dajtNr1Wt17D0fh-t4GFcvqwMYN03qb8C33na_UrCUcGrt-Z0J9aPL6TPSbOxIC-pbHWKUDo2XsUOr3hQmlRew%3D%3D)
[![License](https://img.shields.io/badge/License-Apache_2.0-orange.svg)](https://spdx.org/licenses/BSD-3-Clause-Clear.html)
[![R-CMD-check](https://github.com/sunroofgod/dabestr-prototype/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sunroofgod/dabestr-prototype/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

<!-- ## Overview -->

dabestr is a package for **D**ata **A**nalysis using
**B**ootstrap-Coupled **EST**imation.

[Estimation
statistics](https://en.wikipedia.org/wiki/Estimation_statistics "Estimation Stats on Wikipedia")
is a [simple
framework](https://thenewstatistics.com/itns/ "Introduction to the New Statistics")
that avoids the
[pitfalls](https://www.nature.com/articles/nmeth.3288 "The fickle P value generates irreproducible results, Halsey et al 2015")
of significance testing. It uses familiar statistical concepts: means,
mean differences, and error bars. More importantly, it focuses on the
effect size of one’s experiment/intervention, as opposed to a false
dichotomy engendered by *P* values.

An estimation plot has two key features.

1.  It **presents all datapoints** as a swarmplot, which orders each
    point to display the underlying distribution.

2.  It presents the **effect size** as a **bootstrap 95% confidence
    interval** on a **separate but aligned axes**.

## Installation

``` r
# Install it from CRAN
install.packages("dabestr")

# Or the development version from GitHub:
# install.packages("devtools")
devtools::install_github(repo = "ACCLAB/dabestr", ref = "dev")
```

## Usage

``` r
library(dabestr)
```

``` r
data("non_proportional_data")

dabest_obj.mean_diff <- load(
  data = non_proportional_data,
  x = Group,
  y = Measurement,
  idx = c("Control 1", "Test 1")
) %>%
  mean_diff()

dabest_plot(dabest_obj.mean_diff, TRUE)
```

![](man/figures/README-unnamed-chunk-4-1.png)<!-- -->

## Citation

**Moving beyond P values: Everyday data analysis with estimation plots**

*Joses Ho, Tayfun Tumkaya, Sameer Aryal, Hyungwon Choi, Adam
Claridge-Chang*

Nature Methods 2019, 1548-7105.
[10.1038/s41592-019-0470-3](http://dx.doi.org/10.1038/s41592-019-0470-3)

[Paywalled publisher
site](https://www.nature.com/articles/s41592-019-0470-3); [Free-to-view
PDF](https://www.nature.com/articles/s41592-019-0470-3.epdf?author_access_token=Euy6APITxsYA3huBKOFBvNRgN0jAjWel9jnR3ZoTv0Pr6zJiJ3AA5aH4989gOJS_dajtNr1Wt17D0fh-t4GFcvqwMYN03qb8C33na_UrCUcGrt-Z0J9aPL6TPSbOxIC-pbHWKUDo2XsUOr3hQmlRew%3D%3D)

## Contributing

All contributions are welcome; please read the [Guidelines for
contributing](https://github.com/ACCLAB/dabestr/blob/master/CONTRIBUTING.md)
first.

We also have a [Code of
Conduct](https://github.com/ACCLAB/dabestr/blob/master/CODE_OF_CONDUCT.md)
to foster an inclusive and productive space.

## Dabestr in other languages

dabestr is also available in
[Python](https://github.com/ACCLAB/DABEST-python "DABEST-Python on Github")
and
[Matlab](https://github.com/ACCLAB/DABEST-Matlab "DABEST-Matlab on Github").
