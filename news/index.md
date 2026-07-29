# Changelog

## dabestr v2025.3.14

CRAN release: 2025-02-26

This release implements quite a couple of new features, matching up with
the current DABEST in python.

#### New features

- Plots now include swarm bars, contrast bars and delta dots
- Forest plots

## dabestr v2023.9.12

CRAN release: 2023-10-13

This release is a complete rebuild of dabestr.

Previous functions from v0.3.0 and before are now depreciated.

##### Main API

This version of dabestr features a new Main API. The following functions
have been sequentially organised for their intended procedural
utilisation.

- [`load()`](https://acclab.github.io/dabestr/reference/load.md)
  processes and converts a tidy dataset into the dabestr format.

- [`mean_diff()`](https://acclab.github.io/dabestr/reference/effect_size.md),
  [`median_diff()`](https://acclab.github.io/dabestr/reference/effect_size.md),
  [`cohens_d()`](https://acclab.github.io/dabestr/reference/effect_size.md),
  [`hedges_g()`](https://acclab.github.io/dabestr/reference/effect_size.md),
  [`cliffs_delta()`](https://acclab.github.io/dabestr/reference/effect_size.md),
  [`cohens_h()`](https://acclab.github.io/dabestr/reference/effect_size.md)
  Computes the effect size for each control-test group pairing in `idx`.

- [`dabest_plot()`](https://acclab.github.io/dabestr/reference/dabest_plot.md)
  produces a Gardner-Altman estimation plot or a Cumming estimation plot
  depending on whether float_contrast is TRUE.

##### New features

- Plotting of shared control and repeated measures
- Proportion plots (unpaired and paired)
- Mini-Meta Delta plots
- Delta-Delta plots

## dabestr v0.3.0

CRAN release: 2020-07-13

- This is a breaking release that includes standardised effect sizes,
  and a bunch of bugfixes.

## dabestr v0.2.5

CRAN release: 2020-04-20

- This release implements the ability to supply a custom vector of
  colors to `palette` during plotting. There is also a bugfix that
  misreported the Ns for the test and control group.

## dabestr v0.2.4

CRAN release: 2020-03-21

- This version represents a resbumission, in response to an automated
  CRAN check for flavor r-patched-solaris-x86. See
  <https://cran.r-project.org/web/checks/check_results_dabestr.html>

## dabestr v0.2.3

CRAN release: 2020-02-11

- This patch fixes an issue that prevented Cumming plot generation, due
  to a ggplot2 new release (v3.3.0).

## dabestr v0.2.2

CRAN release: 2019-07-04

- This patch fixes an issue where local variables were duplicated in
  column names, requiring proper unquoting.

## dabestr v0.2.1

CRAN release: 2019-06-26

- This release fixes a bug due to the new version of `ellipsis`. (# 37).

## dabestr v0.2.0

CRAN release: 2019-01-07

- This release fixes a bug that plotted the categories alphabetically in
  Gardner-Altman plots. (# 24).

## dabestr v0.1.0

CRAN release: 2018-11-19

- First release of dabestr.
