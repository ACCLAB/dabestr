# dabestr v2025.3.14
This release implements quite a couple of new features, matching up with the current DABEST in python.

### New features
* Plots now include swarm bars, contrast bars and delta dots
* Forest plots

# dabestr v2023.9.12
This release is a complete rebuild of dabestr. 

Previous functions from v0.3.0 and before are now depreciated.

#### Main API
This version of dabestr features a new Main API. The following functions have been sequentially organised for their intended procedural utilisation.

* `load()` 
processes and converts a tidy dataset into the dabestr format.

* `mean_diff()`, `median_diff()`, `cohens_d()`, `hedges_g()`, `cliffs_delta()`, `cohens_h()` 
Computes the effect size for each control-test group pairing in `idx`.

* `dabest_plot()` produces a Gardner-Altman estimation plot or a Cumming estimation plot depending on whether float_contrast is TRUE.

#### New features
* Plotting of shared control and repeated measures
* Proportion plots (unpaired and paired)
* Mini-Meta Delta plots
* Delta-Delta plots


# dabestr v0.3.0
* This is a breaking release that includes standardised effect sizes, and a bunch of bugfixes.

# dabestr v0.2.5
* This release implements the ability to supply a custom vector of colors to `palette` during plotting. There is also a bugfix that misreported the Ns for the test and control group.

# dabestr v0.2.4
* This version represents a resbumission, in response to an automated CRAN check for flavor r-patched-solaris-x86. See https://cran.r-project.org/web/checks/check_results_dabestr.html

# dabestr v0.2.3
* This patch fixes an issue that prevented Cumming plot generation, due to a ggplot2 new release (v3.3.0).

# dabestr v0.2.2
* This patch fixes an issue where local variables were duplicated in column names, requiring proper unquoting.

# dabestr v0.2.1
* This release fixes a bug due to the new version of `ellipsis`. (# 37).

# dabestr v0.2.0

* This release fixes a bug that plotted the categories alphabetically in Gardner-Altman plots. (# 24).

# dabestr v0.1.0

* First release of dabestr.
