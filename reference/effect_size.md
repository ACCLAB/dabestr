# Calculating effect sizes

Computes the effect size for each control-test group pairing in `idx`.
The resampling bootstrap distribution of the effect size is then
subjected to Bias-corrected and accelerated bootstrap (BCa) correction.

The following effect sizes `mean_diff`, `median_diff`, `cohens_d`,
`hedges_g` and `cliffs_delta` are used for most plot types.

## Usage

``` r
mean_diff(dabest_obj, perm_count = 5000)

median_diff(dabest_obj, perm_count = 5000)

cohens_d(dabest_obj, perm_count = 5000)

hedges_g(dabest_obj, perm_count = 5000)

cliffs_delta(dabest_obj, perm_count = 5000)

cohens_h(dabest_obj, perm_count = 5000)
```

## Arguments

- dabest_obj:

  A dabest_obj created by loading in dataset along with other specified
  parameters with the
  [`load()`](https://acclab.github.io/dabestr/reference/load.md)
  function.

- perm_count:

  The number of reshuffles of control and test labels to be performed
  for each p-value.

## Value

Returns a `dabest_effectsize_obj` list with 22 elements. The following
are the elements contained within:

- `raw_data` The tidy dataset passed to
  [`load()`](https://acclab.github.io/dabestr/reference/load.md) that
  was cleaned and altered for plotting.

- `idx` The list of control-test groupings as initially passed to
  [`load()`](https://acclab.github.io/dabestr/reference/load.md).

- `delta_x_labels` Vector containing labels for the x-axis of the delta
  plot.

- `delta_y_labels` String label for the y-axis of the delta plot.

- `Ns` List of labels for x-axis of the raw plot.

- `raw_y_labels` Vector containing labels for the y-axis of the raw
  plot.

- `is_paired` Boolean value determining if it is a paired plot.

- `is_colour` Boolean value determining if there is a colour column for
  the plot.

- `paired` Paired ("sequential" or "baseline") as initially passed to
  [`load()`](https://acclab.github.io/dabestr/reference/load.md).

- `resamples` The number of resamples to be used to generate the effect
  size bootstraps.

- `control_summary` Numeric value for plotting of control summary lines
  for float_contrast = `TRUE`.

- `test_summary` Numeric value for plotting of control summary lines for
  float_contrast = `TRUE`.

- `ylim` Vector containing the y limits for the raw plot.

- `enquo_x` Quosure of x as initially passed to
  [`load()`](https://acclab.github.io/dabestr/reference/load.md).

- `enquo_y` Quosure of y as initially passed to
  [`load()`](https://acclab.github.io/dabestr/reference/load.md).

- `enquo_id_col` Quosure of id_col as initially passed to
  [`load()`](https://acclab.github.io/dabestr/reference/load.md).

- `enquo_colour` Quosure of colour as initially passed to
  [`load()`](https://acclab.github.io/dabestr/reference/load.md).

- `proportional` Boolean value as initially passed to
  [`load()`](https://acclab.github.io/dabestr/reference/load.md).

- `minimeta` Boolean value as initially passed to
  [`load()`](https://acclab.github.io/dabestr/reference/load.md).

- `delta` Boolean value as initially passed to
  [`load()`](https://acclab.github.io/dabestr/reference/load.md).

- `proportional_data` List of calculations related to the plotting of
  proportion plots.

- `boot_result` List containing values related to the calculation of the
  effect sizes, bootstrapping and BCa correction.

- `baseline_ec_boot_result` List containing values related to the
  calculation of the effect sizes, bootstrapping and BCa correction for
  the baseline error curve.

- `permtest_pvals` List containing values related to the calculations of
  permutation t tests and the corresponding p values, and p values for
  different types of effect sizes and different statistical tests.

## Details

The plot types listed under here are limited to use only the following
effect sizes.

- Proportion plots offers only `mean_diff` and `cohens_h`.

- Mini-Meta Delta plots offers only `mean_diff`.

The other plots are able to use all given basic effect sizes as listed
in the Description.

## Examples

``` r
# Loading of the dataset
data(non_proportional_data)

# Applying effect size to the dabest object
dabest_obj <- load(non_proportional_data,
  x = Group, y = Measurement,
  idx = c("Control 1", "Test 1")
)
dabest_obj.mean_diff <- mean_diff(dabest_obj)

# Printing dabest effectsize object
print(dabest_obj.mean_diff)
#> DABESTR v2025.3.14
#> ==================
#> 
#> Good morning!
#> The current time is 07:05 AM on Wednesday July 29, 2026.
#> 
#> The character(0) mean difference between Test 1 and Control 1 is 0.585 [95%CI 0.307, 0.869].
#> The p-value of the two-sided permutation t-test is 0.0004, calculated for legacy purposes only.
#> 
#> 5000 bootstrap samples were taken; the confidence interval is bias-corrected and accelerated.
#> Any p-value reported is the probability of observing the effect size (or greater),
#> assuming the null hypothesis of zero difference is true.
#> For each p-value, 5000 reshuffles of the control and test labels were performed.
#> 
```
