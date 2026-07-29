# Loading data with dabestr

Processes and converts a tidy dataset into the dabestr format. The
output of this function is then used as an input for various procedural
functions within dabestr to create estimation plots.

## Usage

``` r
load(
  data,
  x,
  y,
  idx = NULL,
  paired = NULL,
  id_col = NULL,
  ci = 95,
  resamples = 5000,
  colour = NULL,
  proportional = FALSE,
  minimeta = FALSE,
  delta2 = FALSE,
  experiment = NULL,
  experiment_label = NULL,
  x1_level = NULL
)
```

## Arguments

- data:

  A tidy dataframe.

- x:

  Column in `data` that contains the treatment groups.

- y:

  Column in `data` that contains the measurement values.

- idx:

  List of control-test groupings for which the effect size will be
  computed for.

- paired:

  Paired ("sequential" or "baseline"). Used for plots for experiments
  with repeated-measures designs.

  If "sequential", comparison happens between each measurement to the
  one directly preceding it. (control vs group i)

  If "baseline", comparison happens between each group to a shared
  control. (group i vs group i+1)

- id_col:

  Column in `data` indicating the identity of the datapoint if the data
  is tagged. Compulsory parameter if paired is TRUE.

- ci:

  Default 95. Determines the range of the confidence interval for effect
  size and bootstrap calculations. Only accepts values between 0 to 100
  (inclusive).

- resamples:

  The number of resamples to be used to generate the effect size
  bootstraps.

- colour:

  Column in `data` that determines the groupings for colour of the
  swarmplot as opposed to `x`.

- proportional:

  Boolean value determining if proportion plots are being produced.

- minimeta:

  Boolean value determining if mini-meta analysis is conducted.

- delta2:

  Boolean value determining if delta-delta analysis for 2 by 2
  experimental designs is conducted.

- experiment:

  Experiment column name for delta-delta analysis.

- experiment_label:

  String specifying the experiment label that is used to distinguish the
  experiment and the factors (being used in the plotting labels).

- x1_level:

  String setting the first factor level in a 2 by 2 experimental design.

## Value

Returns a `dabest_obj` list with 18 elements. The following are the
elements contained within:

- `raw_data` The tidy dataset passed to `load()` that was cleaned and
  altered for plotting.

- `proportional_data` List of calculations related to the plotting of
  proportion plots.

- `enquo_x` Quosure of x as initially passed to `load()`.

- `enquo_y` Quosure of y as initially passed to `load()`.

- `enquo_id_col` Quosure of id_col as initially passed to `load()`.

- `enquo_colour` Quosure of colour as initially passed to `load()`.

- `proportional` Boolean value determining if proportion plots are being
  produced.

- `minimeta` Boolean value determining if mini-meta analysis is
  conducted.

- `delta2` Boolean value determining if delta-delta analysis for 2 by 2
  experimental designs is conducted.

- `idx` List of control-test groupings for which the effect size will be
  computed for.

- `resamples` The number of resamples to be used to generate the effect
  size bootstraps.

- `is_paired` Boolean value determining if it is a paired plot.

- `is_colour` Boolean value determining if there is a specified colour
  column for the plot.

- `paired` Paired ("sequential" or "baseline") as initially passed to
  `load()`.

- `ci` Numeric value which determines the range of the confidence
  interval for effect size and bootstrap calculations. Only accepts
  values between 0 to 100 (inclusive).

- `Ns` List of labels for x-axis of the rawdata swarm plot.

- `control_summary` Numeric value for plotting of control summary lines
  for float_contrast= TRUE.

- `test_summary` Numeric value for plotting of test summary lines for
  float_contrast = TRUE.

- `ylim` Vector containing the y limits for the rawdata swarm plot.

## Examples

``` r
# Loading in of the dataset
data(non_proportional_data)

# Creating a dabest object
dabest_obj <- load(
  data = non_proportional_data, x = Group, y = Measurement,
  idx = c("Control 1", "Test 1")
)

# Printing dabest object
print(dabest_obj)
#> DABESTR v2025.3.14
#> ==================
#> 
#> Good morning!
#> The current time is 07:05 AM on Wednesday July 29, 2026.
#> 
#> ffect size(s) with 95% confidence intervals will be computed for:
#> 1. Test 1 minus Control 1
#> 
#> 5000 resamples will be used to generate the effect size bootstraps.
#> 
```
