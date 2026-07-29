# Generates a Forest Plot

This function creates a forest plot summarizing a list of contrasts.

## Usage

``` r
forest_plot(
  contrasts,
  contrast_labels,
  contrast_type = "delta2",
  effect_size = "mean_diff",
  ylabel = "effect size",
  title = "Delta Delta Forest",
  fontsize = 12,
  title_font_size = 16,
  violin_kwargs = NULL,
  marker_size = 1.1,
  ci_line_width = 1.3,
  custom_palette = NULL,
  rotation_for_xlabels = 0,
  alpha_violin_plot = 0.8
)
```

## Arguments

- contrasts:

  A list of contrast objects. These objects should contain the
  statistical information for each comparison (e.g., estimates, standard
  errors).

- contrast_labels:

  A list of labels for the contrast objects. E.g., c('Drug1', 'Drug2',
  'Drug3') These labels will be used to identify each comparison on the
  plot.

- contrast_type:

  Select between "delta2" (for delta-delta) or "minimeta" for mini-meta
  analysis. This determines the type of effect size calculation used in
  the plot.

- effect_size:

  Character string specifying the effect size metric to display. Valid
  options include "mean_diff", "median_diff", "cliffs_delta",
  "cohens_d", "hedges_g", or "delta_g". The default is "mean_diff".

- ylabel:

  Character string specifying the axis label for the dependent variable
  (Y-axis for vertical layout, X-axis for horizontal layout). The
  default is "value".

- title:

  Character string specifying the title for the forest plot. The default
  is "Delta delta Forest".

- fontsize:

  Font size for text elements in the plot. Default is 12.

- title_font_size:

  Font size for text of plot title. Defaults is 16.

- violin_kwargs:

  Additional arguments for violin plot customization. Default is NULL

- marker_size:

  Marker size for plotting mean differences or effect sizes. Default is
  20.

- ci_line_width:

  Width of confidence interval lines. Default is 2.5.

- custom_palette:

  A list or key:value pair of colors, one for each contrast object.
  E.g., c('gray', 'blue', 'green') or c('Drug1'='gray', 'Drug2'='blue',
  'Drug3'='green'). Default NULL.

- rotation_for_xlabels:

  Rotation angle for x-axis labels, improving readability. Default is
  45.

- alpha_violin_plot:

  Transparency level for violin plots. Default is 0.8

## Value

A ggplot object representing the forest plot.
