% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/002_forest_plot.R
\name{forest_plot}
\alias{forest_plot}
\title{Generates a Forest Plot}
\usage{
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
}
\arguments{
\item{contrasts}{A list of contrast objects. These objects should contain the
statistical information for each comparison (e.g., estimates, standard errors).}

\item{contrast_labels}{A list of labels for the contrast objects. E.g.,
c('Drug1', 'Drug2', 'Drug3') These labels will be used to identify each comparison on the plot.}

\item{contrast_type}{Select between "delta2" (for delta-delta) or "minimeta"
for mini-meta analysis. This determines the type of effect size calculation
used in the plot.}

\item{effect_size}{Character string specifying the effect size metric to display.
Valid options include "mean_diff", "median_diff", "cliffs_delta", "cohens_d",
"hedges_g", or "delta_g". The default is "mean_diff".}

\item{ylabel}{Character string specifying the axis label for the dependent
variable (Y-axis for vertical layout, X-axis for horizontal layout).
The default is "value".}

\item{title}{Character string specifying the title for the forest plot.
The default is "Delta delta Forest".}

\item{fontsize}{Font size for text elements in the plot. Default is 12.}

\item{title_font_size}{Font size for text of plot title. Defaults is 16.}

\item{violin_kwargs}{Additional arguments for violin plot customization. Default is NULL}

\item{marker_size}{Marker size for plotting mean differences or effect sizes. Default is 20.}

\item{ci_line_width}{Width of confidence interval lines. Default is 2.5.}

\item{custom_palette}{A list or key:value pair of colors, one for each contrast object.
E.g., c('gray', 'blue', 'green') or c('Drug1'='gray', 'Drug2'='blue', 'Drug3'='green'). Default NULL.}

\item{rotation_for_xlabels}{Rotation angle for x-axis labels, improving readability. Default is 45.}

\item{alpha_violin_plot}{Transparency level for violin plots. Default is 0.8}
}
\value{
A ggplot object representing the forest plot.
}
\description{
This function creates a forest plot summarizing a list of contrasts.
}
