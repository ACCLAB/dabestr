% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/999_plot_kwargs.R
\name{plot_kwargs}
\alias{plot_kwargs}
\title{Adjustable Plot Aesthetics}
\description{
These are the available plot kwargs for adjusting the plot aesthetics of your
estimation plot:
\itemize{
\item \code{swarm_label} Default "value" or "proportion of success" for proportion plots.
Label for the y-axis of the swarm plot.
\item \code{contrast_label} Default "effect size", based on the effect sizes as given in \code{\link[=effect_size]{effect_size()}}.
Label for the y-axis of the contrast plot.
\item \code{delta2_label} Default NULL. Label for the y-label for the delta-delta plot.
\item \code{swarm_x_text} Default 11. Numeric value determining the font size of the x-axis of the swarm plot.
\item \code{swarm_y_text} Default 15. Numeric value determining the font size of the y-axis of the swarm plot.
\item \code{contrast_x_text} Default 11. Numeric value determining the font size of the x-axis of the delta plot.
\item \code{contrast_y_text} Default 15. Numeric value determining the font size of the y-axis of the delta plot.
\item \code{swarm_ylim} Default NULL. Vector containing the y limits for the swarm plot
\item \code{contrast_ylim} Default NULL. Vector containing the y limits for the delta plot.
\item \code{delta2_ylim} Default NULL. Vector containing the y limits for the delta-delta plot.
\item \code{raw_marker_size} Default 1.5. Numeric value determining the size of the points used in the swarm plot.
\item \code{tufte_size} Default 0.8. Numeric value determining the size of the tufte line in the swarm plot.
\item \code{es_marker_size} Default 0.5. Numeric value determining the size of the points used in the delta plot.
\item \code{es_line_size} Default 0.8. Numeric value determining the size of the ci line in the delta plot.
\item \code{raw_marker_alpha} Default 1. Numeric value determining the transparency of the points in the swarm plot.
\item \code{raw_bar_width} Default 0.3. Numeric value determining the width of the bar in the sankey diagram.
\item \code{raw_marker_spread} Default 2. The distance between the points if it is a swarm plot.
\item \code{raw_marker_side_shift} Default 0. The horizontal distance that the swarm plot points are moved in the
direction of the \code{asymmetric_side}.
\item \code{asymmetric_side} Default "right". Can be either "right" or "left". Controls which side the swarm points are shown.
\item \code{show_delta2} Default FALSE. Boolean value determining if the delta-delta plot is shown.
\item \code{show_mini_meta} Default FALSE. Boolean value determining if the weighted average plot is shown.
If False, the resulting graph would be identical to a multiple two-groups plot.
\item \code{show_zero_dot} Default TRUE. Boolean value determining if there is a dot on
the zero line of the effect size for the control-control group.
\item \code{show_baseline_ec} Default FALSE. Boolean value determining whether the baseline curve is shown.
\item \code{show_legend} Default TRUE. If TRUE, legend will be shown. If FALSE, legend will not be shown.
\item \code{sankey} Default TRUE. Boolean value determining if the flows between the bar charts will be plotted.
\item \code{raw_flow_alpha} Default 0.5. Numeric value determining the transparency of the sankey flows in a
paired proportion plot.
\item \code{flow} Default TRUE. Boolean value determining whether the bars will be plotted in pairs.
\item \code{custom_palette} Default "d3". String. The following palettes are available for use:
npg, aaas, nejm, lancet, jama, jco, ucscgb, d3, locuszoom, igv, cosmic, uchicago, brewer, ordinal, viridis_d.
\item \code{contrast_bars} Default TRUE. Whether or not to display the contrast bars at the delta plot.
\item \code{params_contrast_bars}. Default value: list(color = NULL, alpha = 0.3). Pass relevant keyword arguments to the contrast bars.
\item \code{swarm_bars} Default TRUE. Whether or not to display the swarm bars.
\item \code{params_swarm_bars}. Default value: list(color = NULL, alpha = 0.3). Pass relevant keyword arguments to the swarm bars.
}
}
