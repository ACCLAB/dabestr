# Helper functions that deal with assignment of plot_kwargs for plot
#
# Contains function `assign_plot_kwargs`.

#' Adjustable Plot Aesthetics
#' @name plot_kwargs
#'
#' @description
#' These are the available plot kwargs for adjusting the plot aesthetics of your
#' estimation plot:
#'
#' - `swarm_label` Default "value" or "proportion of success" for proportion plots.
#' Label for the y-axis of the swarm plot.
#' - `contrast_label` Default "effect size", based on the effect sizes as given in [effect_size()].
#' Label for the y-axis of the contrast plot.
#' - `delta2_label` Default NULL. Label for the y-label for the delta-delta plot.
#' - `swarm_x_text` Default 11. Numeric value determining the font size of the x-axis of the swarm plot.
#' - `swarm_y_text` Default 15. Numeric value determining the font size of the y-axis of the swarm plot.
#' - `contrast_x_text` Default 11. Numeric value determining the font size of the x-axis of the delta plot.
#' - `contrast_y_text` Default 15. Numeric value determining the font size of the y-axis of the delta plot.
#' - `swarm_ylim` Default NULL. Vector containing the y limits for the swarm plot
#' - `contrast_ylim` Default NULL. Vector containing the y limits for the delta plot.
#' - `delta2_ylim` Default NULL. Vector containing the y limits for the delta-delta plot.
#' - `raw_marker_size` Default 1.5. Numeric value determining the size of the points used in the swarm plot.
#' - `tufte_size` Default 0.8. Numeric value determining the size of the tufte line in the swarm plot.
#' - `es_marker_size` Default 0.5. Numeric value determining the size of the points used in the delta plot.
#' - `es_line_size` Default 0.8. Numeric value determining the size of the ci line in the delta plot.
#' - `raw_marker_alpha` Default 1. Numeric value determining the transparency of the points in the swarm plot.
#' - `raw_bar_width` Default 0.3. Numeric value determining the width of the bar in the sankey diagram.
#' - `raw_marker_spread` Default 2. The distance between the points if it is a swarm plot.
#' - `raw_marker_side_shift` Default 0. The horizontal distance that the swarm plot points are moved in the
#' direction of the `asymmetric_side`.
#' - `asymmetric_side` Default "right". Can be either "right" or "left". Controls which side the swarm points are shown.
#' - `show_delta2` Default FALSE. Boolean value determining if the delta-delta plot is shown.
#' - `show_mini_meta` Default FALSE. Boolean value determining if the weighted average plot is shown.
#' If False, the resulting graph would be identical to a multiple two-groups plot.
#' - `show_zero_dot` Default TRUE. Boolean value determining if there is a dot on
#' the zero line of the effect size for the control-control group.
#' - `show_baseline_ec` Default FALSE. Boolean value determining whether the baseline curve is shown.
#' - `sankey` Default TRUE. Boolean value determining if the flows between the bar charts will be plotted.
#' - `raw_flow_alpha` Default 0.5. Numeric value determining the transparency of the sankey flows in a
#' paired proportion plot.
#' - `flow` Default TRUE. Boolean value determining whether the bars will be plotted in pairs.
#' - `custom_palette` Default "d3". String. The following palettes are available for use:
#' npg, aaas, nejm, lancet, jama, jco, ucscgb, d3, locuszoom, igv, cosmic, uchicago, brewer, ordinal, viridis_d.
#'
#'
NULL

assign_plot_kwargs <- function(dabest_effectsize_obj, plot_kwargs) {
  custom_palette <- "d3"

  swarm_label <- dabest_effectsize_obj$raw_y_labels
  contrast_label <- dabest_effectsize_obj$delta_y_labels
  delta2_label <- NULL

  swarm_ylim <- NULL
  contrast_ylim <- NULL
  delta2_ylim <- NULL

  show_delta2 <- dabest_effectsize_obj$delta2
  show_mini_meta <- dabest_effectsize_obj$minimeta

  asymmetric_side <- "right"
  raw_marker_size <- 1.5
  raw_marker_alpha <- 1
  raw_marker_spread <- 2
  raw_marker_side_shift <- 0
  raw_flow_alpha <- 0.5
  raw_bar_width <- 0.3
  tufte_size <- 0.8
  es_marker_size <- 0.5
  es_line_size <- 0.8

  swarm_y_text <- 15
  swarm_x_text <- 11
  contrast_y_text <- 15
  contrast_x_text <- 11

  show_zero_dot <- TRUE
  show_baseline_ec <- FALSE

  sankey <- TRUE
  flow <- TRUE

  if (isFALSE(is.null(plot_kwargs$swarm_label))) {
    swarm_label <- plot_kwargs$swarm_label
  }
  if (isFALSE(is.null(plot_kwargs$contrast_label))) {
    contrast_label <- plot_kwargs$contrast_label
  }
  if (isFALSE(is.null(plot_kwargs$custom_palette))) {
    custom_palette <- plot_kwargs$custom_palette
  }
  if (isFALSE(is.null(plot_kwargs$swarm_ylim))) {
    swarm_ylim <- plot_kwargs$swarm_ylim
  }
  if (isFALSE(is.null(plot_kwargs$contrast_ylim))) {
    contrast_ylim <- plot_kwargs$contrast_ylim
  }
  if (isFALSE(is.null(plot_kwargs$delta2_ylim))) {
    delta2_ylim <- plot_kwargs$delta2_ylim
  }
  if (isFALSE(is.null(plot_kwargs$delta2_label))) {
    delta2_label <- plot_kwargs$delta2_label
  }
  if (isFALSE(is.null(plot_kwargs$show_delta2))) {
    show_delta2 <- plot_kwargs$show_delta2
  }
  if (isFALSE(is.null(plot_kwargs$show_mini_meta))) {
    show_mini_meta <- plot_kwargs$show_mini_meta
  }
  if (isFALSE(is.null(plot_kwargs$raw_marker_size))) {
    raw_marker_size <- plot_kwargs$raw_marker_size
  }
  if (isFALSE(is.null(plot_kwargs$raw_marker_alpha))) {
    raw_marker_alpha <- plot_kwargs$raw_marker_alpha
  }
  if (isFALSE(is.null(plot_kwargs$raw_marker_side_shift))) {
    raw_marker_side_shift <- plot_kwargs$raw_marker_side_shift
  }
  if (isFALSE(is.null(plot_kwargs$tufte_size))) {
    tufte_size <- plot_kwargs$tufte_size
  }
  if (isFALSE(is.null(plot_kwargs$es_marker_size))) {
    es_marker_size <- plot_kwargs$es_marker_size
  }
  if (isFALSE(is.null(plot_kwargs$es_line_size))) {
    es_line_size <- plot_kwargs$es_line_size
  }
  if (isFALSE(is.null(plot_kwargs$raw_bar_width))) {
    raw_bar_width <- plot_kwargs$raw_bar_width
  }
  if (isFALSE(is.null(plot_kwargs$raw_marker_spread))) {
    raw_marker_spread <- plot_kwargs$raw_marker_spread
  }
  if (isFALSE(is.null(plot_kwargs$sankey))) {
    sankey <- plot_kwargs$sankey
  }
  if (isFALSE(is.null(plot_kwargs$flow))) {
    flow <- plot_kwargs$flow
  }
  if (isFALSE(is.null(plot_kwargs$raw_flow_alpha))) {
    raw_flow_alpha <- plot_kwargs$raw_flow_alpha
  }
  if (isFALSE(is.null(plot_kwargs$swarm_y_text))) {
    swarm_y_text <- plot_kwargs$swarm_y_text
  }
  if (isFALSE(is.null(plot_kwargs$swarm_x_text))) {
    swarm_x_text <- plot_kwargs$swarm_x_text
  }
  if (isFALSE(is.null(plot_kwargs$contrast_y_text))) {
    contrast_y_text <- plot_kwargs$contrast_y_text
  }
  if (isFALSE(is.null(plot_kwargs$contrast_x_text))) {
    contrast_x_text <- plot_kwargs$contrast_x_text
  }
  if (isFALSE(is.null(plot_kwargs$show_zero_dot))) {
    show_zero_dot <- plot_kwargs$show_zero_dot
  }
  if (isFALSE(is.null(plot_kwargs$show_baseline_ec))) {
    show_baseline_ec <- plot_kwargs$show_baseline_ec
  }
  if (isFALSE(is.null(plot_kwargs$asymmetric_side))) {
    asymmetric_side <- plot_kwargs$asymmetric_side
  }

  return(list(
    swarm_label = swarm_label,
    contrast_label = contrast_label,
    custom_palette = custom_palette,
    swarm_ylim = swarm_ylim,
    contrast_ylim = contrast_ylim,
    delta2_ylim = delta2_ylim,
    delta2_label = delta2_label,
    show_delta2 = show_delta2,
    show_mini_meta = show_mini_meta,
    raw_marker_size = raw_marker_size,
    raw_marker_alpha = raw_marker_alpha,
    raw_marker_spread = raw_marker_spread,
    raw_marker_side_shift = raw_marker_side_shift,
    raw_bar_width = raw_bar_width,
    tufte_size = tufte_size,
    es_marker_size = es_marker_size,
    es_line_size = es_line_size,
    sankey = sankey,
    flow = flow,
    raw_flow_alpha = raw_flow_alpha,
    swarm_y_text = swarm_y_text,
    swarm_x_text = swarm_x_text,
    contrast_y_text = contrast_y_text,
    contrast_x_text = contrast_x_text,
    show_zero_dot = show_zero_dot,
    show_baseline_ec = show_baseline_ec,
    asymmetric_side = asymmetric_side
  ))
}
