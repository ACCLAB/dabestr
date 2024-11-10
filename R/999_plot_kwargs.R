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
#' - `raw_marker_spread` Default 2 and Default 2.5 for horizontal plots. The distance between the points if it is a swarm plot.
#' - `raw_marker_side_shift` Default 0. The horizontal distance that the swarm plot points are moved in the
#' direction of the `asymmetric_side`.
#' - `asymmetric_side` Default "right". Can be either "right" or "left". Controls which side the swarm points are shown.
#' - `show_delta2` Default FALSE. Boolean value determining if the delta-delta plot is shown.
#' - `show_mini_meta` Default FALSE. Boolean value determining if the weighted average plot is shown.
#' If False, the resulting graph would be identical to a multiple two-groups plot.
#' - `show_zero_dot` Default TRUE. Boolean value determining if there is a dot on
#' the zero line of the effect size for the control-control group.
#' - `show_baseline_ec` Default FALSE. Boolean value determining whether the baseline curve is shown.
#' - `show_legend` Default TRUE. If TRUE, legend will be shown. If FALSE, legend will not be shown.
#' - `sankey` Default TRUE. Boolean value determining if the flows between the bar charts will be plotted.
#' - `raw_flow_alpha` Default 0.5. Numeric value determining the transparency of the sankey flows in a
#' paired proportion plot.
#' - `flow` Default TRUE. Boolean value determining whether the bars will be plotted in pairs.
#' - `custom_palette` Default "d3". String. The following palettes are available for use:
#' npg, aaas, nejm, lancet, jama, jco, ucscgb, d3, locuszoom, igv, cosmic, uchicago, brewer, ordinal, viridis_d.
#' - `contrast_bars` Default TRUE. Whether or not to display the contrast bars at the delta plot.
#' - `params_contrast_bars`. Default value: list(color = NULL, alpha = 0.3). Pass relevant keyword arguments to the contrast bars.
#' - `swarm_bars` Default TRUE. Whether or not to display the swarm bars.
#' - `params_swarm_bars`. Default value: list(color = NULL, alpha = 0.3). Pass relevant keyword arguments to the swarm bars.
#'
#'
assign_plot_kwargs <- function(dabest_effectsize_obj, plot_kwargs) {
  check_effectsize_object(dabest_effectsize_obj)
  custom_palette <- "d3"

  swarm_label <- dabest_effectsize_obj$raw_y_labels
  contrast_label <- dabest_effectsize_obj$delta_y_labels
  delta2_label <- NULL

  swarm_ylim <- NULL
  contrast_ylim <- NULL
  delta2_ylim <- NULL

  show_delta2 <- dabest_effectsize_obj$delta2
  show_mini_meta <- dabest_effectsize_obj$minimeta
  horizontal <- FALSE
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
  show_legend <- TRUE

  sankey <- TRUE
  flow <- TRUE

  if (!(is.null(plot_kwargs$swarm_label))) {
    swarm_label <- plot_kwargs$swarm_label
  }
  if (!(is.null(plot_kwargs$contrast_label))) {
    contrast_label <- plot_kwargs$contrast_label
  }
  if (!(is.null(plot_kwargs$custom_palette))) {
    custom_palette <- plot_kwargs$custom_palette
  }
  if (!(is.null(plot_kwargs$swarm_ylim))) {
    swarm_ylim <- plot_kwargs$swarm_ylim
  }
  if (!(is.null(plot_kwargs$contrast_ylim))) {
    contrast_ylim <- plot_kwargs$contrast_ylim
  }
  if (!(is.null(plot_kwargs$delta2_ylim))) {
    delta2_ylim <- plot_kwargs$delta2_ylim
  }
  if (!(is.null(plot_kwargs$delta2_label))) {
    delta2_label <- plot_kwargs$delta2_label
  }
  if (!(is.null(plot_kwargs$show_delta2))) {
    show_delta2 <- plot_kwargs$show_delta2
  }
  if (!(is.null(plot_kwargs$show_mini_meta))) {
    show_mini_meta <- plot_kwargs$show_mini_meta
  }
  if (!(is.null(plot_kwargs$raw_marker_size))) {
    raw_marker_size <- plot_kwargs$raw_marker_size
  }
  if (!(is.null(plot_kwargs$raw_marker_alpha))) {
    raw_marker_alpha <- plot_kwargs$raw_marker_alpha
  }
  if (!(is.null(plot_kwargs$raw_marker_side_shift))) {
    raw_marker_side_shift <- plot_kwargs$raw_marker_side_shift
  }
  if (!(is.null(plot_kwargs$tufte_size))) {
    tufte_size <- plot_kwargs$tufte_size
  }
  if (!(is.null(plot_kwargs$es_marker_size))) {
    es_marker_size <- plot_kwargs$es_marker_size
  }
  if (!(is.null(plot_kwargs$es_line_size))) {
    es_line_size <- plot_kwargs$es_line_size
  }
  if (!(is.null(plot_kwargs$raw_bar_width))) {
    raw_bar_width <- plot_kwargs$raw_bar_width
  }
  if (!(is.null(plot_kwargs$horizontal))) {
    horizontal <- plot_kwargs$horizontal
  }
  if (!(is.null(plot_kwargs$raw_marker_spread))) {
    raw_marker_spread <- plot_kwargs$raw_marker_spread
  } else if (horizontal) {
    raw_marker_spread <- 2.5
  }
  if (!(is.null(plot_kwargs$sankey))) {
    sankey <- plot_kwargs$sankey
  }
  if (!(is.null(plot_kwargs$flow))) {
    flow <- plot_kwargs$flow
  }
  if (!(is.null(plot_kwargs$raw_flow_alpha))) {
    raw_flow_alpha <- plot_kwargs$raw_flow_alpha
  }
  if (!(is.null(plot_kwargs$swarm_y_text))) {
    swarm_y_text <- plot_kwargs$swarm_y_text
  }
  if (!(is.null(plot_kwargs$swarm_x_text))) {
    swarm_x_text <- plot_kwargs$swarm_x_text
  }
  if (!(is.null(plot_kwargs$contrast_y_text))) {
    contrast_y_text <- plot_kwargs$contrast_y_text
  }
  if (!(is.null(plot_kwargs$contrast_x_text))) {
    contrast_x_text <- plot_kwargs$contrast_x_text
  }
  if (!(is.null(plot_kwargs$show_zero_dot))) {
    show_zero_dot <- plot_kwargs$show_zero_dot
  }
  if (!(is.null(plot_kwargs$show_baseline_ec))) {
    show_baseline_ec <- plot_kwargs$show_baseline_ec
  }
  if (!(is.null(plot_kwargs$show_legend))) {
    show_legend <- plot_kwargs$show_legend
  }
  if (!(is.null(plot_kwargs$asymmetric_side))) {
    asymmetric_side <- plot_kwargs$asymmetric_side
  }
  contrast_bars <- TRUE
  if (!(is.null(plot_kwargs$contrast_bars))) {
    contrast_bars <- plot_kwargs$contrast_bars
  }
  # Swarm bars
  swarm_bars <- TRUE
  if (!(is.null(plot_kwargs$swarm_bars))) {
    swarm_bars <- plot_kwargs$swarm_bars
  }
  # Swarm bars kwargs
  default_params_swarm_bars <- list(
    color = NULL,
    alpha = 0.3
  )
  if (is.null(plot_kwargs$params_swarm_bars)) {
    # If user has not provided params_swarm_bars, use defaults
    params_swarm_bars <- default_params_swarm_bars
  } else {
    # If user has provided params_swarm_bars, update defaults with user values
    params_swarm_bars <- utils::modifyList(
      default_params_swarm_bars,
      plot_kwargs$params_swarm_bars
    )
  }
  # Contrast bars kwargs.
  default_params_contrast_bars <- list(
    color = NULL,
    alpha = 0.3
  )
  if (is.null(plot_kwargs$params_contrast_bars)) {
    # If user has not provided params_contrast_bars, use defaults
    params_contrast_bars <- default_params_contrast_bars
  } else {
    # If user has provided params_contrast_bars,
    # update defaults with user values
    params_contrast_bars <- utils::modifyList(
      default_params_contrast_bars,
      plot_kwargs$params_contrast_bars
    )
  }
  delta_text <- TRUE
  if (!(is.null(plot_kwargs$delta_text))) {
    delta_text <- plot_kwargs$delta_text
  }
  delta_dots <- TRUE
  if (!(is.null(plot_kwargs$delta_dots))) {
    delta_dots <- plot_kwargs$delta_dots
  }

  # Delta dots kwargs.
  default_params_delta_dots <- list(
    "pch" = 17, # default dot symbol is triangle
    "alpha" = 0.5,
    "cex" = 2,
    "size" = 2, # size 3 is too big
    "side" = "right"
  )
  if (is.null(plot_kwargs$params_delta_dots)) {
    params_delta_dots <- default_params_delta_dots
  } else {
    params_delta_dots <- utils::modifyList(
      default_params_delta_dots,
      plot_kwargs$params_delta_dots
    )
  }
  # Delta text kwargs.
  default_params_delta_text <- list(
    "color" = NULL,
    "alpha" = 1,
    "fontsize" = 10,
    "ha" = "center", # hjust
    "va" = "center", # vjust
    "rotation" = 0,
    "x_location" = "right",
    "x_coordinates" = NULL,
    "y_coordinates" = NULL,
    "x_adjust" = 0
  )
  if (is.null(plot_kwargs$params_delta_text)) {
    params_delta_text <- default_params_delta_text
  } else {
    params_delta_text <- utils::modifyList(
      default_params_delta_text,
      plot_kwargs$params_delta_text
    )
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
    show_legend = show_legend,
    asymmetric_side = asymmetric_side,
    horizontal = horizontal,
    contrast_bars = contrast_bars,
    params_contrast_bars = params_contrast_bars,
    swarm_bars = swarm_bars,
    params_swarm_bars = params_swarm_bars,
    delta_text = delta_text,
    params_delta_text = params_delta_text,
    delta_dots = delta_dots,
    params_delta_dots = params_delta_dots
  ))
}
