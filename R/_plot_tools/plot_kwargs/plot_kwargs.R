#' Helper functions that deal with assignment of plot_kwargs for plot
#' 
#' @description
#' Contains function `assign_plot_kwargs`.

#' Function that handles plot_kwargs for final plotting
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
  
  raw_marker_size <- 1.5
  raw_marker_alpha <- 1
  raw_marker_spread <- 2
  raw_bar_width <- 0.3
  tufte_size <- 0.8
  es_marker_size <- 0.5
  es_line_size <- 0.8
  
  sankey <- TRUE
  
  if(isFALSE(is.null(plot_kwargs$swarm_label))) {
    swarm_label <- plot_kwargs$swarm_label
  }
  if(isFALSE(is.null(plot_kwargs$contrast_label))) {
    contrast_label <- plot_kwargs$contrast_label
  }
  if(isFALSE(is.null(plot_kwargs$custom_palette))) {
    custom_palette <- plot_kwargs$custom_palette
  }
  if(isFALSE(is.null(plot_kwargs$swarm_ylim))) {
    swarm_ylim <- plot_kwargs$swarm_ylim
  }
  if(isFALSE(is.null(plot_kwargs$contrast_ylim))) {
    contrast_ylim <- plot_kwargs$contrast_ylim
  }
  if(isFALSE(is.null(plot_kwargs$delta2_ylim))) {
    delta2_ylim <- plot_kwargs$delta2_ylim
  }
  if(isFALSE(is.null(plot_kwargs$delta2_label))) {
    delta2_label <- plot_kwargs$delta2_label
  }
  if(isFALSE(is.null(plot_kwargs$show_delta2))) {
    show_delta2 <- plot_kwargs$show_delta2
  }
  if(isFALSE(is.null(plot_kwargs$show_mini_meta))) {
    show_mini_meta <- plot_kwargs$show_mini_meta
  }
  if(isFALSE(is.null(plot_kwargs$raw_marker_size))) {
    raw_marker_size <- plot_kwargs$raw_marker_size
  }
  if(isFALSE(is.null(plot_kwargs$raw_marker_alpha))) {
    raw_marker_alpha <- plot_kwargs$raw_marker_alpha
  }
  if(isFALSE(is.null(plot_kwargs$tufte_size))) {
    tufte_size <- plot_kwargs$tufte_size
  }
  if(isFALSE(is.null(plot_kwargs$es_marker_size))) {
    es_marker_size <- plot_kwargs$es_marker_size
  }
  if(isFALSE(is.null(plot_kwargs$es_line_size))) {
    es_line_size <- plot_kwargs$es_line_size
  }
  if(isFALSE(is.null(plot_kwargs$raw_bar_width))) {
    raw_bar_width <- plot_kwargs$raw_bar_width
  }
  if(isFALSE(is.null(plot_kwargs$raw_marker_spread))) {
    raw_marker_spread <- plot_kwargs$raw_marker_spread
  }
  if(isFALSE(is.null(plot_kwargs$sankey))) {
    sankey <- plot_kwargs$sankey
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
    raw_bar_width = raw_bar_width,
    tufte_size = tufte_size,
    es_marker_size = es_marker_size,
    es_line_size = es_line_size,
    sankey = sankey
  ))
}