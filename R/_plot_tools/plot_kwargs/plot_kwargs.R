#' Helper functions that deal with assignment of plot_kwargs for plot
#' 
#' @description
#' Contains function `assign_plot_kwargs`.

#' Function that handles plot_kwargs for final plotting
assign_plot_kwargs <- function(plot_kwargs) {
  colour_col <- NULL
  swarm_label <- NULL
  contrast_label <- NULL
  custom_palette <- "d3"
  swarm_ylim <- NULL
  contrast_ylim <- NULL
  delta2_ylim <- NULL
  delta2_label <- NULL
  
  if(isFALSE(is.null(plot_kwargs$colour_col))) {
    colour_col <- plot_kwargs$colour_col
  }
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
  return(list(
    colour_col = colour_col,
    swarm_label = swarm_label,
    contrast_label = contrast_label,
    custom_palette = custom_palette,
    swarm_ylim = swarm_ylim,
    contrast_ylim = contrast_ylim,
    delta2_ylim = delta2_ylim,
    delta2_label = delta2_label
  ))
}

