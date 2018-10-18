get_tick_labels <- function(plot.obj, axes) {
  # This works for ggplot2 v3.0.0; not fully tested with other versions yet.
  plot.obj.build <- ggplot2::ggplot_build(plot.obj)

  if (axes == "x") {
    ticks <- plot.obj.build$layout$panel_params[[1]]$x.labels
  } else if (axes == "y") {
    ticks <- plot.obj.build$layout$panel_params[[1]]$y.labels
  } else {
    stop('`axes` must be either "x" or "y"')
  }

  return(ticks)
}



get_tick_positions <- function(plot.obj, axes) {
  # This works for ggplot2 v3.0.0; not fully tested with other versions yet.
  plot.obj.build <- ggplot2::ggplot_build(plot.obj)

  if (axes == "x") {
    pos <- plot.obj.build$layout$panel_params[[1]]$y.minor_source
  } else if (axes == "y") {
    pos <- plot.obj.build$layout$panel_params[[1]]$y.major_source
  } else {
    stop('`axes` must be either "x" or "y"')
  }

  return(pos)
}



max_nchar_ticks <- function(tick_list) {
  tick_nchars <- sapply(tick_list, nchar)
  return(max(tick_nchars))
}
