get_tick_labels <- function(plot.obj, axes) {
  # This works for ggplot2 v3.0.0; not fully tested with other versions yet.
  plot.obj.build <- ggplot_build(plot.obj)

  if (axes == "x") {
    ticks <- plot.obj.build$layout$panel_params[[1]]$x.labels
  } else if (axes == "y") {
    ticks <- plot.obj.build$layout$panel_params[[1]]$y.labels
  } else {
    stop('`axes` must be either "x" or "y"')
  }

  return(ticks)
}

max_nchar_ticks <- function(tick_list) {
  tick_nchars <- sapply(tick_list, nchar)
  return(max(tick_nchars))
}
