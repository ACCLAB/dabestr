get_tick_labels <- function(plot.obj, axes) {

  plot.obj.build <- ggplot2::ggplot_build(plot.obj)
  panel.params <- plot.obj.build$layout$panel_params[[1]]

  #### Modified in v0.2.3, update for ggplot2>=v3.3.0.
  if (utils::packageVersion("ggplot2") <= "3.2.1") {
    xticks <- panel.params$x.labels
    yticks <- panel.params$y.labels
  } else {
    xticks <- panel.params$x$get_labels()
    yticks <- panel.params$y$get_labels()
  }

  if (axes == "x") {
    return(xticks[!is.na(xticks)])
  } else if (axes == "y") {
    return(yticks[!is.na(yticks)])
  } else {
    stop('`axes` must be either "x" or "y"')
  }

}


#### Deprecated in v0.2.3; not used in package.
# get_tick_positions <- function(plot.obj, axes) {
#   # This works for ggplot2 v3.0.0; not fully tested with other versions yet.
#   plot.obj.build <- ggplot2::ggplot_build(plot.obj)
#
#   if (axes == "x") {
#     pos <- plot.obj.build$layout$panel_params[[1]]$y.minor_source
#   } else if (axes == "y") {
#     pos <- plot.obj.build$layout$panel_params[[1]]$y.major_source
#   } else {
#     stop('`axes` must be either "x" or "y"')
#   }
#
#   return(pos)
# }



max_nchar_ticks <- function(tick_list) {
  tick_nchars <- sapply(tick_list, nchar)
  return(max(tick_nchars))
}
