#' Get tick labels from a ggplot object.
#'
#' Requires ggplot2 v3.0.0.
#' @param plot.obj A \code{ggplot} object.
#' @param axes string, either "x" or "y".
#'
#' @examples
#' my.plot <- ggplot(iris, aes(Species, Petal.Width)) + geom_boxplot()
#' my.plot.yticks <- get_tick_labels(my.plot, axes="y")
#' my.plot.xticks <- get_tick_labels(my.plot, axes="x")
#'
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

#' Get the longest character string from a list.
#'
#' @param tick_list A list of strings.
#'
#' @examples
#' my.plot <- ggplot(iris, aes(Species, Petal.Width)) + geom_boxplot()
#' my.plot.yticks <- get_tick_labels(my.plot, axes="y")
#' max_ticklength   <- max_nchar_ticks(my.plot.yticks)
#'
max_nchar_ticks <- function(tick_list) {
  tick_nchars <- sapply(tick_list, nchar)
  return(max(tick_nchars))
}
