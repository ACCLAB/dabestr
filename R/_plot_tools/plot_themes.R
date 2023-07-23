#' Contains <ggplot> themes for float_contrast == TRUE | FALSE
#' 
#' @description
#' Contains themes `float_contrast_theme` and `non_float_contrast_theme`.

#' Theme for left-right graph
float_contrast_theme <- theme(plot.margin = ggplot2::unit(c(0, 0, 0, 0), "pt"),
                              axis.line.x = ggplot2::element_blank(), 
                              axis.title.x = ggplot2::element_blank(), 
                              axis.ticks.x = ggplot2::element_blank(),
                              axis.title.x.bottom = ggplot2::element_blank(),
                              legend.title = ggplot2::element_blank())

#' Theme for top-down graph
non_float_contrast_theme <- theme(plot.margin = ggplot2::unit(c(0, 0, 0, 0), "pt"),
                                  axis.line.x = ggplot2::element_blank(), 
                                  axis.title.x = ggplot2::element_blank(), 
                                  axis.ticks.x = ggplot2::element_blank(),
                                  axis.title.x.bottom = ggplot2::element_blank(),
                                  axis.title.y = ggplot2::element_text(size = 10),
                                  legend.title = ggplot2::element_blank())

#' Theme for removal of all axes and labels
remove_all_axes_theme <- theme(axis.line = ggplot2::element_blank(),
                               axis.title = ggplot2::element_blank(),
                               axis.ticks = ggplot2::element_blank(),
                               legend.title = ggplot2::element_blank(),
                               plot.margin = ggplot2::unit(c(0, 0, 0, 0), "pt"))