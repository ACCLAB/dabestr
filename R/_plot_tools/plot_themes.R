#' Contains <ggplot> themes for float_contrast == TRUE | FALSE
#' 
#' @description
#' Contains themes `float_contrast_theme` and `non_float_contrast_theme`.

#' Theme for left-right graph
float_contrast_theme <- 
  theme(plot.margin = ggplot2::unit(c(5.5, 0, 5.5, 0), "pt"),
        axis.line.x = ggplot2::element_blank(), 
        axis.title.x = ggplot2::element_blank(), 
        axis.ticks.x = ggplot2::element_blank(),
        axis.title.x.bottom = ggplot2::element_blank(),
        legend.title=element_blank())

#' Theme for top-down graph
non_float_contrast_theme <- 
  theme(axis.line.x = ggplot2::element_blank(), 
        axis.title.x = ggplot2::element_blank(), 
        axis.ticks.x = ggplot2::element_blank(),
        axis.title.x.bottom = ggplot2::element_blank(),
        axis.title.y = element_text(size = 10),
        legend.title=element_blank())