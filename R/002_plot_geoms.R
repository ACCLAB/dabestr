# Contains custom <ggproto> geom_objects for plotting.
# 
# Contains main geoms `geom_halfviolin`, `geom_bootci`, `geom_proportionbar`, `geom_sankeybar` and `geom_sankeyflow`.
#' @importFrom ggplot2 .pt
# Halfviolin Geom
draw_group_halfviolin <- function(data, panel_scales, coord) {
  coords <- coord$transform(data, panel_scales) 
  
  first_row <- coords[1, , drop = FALSE]
  
  violin <- grid::polygonGrob(x = coords$x,
                        y = coords$y,
                        gp = grid::gpar(col = first_row$colour,
                                  fill = scales::alpha(first_row$fill, first_row$alpha)))
  
}

GeomHalfViolin <- ggplot2::ggproto("GeomHalfViolin", ggplot2::Geom,
                          required_aes = c("x", "y"),
                          default_aes = ggplot2::aes(colour = NA, 
                                            fill = "grey35",
                                            alpha = 0.8),
                          draw_key = ggplot2::draw_key_point,
                          draw_group = draw_group_halfviolin)

geom_halfviolin <- function(mapping = NULL, data = NULL, stat = "identity", 
                            position = "identity", show.legend = NA, 
                            na.rm = FALSE, inherit.aes = TRUE, ...) {
  ggplot2::layer(data = data, 
        mapping = mapping,
        stat = stat,
        geom = GeomHalfViolin,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...))
}

# Boot_CI Geom
draw_panel_boot_ci <- function(data, panel_scales, coord) {
  coords <- coord$transform(data, panel_scales) 
  
  ci_line <- grid::segmentsGrob(x0 = coords$x,
                          x1 = coords$x,
                          y0 = coords$ymin,
                          y1 = coords$ymax,
                          gp = grid::gpar(lwd = coords$linesize * .pt,
                                    lineend = coords$lineend))
  
  ci_dot <- grid::pointsGrob(x = coords$x,
                       y = coords$middle,
                       pch = coords$shape,
                       size = grid::unit(coords$dotsize, "char"))
  
  grid::gTree(children = grid::gList(ci_line, ci_dot))
  
}

GeomBootCI <- ggplot2::ggproto("GeomBootCI", ggplot2::Geom,
                      required_aes = c("x", "ymin", "ymax", "middle"),
                      default_aes = ggplot2::aes(linesize = 0.8,
                                        dotsize = 0.5,
                                        shape = 19, 
                                        lwd = 2,
                                        lineend = "square"),
                      draw_key = ggplot2::draw_key_point,
                      draw_panel = draw_panel_boot_ci)

geom_bootci <- function(mapping = NULL, data = NULL, stat = "identity", 
                        position = "identity", show.legend = NA, 
                        na.rm = FALSE, inherit.aes = TRUE, ...) {
  ggplot2::layer(data = data, 
        mapping = mapping,
        stat = stat,
        geom = GeomBootCI,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...))
}

# Proportion Bar Geom
"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

draw_group_proportion_bar <- function(data, panel_scales, coord) {
  coords <- coord$transform(data, panel_scales) 
  
  first_row <- coords[1, , drop = FALSE]
  
  failure_bar <- grid::polygonGrob(x = coords$x,
                             y = coords$y,
                             gp = grid::gpar(col = first_row$colour,
                                             fill = scales::alpha(first_row$fill, first_row$alpha)))
}

GeomProportionBar <- ggplot2::ggproto("GeomProportionBar", ggplot2::Geom,
                             required_aes = c("x", "y"),
                             default_aes = ggplot2::aes(colour = NA,
                                               fill = "white",
                                               alpha = NA),
                             draw_key = ggplot2::draw_key_polygon,
                             draw_group = draw_group_proportion_bar)

geom_proportionbar <- function(mapping = NULL, data = NULL, 
                               stat = "identity", position = "identity", 
                               ...,
                               show.legend = NA, 
                               na.rm = FALSE, 
                               inherit.aes = TRUE) {
  ggplot2::layer(data = data, 
        mapping = mapping,
        stat = stat,
        geom = GeomProportionBar,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
          na.rm = na.rm, 
          ...))
}

# SankeyFlow Geom
draw_group_sankey_flow <- function(data, panel_scales, coord) {
  coords <- coord$transform(data, panel_scales) 
  first_row <- coords[1, , drop = FALSE]
  
  flow <- grid::polygonGrob(x = coords$x,
                      y = coords$y,
                      gp = grid::gpar(col = first_row$colour,
                                fill = scales::alpha(first_row$fillcol, first_row$alpha)))
}

GeomSankeyFlow <- ggplot2::ggproto("GeomSankeyFlow", ggplot2::Geom,
                          required_aes = c("x", "y"),
                          default_aes = ggplot2::aes(colour = NA,
                                            fillcol = "gray50",
                                            alpha = 0.5),
                          draw_key = ggplot2::draw_key_polygon,
                          draw_group = draw_group_sankey_flow)

geom_sankeyflow <- function(mapping = NULL, data = NULL, stat = "identity", 
                            position = "identity", show.legend = NA, 
                            na.rm = FALSE, inherit.aes = TRUE, ...) {
  ggplot2::layer(data = data, 
        mapping = mapping,
        stat = stat,
        geom = GeomSankeyFlow,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...))
}