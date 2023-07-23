#' Contains custom <ggproto> geom_objects for plotting.
#' 
#' @description
#' Contains main geoms `geom_halfviolin`, `geom_bootci`, `geom_proportionbar`, `geom_sankeybar` and `geom_sankeyflow`.

# Halfviolin Geom
draw_group_halfviolin <- function(data, panel_scales, coord) {
  coords <- coord$transform(data, panel_scales) 
  
  first_row <- coords[1, , drop = FALSE]
  
  violin <- polygonGrob(x = coords$x,
                        y = coords$y,
                        gp = gpar(col = first_row$colour,
                                  fill = alpha(first_row$fill, first_row$alpha)))
  
}

GeomHalfViolin <- ggproto("GeomHalfViolin", Geom,
                          required_aes = c("x", "y"),
                          default_aes = aes(colour = NA, 
                                            fill = "grey35",
                                            alpha = 0.8),
                          draw_key = draw_key_point,
                          draw_group = draw_group_halfviolin)

geom_halfviolin <- function(mapping = NULL, data = NULL, stat = "identity", 
                            position = "identity", show.legend = NA, 
                            na.rm = FALSE, inherit.aes = TRUE, ...) {
  layer(data = data, 
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
  
  ci_line <- segmentsGrob(x0 = coords$x,
                          x1 = coords$x,
                          y0 = coords$ymin,
                          y1 = coords$ymax,
                          gp = gpar(lwd = coords$linesize * .pt,
                                    lineend = coords$lineend))
  
  ci_dot <- pointsGrob(x = coords$x,
                       y = coords$middle,
                       pch = coords$shape,
                       size = unit(coords$dotsize, "char"))
  
  gTree(children = gList(ci_line, ci_dot))
  
}

GeomBootCI <- ggproto("GeomBootCI", Geom,
                      required_aes = c("x", "ymin", "ymax", "middle"),
                      default_aes = aes(linesize = 0.8,
                                        dotsize = 0.5,
                                        shape = 19, 
                                        lwd = 2,
                                        lineend = "square"),
                      draw_key = draw_key_point,
                      draw_panel = draw_panel_boot_ci)

geom_bootci <- function(mapping = NULL, data = NULL, stat = "identity", 
                        position = "identity", show.legend = NA, 
                        na.rm = FALSE, inherit.aes = TRUE, ...) {
  layer(data = data, 
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
  
  failure_bar <- polygonGrob(x = coords$x,
                             y = coords$y,
                             gp = gpar(col = first_row$colour,
                                       fill = alpha(first_row$fill, first_row$alpha)))
}

GeomProportionBar <- ggproto("GeomProportionBar", Geom,
                             required_aes = c("x", "y"),
                             default_aes = aes(colour = "black",
                                               fill = "white",
                                               alpha = NA),
                             draw_key = draw_key_polygon,
                             draw_group = draw_group_proportion_bar)

geom_proportionbar <- function(mapping = NULL, data = NULL, 
                               stat = "identity", position = "identity", 
                               ...,
                               show.legend = NA, 
                               na.rm = FALSE, 
                               inherit.aes = TRUE) {
  layer(data = data, 
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
  
  flow <- polygonGrob(x = coords$x,
                      y = coords$y,
                      gp = gpar(col = first_row$colour,
                                fill = alpha(first_row$fillcol, first_row$alpha)))
}

GeomSankeyFlow <- ggproto("GeomSankeyFlow", Geom,
                          required_aes = c("x", "y"),
                          default_aes = aes(colour = NA,
                                            fillcol = "gray50",
                                            alpha = 0.5),
                          draw_key = draw_key_polygon,
                          draw_group = draw_group_sankey_flow)

geom_sankeyflow <- function(mapping = NULL, data = NULL, stat = "identity", 
                            position = "identity", show.legend = NA, 
                            na.rm = FALSE, inherit.aes = TRUE, ...) {
  layer(data = data, 
        mapping = mapping,
        stat = stat,
        geom = GeomSankeyFlow,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...))
}