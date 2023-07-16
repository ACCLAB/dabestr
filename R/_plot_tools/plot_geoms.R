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

draw_panel_proportion_bar <- function(data, panel_scales, coord) {
  coords <- coord$transform(data, panel_scales) 
  
  failure_bar <- rectGrob(x = coords$x,
                          y = 1,
                          width = coords$width,
                          height = 1 - coords$y,
                          default.units = "native",
                          just = c("center", "top"),
                          gp = gpar(col = coords$colour,
                                    fill = "white",
                                    lwd = coords$size * .pt))
  
  success_bar <- rectGrob(x = coords$x,
                          y = 0,
                          width = coords$width,
                          height = coords$y,
                          default.units = "native",
                          just = c("center", "bottom"),
                          gp = gpar(col = coords$colour,
                                    fill = alpha(coords$fill, coords$alpha),
                                    lwd = coords$size * .pt))
  
  gTree(children = gList(failure_bar, success_bar))
  
}

GeomProportionBar <- ggproto("GeomProportionBar", GeomRect,
                             required_aes = c("x", "y"),
                             non_missing_aes = c("xmin", "xmax", "ymin", "ymax"),
                             default_aes = aes(colour = NA,
                                               width = 0.2,
                                               fill = "grey35",
                                               alpha = NA,
                                               lwd = 2,
                                               size = 0.7),
                             
                             set_up_data = function(data, params) {
                               data$width <- data$width %||% params$width
                               data <- transform(data,
                                                 ymin = pmin(y, 0), ymax = pmax(y, 1),
                                                 xmin = x - width, xmax = x + width,
                                                 width = width)
                               data
                             },
                             
                             draw_key = draw_key_polygon,
                             draw_panel = draw_panel_proportion_bar)

geom_proportionbar <- function(mapping = NULL, data = NULL, 
                               stat = "identity", position = "stack", 
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

# SankeyBar Geom
draw_panel_sankey_bar <- function(data, panel_scales, coord) {
  coords <- coord$transform(data, panel_scales) 
  
  failure_bar <- rectGrob(x = coords$x,
                          y = coords$yfailure + coords$gap/4,
                          width = coords$width,
                          height = 1 - coords$proportionsuccess - coords$gap/2,
                          gp = gpar(col = coords$col_failure,
                                    fill = alpha(coords$fill_failure, coords$alpha),
                                    lwd = coords$size * .pt))
  
  success_bar <- rectGrob(x = coords$x,
                          y = coords$ysuccess - coords$gap/4,
                          width = coords$width,
                          height = coords$proportionsuccess - coords$gap/2,
                          gp = gpar(col = coords$col_success,
                                    fill = alpha(coords$fill_success, coords$alpha),
                                    lwd = coords$size * .pt))
  
  gTree(children = gList(failure_bar, success_bar))
  
}

GeomSankeyBar <- ggproto("GeomSankeyBar", Geom,
                         required_aes = c("x", "yfailure", "ysuccess", "proportionsuccess"),
                         default_aes = aes(col = NA,
                                           width = 0.2,
                                           col_failure = NA,
                                           col_success = NA,
                                           fill_failure = "#818181",
                                           fill_success = "#db6159",
                                           alpha = 1,
                                           lwd = 2,
                                           size = 0.7,
                                           gap = 0.05),
                         draw_key = draw_key_rect,
                         draw_panel = draw_panel_sankey_bar)

geom_sankeybar <- function(mapping = NULL, data = NULL, stat = "identity", 
                           position = "identity", show.legend = NA, 
                           na.rm = FALSE, inherit.aes = TRUE, ...) {
  layer(data = data, 
        mapping = mapping,
        stat = stat,
        geom = GeomSankeyBar,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...))
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