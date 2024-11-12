#' Contains custom <ggproto> geom_objects for plotting.
#'
#' List of geom_*:
#' - `geom_halfviolin`
#' - `geom_bootci`,
#' - `geom_proportionbar`
#' - `geom_sankeyflow`.
#'
#' @importFrom ggplot2 .pt
#' @noRd
# Halfviolin Geom
draw_group_halfviolin <- function(data, panel_scales, coord) {
  coords <- coord$transform(data, panel_scales)

  first_row <- coords[1, , drop = FALSE]
  violin <- grid::polygonGrob(
    x = coords$x,
    y = coords$y,
    gp = grid::gpar(
      col = first_row$colour,
      fill = scales::alpha(first_row$fill, first_row$alpha)
    )
  )
}

# TODO Add documentation
GeomHalfViolin <- ggplot2::ggproto("GeomHalfViolin", ggplot2::Geom,
  required_aes = c("x", "y"),
  default_aes = ggplot2::aes(
    colour = NA,
    fill = "grey35",
    alpha = 0.8
  ),
  draw_key = ggplot2::draw_key_point,
  draw_group = draw_group_halfviolin
)

geom_halfviolin <- function(mapping = NULL, data = NULL, stat = "identity",
                            position = "identity", show.legend = NA,
                            na.rm = FALSE, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomHalfViolin,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

# Boot_CI Geom
draw_panel_boot_ci <- function(data, panel_scales, coord) {
  coords <- coord$transform(data, panel_scales)

  # For flipped coordinates, we need to swap x and y coordinates
  if (inherits(coord, "CoordFlip")) {
    # Use y values where we previously used x
    if (length(coords$y) > 0 && length(coords$middle) > 0) {
      ci_line <- grid::segmentsGrob(
        y0 = coords$y,
        y1 = coords$y,
        x0 = coords$xmin,
        x1 = coords$xmax,
        gp = grid::gpar(
          lwd = coords$linesize * .pt,
          lineend = coords$lineend
        )
      )
      # Calculate the middle point position
      middle_x <- (coords$xmin + coords$xmax) / 2
      ci_dot <- grid::pointsGrob(
        y = coords$y,
        x = middle_x,
        pch = coords$shape,
        size = grid::unit(coords$dotsize, "char")
      )
      grid::gTree(children = grid::gList(ci_line, ci_dot))
    } else {
      warning("Non valid y and middle coordinates found")
      grid::nullGrob()
    }
  } else {
    ci_line <- grid::segmentsGrob(
      x0 = coords$x,
      x1 = coords$x,
      y0 = coords$ymin,
      y1 = coords$ymax,
      gp = grid::gpar(
        lwd = coords$linesize * .pt,
        lineend = coords$lineend
      )
    )

    ci_dot <- grid::pointsGrob(
      x = coords$x,
      y = coords$middle,
      pch = coords$shape,
      size = grid::unit(coords$dotsize, "char")
    )
    grid::gTree(children = grid::gList(ci_line, ci_dot))
  }
}

# TODO Add documentation
GeomBootCI <- ggplot2::ggproto("GeomBootCI", ggplot2::Geom,
  required_aes = c("x", "ymin", "ymax", "middle"),
  default_aes = ggplot2::aes(
    linesize = 0.8,
    dotsize = 0.5,
    shape = 19,
    lwd = 2,
    lineend = "square"
  ),
  draw_key = ggplot2::draw_key_point,
  draw_panel = draw_panel_boot_ci
)

geom_bootci <- function(mapping = NULL, data = NULL, stat = "identity",
                        position = "identity", show.legend = NA,
                        na.rm = FALSE, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomBootCI,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

draw_group_proportion_bar <- function(data, panel_scales, coord) {
  coords <- coord$transform(data, panel_scales)

  first_row <- coords[1, , drop = FALSE]

  failure_bar <- grid::polygonGrob(
    x = coords$x,
    y = coords$y,
    gp = grid::gpar(
      col = first_row$colour,
      fill = scales::alpha(first_row$fill, first_row$alpha)
    )
  )
}

# TODO Add documentation
GeomProportionBar <- ggplot2::ggproto("GeomProportionBar", ggplot2::Geom,
  required_aes = c("x", "y"),
  default_aes = ggplot2::aes(
    colour = NA,
    fill = "white",
    alpha = NA
  ),
  draw_key = ggplot2::draw_key_polygon,
  draw_group = draw_group_proportion_bar
)

geom_proportionbar <- function(mapping = NULL, data = NULL,
                               stat = "identity", position = "identity",
                               ...,
                               show.legend = NA,
                               na.rm = FALSE,
                               inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomProportionBar,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

# SankeyFlow Geom
draw_group_sankey_flow <- function(data, panel_scales, coord) {
  coords <- coord$transform(data, panel_scales)
  first_row <- coords[1, , drop = FALSE]

  flow <- grid::polygonGrob(
    x = coords$x,
    y = coords$y,
    gp = grid::gpar(
      col = first_row$colour,
      fill = scales::alpha(first_row$fill, first_row$alpha)
    )
  )
}

# TODO Add documentation
GeomSankeyFlow <- ggplot2::ggproto("GeomSankeyFlow", ggplot2::Geom,
  required_aes = c("x", "y"),
  default_aes = ggplot2::aes(
    colour = NA,
    fill = "gray50",
    alpha = NA
  ),
  draw_key = ggplot2::draw_key_polygon,
  draw_group = draw_group_sankey_flow
)

geom_sankeyflow <- function(mapping = NULL, data = NULL, stat = "identity",
                            position = "identity", show.legend = NA,
                            na.rm = FALSE, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomSankeyFlow,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
