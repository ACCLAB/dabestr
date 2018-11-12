# Copyright (c) 2018 David Robinson, Micah Allen, Davide Poggiali, Kirstie
# Whitaker, Tom Rhys Marshall and Rogier Kievit.
#
# The R code in this file (referred to as the Software) is licensed under the
# MIT license.
#
# LICENSE:
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

#' @importFrom magrittr %>%
geom_flat_violin <- function(
  mapping     = NULL,
  data        = NULL,
  stat        = "ydensity",
  position    = "dodge",
  trim        = TRUE,
  scale       = "area",
  show.legend = NA,
  inherit.aes = TRUE, ...) {

  ggplot2::layer(
    data        = data,
    mapping     = mapping,
    stat        = stat,
    geom        = GeomFlatViolin,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = list(trim = trim,scale = scale, ...)
  )

}



"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}



GeomFlatViolin <-
  ggplot2::ggproto(
    "GeomFlatViolin",

    ggplot2::Geom,

    setup_data = function(data, params) {
      data$width <- data$width %||%
        params$width %||%
        (resolution(data$x, FALSE) * 0.9)
      # ymin, ymax, xmin, and xmax define the bounding rectangle for each group.
      data %>%
        dplyr::group_by(group) %>%
        dplyr::mutate(ymin = min(y),
                      ymax = max(y),
                      xmin = x,
                      xmax = x + width / 2)
    },

    draw_group = function(data, panel_scales, coord) {
      # Find the points for the line to go all the way around
      data <- transform(data, xminv = x,
                        xmaxv = x + violinwidth * (xmax - x))

      # Make sure it's sorted properly to draw the outline
      newdata <- rbind(plyr::arrange(transform(data, x = xminv), y),
                       plyr::arrange(transform(data, x = xmaxv), -y))

      # Close the polygon: set first and last point the same.
      # Needed for coord_polar and such.
      newdata <- rbind(newdata, newdata[1,])

      ggplot2:::ggname("geom_flat_violin",
                       ggplot2::GeomPolygon$draw_panel(
                         newdata, panel_scales, coord)
                       )
    },

    draw_key = ggplot2::draw_key_polygon,

    default_aes = ggplot2::aes(weight = 1, colour = "grey20",
                               fill = "grey", size = 0.5,
                               alpha = NA, linetype = "solid"),

    required_aes = c("x", "y")
  )
