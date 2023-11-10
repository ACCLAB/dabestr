#' Producing an estimation plot
#'
#' @name dabest_plot
#'
#' @param dabest_effectsize_obj A dabest_effectsize_obj created by loading in a
#' dabest_obj along with other specified parameters with the [effect_size()] function.
#' @param float_contrast Default TRUE. If TRUE, a Gardner-Altman plot will be produced.
#' If FALSE, a Cumming estimation plot will be produced.
#' @param show_legend Default TRUE. If TRUE, legend will be shown. If FALSE, legend 
#' will not be shown.
#' @param ... Adjustment parameters to control and adjust the appearance of the plot.
#' (list of all possible adjustment parameters can be found under [plot_kwargs])
#'
#' @description
#' Produces a Gardner-Altman estimation plot or a Cumming estimation plot depending
#' on whether float_contrast is TRUE. The plot presents all datapoints as a swarmplot,
#' which orders each point to display the underlying distribution. It also presents
#' the effect size as a bootstrap 95% confidence interval (95% CI) on a separate
#' but aligned axes.
#'
#' @usage
#' dabest_plot(dabest_effectsize_obj, float_contrast = TRUE, show_legend = TRUE, ...)
#'
#' @examples
#' # Loading of the dataset
#' data(twogroup_data)
#'
#' # Preparing the data to be plotted
#' dabest_obj <- load(non_proportional_data,
#'   x = Group, y = Measurement,
#'   idx = c("Control 1", "Test 1")
#' )
#' dabest_obj.mean_diff <- mean_diff(dabest_obj)
#'
#' # Plotting an estimation plot
#' dabest_plot(dabest_obj.mean_diff, TRUE, TRUE)
#'
#' @export

dabest_plot <- function(dabest_effectsize_obj, float_contrast = TRUE, show_legend = TRUE, ...) {
  if (!methods::is(dabest_effectsize_obj, "dabest_effectsize")) {
    cli::cli_abort(c("{.field dabest_effectsize_obj} must be a {.cls dabest_effectsize} object."),
      "x" = "Please supply a {.cls dabest_effectsize} object."
    )
  }

  plot_kwargs <- list(...)
  plot_kwargs <- assign_plot_kwargs(dabest_effectsize_obj, plot_kwargs)

  custom_palette <- plot_kwargs$custom_palette

  is_colour <- dabest_effectsize_obj$is_colour
  is_deltadelta <- plot_kwargs$show_delta2
  is_mini_meta <- plot_kwargs$show_mini_meta
  idx <- dabest_effectsize_obj$idx
  raw_legend <- NULL

  if (length(unlist(idx)) >= 3) {
    float_contrast <- FALSE
  }

  if (isFALSE(float_contrast)) {
    raw_plot <- plot_raw(dabest_effectsize_obj, float_contrast = FALSE, plot_kwargs)
    delta_plot <- plot_delta(dabest_effectsize_obj, float_contrast = FALSE, plot_kwargs)

    delta_range <- delta_plot$delta_range
    delta_plot <- delta_plot$delta_plot

    raw_plot <- apply_palette(raw_plot, custom_palette)
    delta_plot <- apply_palette(delta_plot, custom_palette)

    if (isTRUE(show_legend)) {
      raw_legend <- cowplot::get_legend(raw_plot +
        ggplot2::guides(alpha = "none") +
        ggplot2::theme(legend.box.margin = ggplot2::margin(0, 0, 0, 0)))
    }

    plot_margin <- ggplot2::unit(c(0, 0, 0, 0), "pt")

    if (isTRUE(is_mini_meta)) {
      plot_margin <- ggplot2::unit(c(0, 5.5, 0, 0), "pt")
    }

    final_plot <- cowplot::plot_grid(
      plotlist = list(
        raw_plot + ggplot2::theme(
          legend.position = "none",
          plot.margin = plot_margin
        ),
        delta_plot + ggplot2::theme(
          legend.position = "none",
          plot.margin = plot_margin
        )
      ),
      nrow = 2,
      ncol = 1,
      axis = "tblr",
      align = "vh"
    )

    if (isTRUE(show_legend)){
      if (isTRUE(is_colour)) {
        legend_plot <- cowplot::plot_grid(
          plotlist = list(raw_legend, NULL),
          nrow = 2,
          ncol = 1,
          rel_heights = c(0.1, 0.9)
        )
        final_plot <- cowplot::plot_grid(final_plot, legend_plot, ncol = 2, nrow = 1, rel_widths = c(0.9, 0.1))
      }
    }

    return(final_plot)
  } else {
    # isTRUE(float_contrast)
    raw_plot <- plot_raw(dabest_effectsize_obj, float_contrast = TRUE, plot_kwargs)
    delta_plot <- plot_delta(dabest_effectsize_obj, float_contrast = TRUE, plot_kwargs)

    delta_plot_range <- delta_plot$delta_range
    delta_plot <- delta_plot$delta_plot

    raw_plot <- apply_palette(raw_plot, custom_palette)
    delta_plot <- apply_palette(delta_plot, custom_palette)

    final_plot <- cowplot::plot_grid(
      plotlist = list(
        raw_plot + ggplot2::theme(legend.position = "none"),
        delta_plot + ggplot2::theme(legend.position = "none")
      ),
      nrow = 1,
      ncol = 2,
      rel_widths = c(0.75, 0.25),
      axis = "lr",
      align = "h"
    )

    if (isTRUE(show_legend)) {
      if (isTRUE(is_colour)) {
        raw_legend <- cowplot::get_legend(raw_plot +
          ggplot2::guides(
            color = ggplot2::guide_legend(nrow = 1),
            alpha = "none"
          ) +
          ggplot2::theme(legend.position = "bottom"))
  
        final_plot <- cowplot::plot_grid(final_plot, raw_legend, ncol = 1, rel_heights = c(0.9, 0.1))
      }
    }
    return(final_plot)
  }
}
