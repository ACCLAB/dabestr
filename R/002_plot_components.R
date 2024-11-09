# Helper functions that generates plot components.
#
# Contains functions `create_rawplot_components`, `create_deltaplot_components` and `create_violinplot_components`.

#' Generates list of TRUE/FALSE for raw plot components that will be built
#'
#' This function generates a list of booleans determining whether certain
#' plot components will be constructed for the rawplot.
#'
#' @param proportional Boolean value as initially passed to [load()].
#' @param is_paired Boolean value determining if it is a paired plot.
#' @param float_contrast Boolean value determining which plot will be produced. If TRUE, a
#' Gardner-Altman plot will be produced.If FALSE, a Cumming estimation plot will be produced.
#'
#' @return List of booleans for raw plot components
#'
#' @noRd
create_rawplot_components <- function(proportional,
                                      is_paired,
                                      float_contrast) {
  main_plot_type <- switch(paste(proportional, is_paired),
    "TRUE FALSE" = "unpaired proportions",
    "FALSE FALSE" = "swarmplot",
    "TRUE TRUE" = "sankey",
    "FALSE TRUE" = "slope"
  )

  is_summary_lines <- is_tufte_lines <- TRUE
  if (!float_contrast) {
    is_summary_lines <- FALSE
  }
  if (main_plot_type == "slope") {
    is_tufte_lines <- FALSE
  }
  if (main_plot_type == "sankey") {
    is_summary_lines <- FALSE
  }

  plot_component <- list(
    main_plot_type = main_plot_type,
    is_summary_lines = is_summary_lines,
    is_tufte_lines = is_tufte_lines
  )

  return(plot_component)
}


#' Generates list of TRUE/FALSE for delta plot components that will be built
#'
#' This function generates a list of booleans determining whether certain
#' plot components will be constructed for the deltaplot.
#'
#' @param proportional Boolean value as initially passed to [load()].
#' @param is_paired Boolean value determining if it is a paired plot.
#' @param float_contrast Boolean value determining which plot will be produced. If TRUE, a
#' Gardner-Altman plot will be produced.If FALSE, a Cumming estimation plot will be produced.
#' @param is_colour Boolean value determining if there is a colour column for the plot.
#' @param delta2 Boolean value determining if delta-delta analysis for
#' 2 by 2 experimental designs is conducted.
#' @param show_zero_dot Boolean value determining if there is a dot on
#' the zero line of the effect size for the control-control group.
#' @param flow Boolean value determining whether the bars will be plotted in pairs.
#' @param show_baseline_ec Boolean value determining whether the baseline curve is shown.
#'
#' @return List of booleans for delta plot components
#' @noRd
create_deltaplot_components <- function(proportional,
                                        is_paired,
                                        float_contrast,
                                        is_colour,
                                        delta2,
                                        show_zero_dot,
                                        flow,
                                        show_baseline_ec) {
  main_violin_type <- if (is_paired || is_colour) "singlecolour" else "multicolour"
  is_summary_lines <- float_contrast
  is_bootci <- TRUE
  is_deltadelta <- delta2
  is_zero_dot <- show_zero_dot && flow
  is_baseline_ec <- show_baseline_ec

  plot_component <- list(
    main_violin_type = main_violin_type,
    is_summary_lines = is_summary_lines,
    is_bootci = is_bootci,
    is_deltadelta = is_deltadelta,
    is_zero_dot = is_zero_dot,
    is_baseline_ec = is_baseline_ec
  )
  return(plot_component)
}

#' Generates list of values for the violin plot components that will be built
#'
#' This function generates the data and metadata necessary to create a
#' violin plot with specific characteristics
#'
#' @param boots Boot result obtained from boot.ci
#' @param idx List of vectors of control-test groupings that determines the arrangement
#' of the final dataframe output.
#' @param float_contrast Boolean value determining if a Gardner-Altman plot or
#' Cumming estimation plot will be produced.
#' @param delta_y_max Max y limits for the delta-delta plot
#' @param delta_y_min Min y limits for the delta-delta plot
#' @param flow Boolean value determining whether the bars will be plotted in pairs.
#' @param zero_dot Boolean value determining if the zero dot will be constructed.
#'
#' @return List of components essential for the violinplot.
#' @noRd
create_violinplot_components <- function(boots,
                                         idx,
                                         float_contrast,
                                         delta_y_max,
                                         delta_y_min,
                                         flow = TRUE,
                                         zero_dot = TRUE) {
  df_for_violin <- data.frame(x = NA, y = NA, tag = NA)
  x_axis_breaks <- c()
  zero_dot_x_breaks <- c()
  curr_boot_idx <- 1
  curr_x_idx <- 0
  x_axis_scalar <- if (flow) 0 else 0.5

  for (group in idx) {
    curr_x_idx <- curr_x_idx + 1
    if (zero_dot) {
      zero_dot_x_breaks <- c(zero_dot_x_breaks, curr_x_idx)
    }
    temp_df_violin <- data.frame(x = NA, y = NA, tag = toString(curr_x_idx))
    df_for_violin <- rbind(df_for_violin, temp_df_violin)

    for (i in 2:length(group)) {
      curr_x_idx <- curr_x_idx + 1
      x_axis_breaks <- c(x_axis_breaks, curr_x_idx)

      ci_coords <- stats::density(boots[[curr_boot_idx]])
      x_coords_ci <- ci_coords$x
      y_coords_ci <- ci_coords$y

      # Standardise y
      y_coords_ci <- (y_coords_ci - min(y_coords_ci)) / (max(y_coords_ci) - min(y_coords_ci))
      y_coords_ci <- y_coords_ci / 6

      if (!float_contrast) {
        y_coords_ci <- y_coords_ci / 1.5
      }

      y_coords_ci <- y_coords_ci + curr_x_idx - x_axis_scalar

      min_x_coords <- min(x_coords_ci)
      max_x_coords <- max(x_coords_ci)

      # Keeping track of ylim limits
      delta_y_min <- min(min_x_coords, delta_y_min)
      delta_y_max <- max(max_x_coords, delta_y_max)

      temp_df_violin <- data.frame(x = x_coords_ci, y = y_coords_ci, tag = rep(toString(curr_x_idx), 512))
      df_for_violin <- rbind(df_for_violin, temp_df_violin)
      curr_boot_idx <- curr_boot_idx + 1
    }
  }

  df_for_violin <- df_for_violin %>%
    dplyr::arrange(tag, x, y)

  plot_component <- list(
    df_for_violin = df_for_violin,
    delta_y_min = delta_y_min,
    delta_y_max = delta_y_max,
    x_axis_breaks = x_axis_breaks,
    zero_dot_x_breaks = zero_dot_x_breaks
  )

  return(plot_component)
}

#' Adds violin plot component to the delta plot
#'
#' This function adds a violin plot component to an existing delta plot,
#' incorporating effect size data and customization options.
#'
#' @param delta_plot A delta plot object, typically created using ggplot2.
#' @param dabest_effectsize_obj A list containing effect size data and bootstrap results.
#' @param main_violin_type Character string specifying the type of violin plot. Can be "multicolour" or other types.
#' @param flow Boolean value determining the arrangement of violins.
#' @param float_contrast Boolean value for additional customization.
#' @param zero_dot_x_breaks X-axis breaks for the zero dot.
#'
#' @return A delta plot with the violin plot component added.
#' @noRd
add_violinplot_component_to_delta_plot <- function(delta_plot, dabest_effectsize_obj, main_violin_type, flow, float_contrast, zero_dot_x_breaks) {
  baseline_ec_boot_result <- dabest_effectsize_obj$baseline_ec_boot_result
  baseline_boots <- baseline_ec_boot_result$bootstraps

  df_for_baseline_ec_violin <- create_dfs_for_baseline_ec_violin(
    baseline_boots,
    zero_dot_x_breaks,
    float_contrast,
    flow
  )
  if (main_violin_type == "multicolour") {
    delta_plot <- delta_plot +
      geom_halfviolin(
        na.rm = TRUE,
        data = df_for_baseline_ec_violin,
        ggplot2::aes(x = y, y = x, fill = tag)
      )
  } else {
    delta_plot <- delta_plot +
      geom_halfviolin(
        na.rm = TRUE,
        data = df_for_baseline_ec_violin,
        ggplot2::aes(x = y, y = x, group = tag)
      )
  }
  return(delta_plot)
}

#' Adds bootstrap confidence intervals to the delta plot
#'
#' This function enhances a delta plot by adding bootstrap confidence intervals,
#' providing a visual representation of uncertainty in the data.
#'
#' @param delta_plot A delta plot object, typically created using ggplot2.
#' @param x_axis_breaks X-axis breaks for the confidence intervals.
#' @param ci_low Lower bounds of the confidence intervals.
#' @param ci_high Upper bounds of the confidence intervals.
#' @param difference The middle value of the confidence intervals.
#' @param es_marker_size Size of the effect size marker.
#' @param es_line_size Thickness of the confidence interval lines.
#'
#' @return A delta plot with bootstrap confidence intervals added.
#' @noRd
add_bootci_component_to_delta_plot <- function(delta_plot, x_axis_breaks, ci_low, ci_high, difference, es_marker_size, es_line_size) {
  delta_plot <- delta_plot +
    geom_bootci(ggplot2::aes(
      x = x_axis_breaks,
      ymin = ci_low,
      ymax = ci_high,
      middle = difference,
      dotsize = es_marker_size,
      linesize = es_line_size
    ))
  return(delta_plot)
}

# TODO add documentation
add_scaling_component_to_delta_plot <- function(delta_plot, float_contrast,
                                                boot_result, delta_x_axis_params,
                                                delta_y_axis_params, summary_data, plot_kwargs) {
  minimeta <- plot_kwargs$show_mini_meta
  delta2 <- plot_kwargs$show_delta2

  # summary control and test
  control_summary <- summary_data[[1]]
  test_summary <- summary_data[[2]]

  # axis params
  delta_x_max <- delta_x_axis_params[[1]]
  delta_x_labels <- delta_x_axis_params[[2]]
  x_axis_breaks <- delta_x_axis_params[[3]]

  delta_y_min <- delta_y_axis_params[[1]]
  delta_y_max <- delta_y_axis_params[[2]]
  delta_y_mean <- delta_y_axis_params[[3]]
  raw_ylim <- delta_y_axis_params[[4]]

  delta_text_space <- 0
  if (!(float_contrast) && (plot_kwargs$delta_text) && (plot_kwargs$params_delta_text$x_location == "right")) {
    delta_text_space <- 0.4
  }

  min_y_coords <- NULL # only valid for float_contrast

  if (float_contrast) {
    difference <- boot_result$difference

    # Calculate new ylims to align summary lines
    min_raw_y <- raw_ylim[1]
    max_raw_y <- raw_ylim[2]
    raw_y_range <- max_raw_y - min_raw_y
    min_y_coords <- difference / (1 - (test_summary - min_raw_y) / (control_summary - min_raw_y))
    delta_y_range <- raw_y_range * -min_y_coords / (control_summary - min_raw_y)

    delta_plot <- delta_plot +
      ggplot2::theme_classic() +
      ggplot2::coord_cartesian(
        ylim = c(min_y_coords, min_y_coords + delta_y_range),
        xlim = c(1.8, delta_x_max + 0.4 + delta_text_space),
        expand = FALSE,
        clip = "off"
      ) +
      ggplot2::scale_x_continuous(
        breaks = c(2),
        labels = delta_x_labels
      ) +
      ggplot2::scale_y_continuous(position = "right")
  } else {
    delta_x_min <- 0.6
    delta_x_scalar <- 0.3

    # Extend xaxis for minimeta/deltadelta.
    if (minimeta || delta2) {
      delta_x_max <- delta_x_max + 2
    }
    ## Custom contrast_ylim
    delta_ylim <- plot_kwargs$contrast_ylim
    if (!(is.null(delta_ylim))) {
      delta_y_min <- delta_ylim[1]
      delta_y_max <- delta_ylim[2]
      delta_y_mean <- (delta_y_max - delta_y_min) / 2
    }

    delta_plot <- delta_plot +
      ggplot2::theme_classic() +
      ggplot2::coord_cartesian(
        ylim = c(
          delta_y_min - delta_y_mean / 10,
          delta_y_max
        ),
        xlim = c(delta_x_min, delta_x_max + delta_x_scalar + delta_text_space),
        expand = FALSE,
        clip = "off"
      ) +
      ggplot2::scale_x_continuous(
        breaks = x_axis_breaks,
        labels = delta_x_labels
      )
  }

  delta_y_params <- list(min_y_coords, delta_y_min, delta_y_max, delta_y_mean)
  return(list(delta_plot, delta_x_max, delta_y_params))
}

#' Adds x-axis component to the raw plot
#'
#' This function enhances a raw plot by adding the x-axis component,
#' including lines and ticks, with adjustments for different plot types.
#'
#' @param raw_plot A raw plot object, typically created using ggplot2.
#' @param main_plot_type Character string specifying the main plot type (e.g., "sankey").
#' @param flow Boolean value for plot arrangement.
#' @param horizontal Boolean value. If TRUE the layout of the plot is horizontal.
#' @param idx Index or grouping information for data.
#' @param raw_y_min Minimum y-value for the raw plot.
#' @param raw_y_range Range of y-values for the raw plot.
#'
#' @return A raw plot with the x-axis component added.
#' @noRd
add_x_axis_component_to_rawplot <- function(raw_plot, main_plot_type, flow, horizontal, idx, raw_y_min, raw_y_range) {
  # computing df for axis redraw
  if (main_plot_type == "sankey" && !(flow)) {
    idx_for_xaxis_redraw <- remove_last_ele_from_nested_list(idx)
    dfs_for_xaxis_redraw <- create_dfs_for_xaxis_redraw(idx_for_xaxis_redraw)
    df_for_line <- dfs_for_xaxis_redraw$df_for_line
    df_for_ticks <- dfs_for_xaxis_redraw$df_for_ticks

    df_for_line <- df_for_line %>%
      dplyr::mutate(
        x = x + 0.5 + (x - 1),
        xend = xend + 0.5 + (xend - 1)
      )

    df_for_ticks <- df_for_ticks %>%
      dplyr::mutate(x = x + 0.5 + (x - 1))
  } else {
    idx_for_xaxis_redraw <- idx
    dfs_for_xaxis_redraw <- create_dfs_for_xaxis_redraw(idx_for_xaxis_redraw)
    df_for_line <- dfs_for_xaxis_redraw$df_for_line
    df_for_ticks <- dfs_for_xaxis_redraw$df_for_ticks
  }

  y_line <- raw_y_min + raw_y_range / 40
  yend_line <- raw_y_min + raw_y_range / 40
  y_ticks <- raw_y_min + raw_y_range / 40
  yend_ticks <- raw_y_min
  if (horizontal) {
    y_line <- raw_y_min
    yend_line <- raw_y_min
    y_ticks <- raw_y_min
    yend_ticks <- raw_y_min - raw_y_range / 40
  }
  if (horizontal) {
    raw_plot <- raw_plot +
      horizontal_theme
  } else {
    raw_plot <- raw_plot +
      non_float_contrast_theme
  }
  raw_plot <- raw_plot +
    # Redraw xaxis line
    ggplot2::geom_segment(
      data = df_for_line,
      linewidth = 0.5,
      lineend = "square",
      color = "black",
      ggplot2::aes(
        x = x,
        xend = xend,
        y = y_line,
        yend = yend_line
      )
    ) +
    # Redraw xaxis ticks
    ggplot2::geom_segment(
      data = df_for_ticks,
      linewidth = 0.5,
      lineend = "square",
      color = "black",
      ggplot2::aes(
        x = x,
        xend = x,
        y = y_ticks,
        yend = yend_ticks
      )
    )
  return(raw_plot)
}
