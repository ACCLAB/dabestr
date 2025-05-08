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
  is_zero_dot <- show_zero_dot && flow && !float_contrast
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

# TODO add documentation
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

# TODO add documentation
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
add_scaling_component_to_delta_plot <- function(delta_plot, float_contrast, boot_result, delta_x_axis_params, delta_y_axis_params, summary_data, plot_kwargs) {
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
