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
  main_plot_type <- ""
  is_summary_lines <- TRUE
  is_tufte_lines <- TRUE

  if (isTRUE(proportional)) {
    if (isFALSE(is_paired)) {
      main_plot_type <- "unpaired proportions"
      if (isTRUE(float_contrast)) {
        is_summary_lines <- TRUE
      } else {
        is_summary_lines <- FALSE
      }
    } else {
      main_plot_type <- "sankey"
      is_summary_lines <- FALSE
    }
  } else {
    if (isFALSE(is_paired)) {
      main_plot_type <- "swarmplot"
      if (isTRUE(float_contrast)) {
        is_summary_lines <- TRUE
      } else {
        is_summary_lines <- FALSE
      }
    } else {
      main_plot_type <- "slope"
      is_tufte_lines <- FALSE
      if (isTRUE(float_contrast)) {
        is_summary_lines <- TRUE
      } else {
        is_summary_lines <- FALSE
      }
    }
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
  main_violin_type <- "multicolour"
  is_summary_lines <- TRUE
  is_bootci <- TRUE
  is_deltadelta <- FALSE
  is_zero_dot <- FALSE
  is_baseline_ec <- FALSE

  if (isTRUE(is_paired) || isTRUE(is_colour)) {
    main_violin_type <- "singlecolour"
  }
  if (isTRUE(delta2)) {
    is_deltadelta <- TRUE
  }
  if (isTRUE(show_zero_dot) && isTRUE(flow)) {
    is_zero_dot <- TRUE
  }
  if (isFALSE(float_contrast)) {
    is_summary_lines <- FALSE
  }
  if (isTRUE(show_baseline_ec)) {
    is_baseline_ec <- TRUE
  }

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
  df_for_violin <- data.frame(
    x = NA,
    y = NA,
    tag = NA
  )

  x_axis_breaks <- c()
  zero_dot_x_breaks <- c()
  curr_boot_idx <- 1
  curr_x_idx <- 0
  x_axis_scalar <- ifelse(flow, 0, 0.5)

  for (group in idx) {
    curr_x_idx <- curr_x_idx + 1
    if (isTRUE(zero_dot)) {
      zero_dot_x_breaks <- append(zero_dot_x_breaks, curr_x_idx)
    }
    temp_df_violin <- data.frame(
      x = NA,
      y = NA,
      tag = toString(curr_x_idx)
    )

    df_for_violin <- rbind(df_for_violin, temp_df_violin)

    for (i in 2:length(group)) {
      curr_x_idx <- curr_x_idx + 1
      x_axis_breaks <- append(x_axis_breaks, curr_x_idx)

      ci_coords <- stats::density(boots[[curr_boot_idx]])

      x_coords_ci <- ci_coords$x
      y_coords_ci <- ci_coords$y

      # Standardise y
      y_coords_ci <- (y_coords_ci - min(y_coords_ci)) / (max(y_coords_ci) - min(y_coords_ci))
      y_coords_ci <- y_coords_ci / 6

      if (isFALSE(float_contrast)) {
        y_coords_ci <- y_coords_ci / 1.5
      }

      y_coords_ci <- y_coords_ci + curr_x_idx - x_axis_scalar

      min_x_coords <- min(x_coords_ci)
      max_x_coords <- max(x_coords_ci)

      # Keeping track of ylim limits
      if (min_x_coords < delta_y_min) {
        delta_y_min <- min_x_coords
      }
      if (max_x_coords > delta_y_max) {
        delta_y_max <- max_x_coords
      }

      temp_df_violin <- data.frame(
        x = x_coords_ci,
        y = y_coords_ci,
        tag = rep(toString(curr_x_idx), 512)
      )

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
