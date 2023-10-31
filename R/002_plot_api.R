#' Generates a ggplot object containing plot components for the rawplot component
#' of an estimation plot.
#'
#' This function takes in a dabest_effectsize_obj object and applies the [create_rawplot_components()]
#' function on the object. Plot components for the rawplot are then produced and returned in the
#' form of a ggplot object.
#'
#' @param dabest_effectsize_obj A dabest_effectsize_obj created by loading in a
#' dabest_obj along with other specified parameters with the [effect_size()] function.
#' @param float_contrast Boolean. If TRUE, a Gardner-Altman plot will be produced.
#' If FALSE, a Cumming estimation plot will be produced.
#' @param plot_kwargs Adjustment parameters to control and adjust the appearance of the plot.
#' (list of all possible adjustment parameters can be found under [plot_kwargs])
#'
#' @return ggplot object containing plot components for the rawplot.
#' @noRd
plot_raw <- function(dabest_effectsize_obj, float_contrast, plot_kwargs) {
  enquo_x <- dabest_effectsize_obj$enquo_x
  enquo_y <- dabest_effectsize_obj$enquo_y
  enquo_id_col <- dabest_effectsize_obj$enquo_id_col
  enquo_colour <- dabest_effectsize_obj$enquo_colour
  proportional <- dabest_effectsize_obj$proportional

  proportional_data <- dabest_effectsize_obj$proportional_data
  proportion_success <- proportional_data$proportion_success

  raw_y_labels <- plot_kwargs$swarm_label
  minimeta <- plot_kwargs$show_mini_meta
  delta2 <- plot_kwargs$show_delta2

  raw_data <- dabest_effectsize_obj$raw_data
  Ns <- dabest_effectsize_obj$Ns
  raw_y_range_vector <- dabest_effectsize_obj$ylim

  test_summary <- dabest_effectsize_obj$test_summary
  control_summary <- dabest_effectsize_obj$control_summary
  is_paired <- dabest_effectsize_obj$is_paired
  is_colour <- dabest_effectsize_obj$is_colour

  paired <- dabest_effectsize_obj$paired

  idx <- dabest_effectsize_obj$idx
  separated_idx <- idx
  raw_x_max <- length(unlist(idx))
  x_axis_raw <- c(seq(1, raw_x_max, 1))

  # Extend x_axis if minimeta/deltadelta is being plotted.
  if (isTRUE(minimeta) || isTRUE(delta2)) {
    raw_x_max <- raw_x_max + 2
  }

  effsize_type <- dabest_effectsize_obj$delta_y_labels

  # Check if multiplot.
  if (length(unlist(idx)) >= 3) {
    float_contrast <- FALSE
  }

  #### Load in sizes of plot elements ####
  raw_marker_size <- plot_kwargs$raw_marker_size
  raw_marker_alpha <- plot_kwargs$raw_marker_alpha
  raw_marker_spread <- plot_kwargs$raw_marker_spread
  raw_marker_side_shift <- plot_kwargs$raw_marker_side_shift
  raw_bar_width <- plot_kwargs$raw_bar_width
  tufte_size <- plot_kwargs$tufte_size
  es_marker_size <- plot_kwargs$es_marker_size
  es_line_size <- plot_kwargs$es_line_size
  sankey <- plot_kwargs$sankey
  flow <- plot_kwargs$flow
  raw_flow_alpha <- plot_kwargs$raw_flow_alpha
  swarm_x_text <- plot_kwargs$swarm_x_text
  swarm_y_text <- plot_kwargs$swarm_y_text
  asymmetric_side <- plot_kwargs$asymmetric_side
  asymmetric_side <- ifelse(asymmetric_side == "right", -1, 1)

  #### Rawplot Building ####
  plot_components <- create_rawplot_components(proportional, is_paired, float_contrast)
  main_plot_type <- plot_components$main_plot_type
  is_summary_lines <- plot_components$is_summary_lines
  is_tufte_lines <- plot_components$is_tufte_lines

  ## Creation of dfs for specific main_plot_types ##
  if (main_plot_type == "sankey") {
    if (isFALSE(flow)) {
      separated_idx <- separate_idx(idx, paired)
      raw_x_max <- length(unlist(separated_idx))
      x_axis_raw <- c(seq(2, raw_x_max, 2)) - 0.5
      is_tufte_lines <- TRUE
    }
    sankey_bar_gap <- 0.025
    sankey_df <- create_dfs_for_sankey(
      float_contrast = float_contrast,
      raw_data = raw_data,
      proportional_data = proportional_data,
      enquo_x = enquo_x,
      enquo_y = enquo_y,
      enquo_id_col = enquo_id_col,
      gap = sankey_bar_gap,
      sankey = sankey,
      idx = separated_idx,
      flow = flow,
      N = Ns$n[1]
    )
    flow_success_to_failure <- sankey_df$flow_success_to_failure
    flow_failure_to_success <- sankey_df$flow_failure_to_success
    flow_success_to_success <- sankey_df$flow_success_to_success
    flow_failure_to_failure <- sankey_df$flow_failure_to_failure
    sankey_bars <- sankey_df$sankey_bars
    sankey_bars <- create_dfs_for_proportion_bar(sankey_bars$proportion_success,
      bar_width = raw_bar_width,
      gap = sankey_bar_gap
    )
  }

  if (main_plot_type == "unpaired proportions") {
    if (isTRUE(float_contrast)) {
      raw_y_max <- 1
      raw_y_min <- 0
    }
    df_for_proportion_bar <- create_dfs_for_proportion_bar(proportion_success, bar_width = raw_bar_width)
  }

  ## Adjustment of labels ##
  if (ggplot2::as_label(enquo_colour) == "NULL" && main_plot_type != "slope") {
    enquo_colour <- enquo_x
  }

  #### Initialise raw_plot & Add main_plot_type component ####
  raw_plot <- switch(main_plot_type,
    "swarmplot" =
      ggplot2::ggplot() +
        ggbeeswarm::geom_beeswarm(
          data = raw_data,
          ggplot2::aes(
            x = x_axis_raw + asymmetric_side * raw_marker_side_shift,
            y = !!enquo_y,
            colour = !!enquo_colour
          ),
          cex = raw_marker_spread,
          method = "swarm",
          side = -asymmetric_side * 1L,
          size = raw_marker_size,
          alpha = raw_marker_alpha,
          corral = "wrap",
          corral.width = 0.35 + raw_marker_spread
        ),
    "slope" =
      plot_slopegraph(dabest_effectsize_obj, plot_kwargs),
    "unpaired proportions" =
      ggplot2::ggplot() +
        # failure bar
        geom_proportionbar(
          data = df_for_proportion_bar,
          ggplot2::aes(x = x_failure, y = y_failure, colour = tag)
        ) +
        # success bar
        geom_proportionbar(
          data = df_for_proportion_bar,
          ggplot2::aes(x = x_success, y = y_success, colour = tag, fill = tag)
        ),
    "sankey" =
      ggplot2::ggplot() +
        geom_sankeyflow(
          data = flow_success_to_failure, na.rm = TRUE,
          ggplot2::aes(x = x, y = y, group = tag, colour = NULL),
          fill = "#db6159", alpha = raw_flow_alpha
        ) +
        geom_sankeyflow(
          data = flow_failure_to_success, na.rm = TRUE,
          ggplot2::aes(x = x, y = y, group = tag, colour = NULL),
          fill = "#818181", alpha = raw_flow_alpha
        ) +
        geom_sankeyflow(
          data = flow_success_to_success, na.rm = TRUE,
          ggplot2::aes(x = x, y = y, group = tag, colour = NULL),
          fill = "#db6159", alpha = raw_flow_alpha
        ) +
        geom_sankeyflow(
          data = flow_failure_to_failure, na.rm = TRUE,
          ggplot2::aes(x = x, y = y, group = tag, colour = NULL),
          fill = "#818181", alpha = raw_flow_alpha
        ) +
        geom_proportionbar(
          data = sankey_bars,
          ggplot2::aes(x = x_failure, y = y_failure, group = tag, colour = NULL),
          fill = "#818181", alpha = raw_marker_alpha
        ) +
        geom_proportionbar(
          data = sankey_bars,
          ggplot2::aes(x = x_success, y = y_success, group = tag, colour = NULL),
          fill = "#db6159", alpha = raw_marker_alpha
        )
  )

  #### Add scaling Component ####
  raw_x_labels <- Ns$swarmticklabs
  if (main_plot_type == "sankey" && isFALSE(flow)) {
    raw_x_labels <- create_xlabs_for_sankey(idx, Ns, enquo_x)
  }
  raw_ylim <- plot_kwargs$swarm_ylim
  raw_ylim <- if (is.null(raw_ylim)) {
    raw_y_range_vector
  } else {
    raw_ylim
  }

  raw_y_max <- raw_ylim[2]
  raw_y_min <- raw_ylim[1]
  if (isFALSE(float_contrast) && isFALSE(proportional)) {
    raw_y_min <- raw_y_min - (raw_y_max - raw_y_min) / 15
  }
  raw_y_mean <- raw_y_max - raw_y_min

  raw_x_min <- ifelse(float_contrast, 0.6, 0.6)
  raw_x_scalar <- ifelse(float_contrast, 0.5, 0.3)

  raw_plot <- raw_plot +
    ggplot2::theme_classic() +
    ggplot2::coord_cartesian(
      ylim = c(raw_y_min, raw_y_max),
      xlim = c(raw_x_min, raw_x_max + raw_x_scalar),
      expand = FALSE,
      clip = "off"
    ) +
    ggplot2::scale_x_continuous(
      breaks = c(x_axis_raw),
      labels = raw_x_labels
    )

  #### Add summary_lines component ####
  if (isTRUE(is_summary_lines)) {
    raw_plot <- raw_plot +
      ggplot2::geom_segment(
        colour = "black", linewidth = 0.3,
        ggplot2::aes(
          x = 1,
          xend = raw_x_max + raw_x_scalar,
          y = control_summary,
          yend = control_summary
        )
      ) +
      ggplot2::geom_segment(
        colour = "black", linewidth = 0.3,
        ggplot2::aes(
          x = 2,
          xend = raw_x_max + raw_x_scalar,
          y = test_summary,
          yend = test_summary
        )
      )
  }

  #### Add tufte lines component ####
  if (isTRUE(is_tufte_lines)) {
    if (main_plot_type == "sankey") {
      tufte_gap_value <- sankey_bar_gap
      tufte_lines_df <- create_df_for_tufte(raw_data, enquo_x, enquo_y, proportional, tufte_gap_value, effsize_type)
      if (isFALSE(flow)) {
        tufte_lines_df <- create_dfs_for_nonflow_tufte_lines(tufte_lines_df,
          idx = separated_idx,
          enquo_x = enquo_x
        )
      }
    } else {
      # if (main_plot_type != "sankey")
      tufte_lines_df <- create_df_for_tufte(raw_data, enquo_x, enquo_y, proportional, 0, effsize_type)
      tufte_gap_value <- ifelse(proportional, min(tufte_lines_df$mean) / 20, min(tufte_lines_df$mean) / 50)
      tufte_lines_df <- create_df_for_tufte(raw_data, enquo_x, enquo_y, proportional, tufte_gap_value, effsize_type)
    }

    ## Adjusting side shifting of tufte lines
    tufte_side_adjust_value <- ifelse(proportional, 0, 0.05)
    row_num <- max(x_axis_raw)
    row_ref <- c(seq(1, row_num, 1)) + asymmetric_side * tufte_side_adjust_value + asymmetric_side * raw_marker_side_shift
    if (isFALSE(flow)) {
      row_ref <- c(seq(1, raw_x_max, 1)) + asymmetric_side * tufte_side_adjust_value + asymmetric_side * raw_marker_side_shift
    }

    # to change: temporary fix for tufte lines black for proportional graphs
    if (isTRUE(proportional) | isTRUE(is_colour)) {
      raw_plot <- raw_plot +
        ggplot2::geom_segment(
          data = tufte_lines_df,
          na.rm = TRUE,
          linewidth = tufte_size,
          colour = "black",
          ggplot2::aes(
            x = row_ref,
            xend = row_ref,
            y = y_top_start,
            yend = y_top_end,
            colour = !!enquo_x
          ),
          lineend = "square"
        ) +
        ggplot2::geom_segment(
          data = tufte_lines_df,
          na.rm = TRUE,
          linewidth = tufte_size,
          colour = "black",
          ggplot2::aes(
            x = row_ref,
            xend = row_ref,
            y = y_bot_start,
            yend = y_bot_end,
            colour = !!enquo_x
          ),
          lineend = "square"
        )
    } else {
      raw_plot <- raw_plot +
        ggplot2::geom_segment(
          data = tufte_lines_df,
          linewidth = tufte_size,
          ggplot2::aes(
            x = row_ref,
            xend = row_ref,
            y = y_top_start,
            yend = y_top_end,
            colour = !!enquo_x
          ),
          lineend = "square"
        ) +
        ggplot2::geom_segment(
          data = tufte_lines_df,
          linewidth = tufte_size,
          ggplot2::aes(
            x = row_ref,
            xend = row_ref,
            y = y_bot_start,
            yend = y_bot_end,
            colour = !!enquo_x
          ),
          lineend = "square"
        )
    }
  }

  #### Remove x-axis and redraw x_axis component ####
  if (isTRUE(float_contrast)) {
    raw_plot <- raw_plot +
      float_contrast_theme +
      ggplot2::geom_segment(
        linewidth = 0.4,
        color = "black",
        ggplot2::aes(x = raw_x_min, xend = raw_x_max + 0.2, y = raw_y_min, yend = raw_y_min)
      )
  } else {
    # Obtain dfs for xaxis redraw
    if (main_plot_type == "sankey" && isFALSE(flow)) {
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

    raw_plot <- raw_plot +
      non_float_contrast_theme +
      # Redraw xaxis line
      ggplot2::geom_segment(
        data = df_for_line,
        linewidth = 0.5,
        lineend = "square",
        color = "black",
        ggplot2::aes(
          x = x,
          xend = xend,
          y = raw_y_min + raw_y_mean / 40,
          yend = raw_y_min + raw_y_mean / 40
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
          y = raw_y_min + raw_y_mean / 40,
          yend = raw_y_min
        )
      )
  }

  #### Add y_labels component ####
  raw_plot <- raw_plot +
    ggplot2::labs(y = raw_y_labels)

  #### Adjust font sizes ####
  raw_plot <- raw_plot +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = swarm_x_text),
      axis.title.y = ggplot2::element_text(size = swarm_y_text)
    )

  return(raw_plot)
}
#' Generates a ggplot object containing plot components for the deltaplot component
#' of an estimation plot.
#'
#' This function takes in a dabest_effectsize_obj object and applies the [create_deltaplot_components()]
#' function on the object. Plot components for the deltaplot are then produced and returned in the
#' form of a ggplot object.
#'
#' @param dabest_effectsize_obj A dabest_effectsize_obj created by loading in a
#' dabest_obj along with other specified parameters with the [effect_size()] function.
#' @param float_contrast Boolean. If TRUE, a Gardner-Altman plot will be produced.
#' If FALSE, a Cumming estimation plot will be produced.
#' @param plot_kwargs Adjustment parameters to control and adjust the appearance of the plot.
#' (list of all possible adjustment parameters can be found under [plot_kwargs])
#'
#' @return ggplot object containing plot components for the deltaplot.
#' @noRd
plot_delta <- function(dabest_effectsize_obj, float_contrast, plot_kwargs) {
  idx <- dabest_effectsize_obj$idx
  separated_idx <- idx
  bootstraps <- dabest_effectsize_obj$bootstraps
  proportional <- dabest_effectsize_obj$proportional
  paired <- dabest_effectsize_obj$paired

  delta_x_labels <- unlist(dabest_effectsize_obj$delta_x_labels)
  delta_y_labels <- plot_kwargs$contrast_label

  minimeta <- plot_kwargs$show_mini_meta
  delta2 <- plot_kwargs$show_delta2

  is_colour <- dabest_effectsize_obj$is_colour
  is_paired <- dabest_effectsize_obj$is_paired

  raw_y_range_vector <- dabest_effectsize_obj$ylim
  raw_y_max <- raw_y_range_vector[2]
  raw_y_min <- raw_y_range_vector[1]

  control_summary <- dabest_effectsize_obj$control_summary
  test_summary <- dabest_effectsize_obj$test_summary

  # Initialising x & y limits
  delta_x_max <- length(unlist(idx))
  delta_y_min <- .Machine$double.xmax
  delta_y_max <- .Machine$double.xmin

  # Obtain boot
  boot_result <- dabest_effectsize_obj$boot_result
  boots <- boot_result$bootstraps

  # Check if multiplot
  if (length(unlist(idx)) >= 3) {
    float_contrast <- FALSE
  }

  #### Load in sizes of plot elements ####
  raw_marker_size <- plot_kwargs$raw_marker_size
  raw_marker_alpha <- plot_kwargs$raw_marker_alpha
  raw_bar_width <- plot_kwargs$raw_bar_width
  tufte_size <- plot_kwargs$tufte_size
  es_marker_size <- plot_kwargs$es_marker_size
  es_line_size <- plot_kwargs$es_line_size
  flow <- plot_kwargs$flow
  contrast_x_text <- plot_kwargs$contrast_x_text
  contrast_y_text <- plot_kwargs$contrast_y_text
  show_zero_dot <- plot_kwargs$show_zero_dot
  show_baseline_ec <- plot_kwargs$show_baseline_ec

  #### Deltaplot Building ####
  delta_plot_components <- create_deltaplot_components(
    proportional,
    is_paired,
    float_contrast,
    is_colour,
    delta2,
    show_zero_dot,
    flow,
    show_baseline_ec
  )
  main_violin_type <- delta_plot_components$main_violin_type
  is_summary_lines <- delta_plot_components$is_summary_lines
  is_bootci <- delta_plot_components$is_bootci
  is_deltadelta <- delta_plot_components$is_deltadelta
  is_zero_dot <- delta_plot_components$is_zero_dot
  is_baseline_ec <- delta_plot_components$is_baseline_ec

  raw_plot_components <- create_rawplot_components(proportional, is_paired, float_contrast)
  main_plot_type <- raw_plot_components$main_plot_type

  #### initialise delta_plot & Add main_violin_type component ####
  # Extend idx and labels if minimeta or deltadelta
  if (isTRUE(minimeta) || isTRUE(delta2)) {
    separated_idx <- c(separated_idx, list(c("minimeta", "deltadelta")))
    idx <- separated_idx
  }
  if (main_plot_type == "sankey" && isFALSE(flow)) {
    separated_idx <- separate_idx(idx, paired)
    delta_x_max <- length(unlist(separated_idx))
    is_tufte_lines <- FALSE
  }

  violin_plot_components <- create_violinplot_components(
    boots,
    separated_idx,
    float_contrast,
    delta_y_max,
    delta_y_min,
    flow,
    show_zero_dot
  )

  df_for_violin <- violin_plot_components$df_for_violin
  delta_y_min <- violin_plot_components$delta_y_min
  delta_y_max <- violin_plot_components$delta_y_max
  delta_y_mean <- (delta_y_max - delta_y_min) / 2
  x_axis_breaks <- violin_plot_components$x_axis_breaks
  zero_dot_x_breaks <- violin_plot_components$zero_dot_x_breaks

  if (main_plot_type == "sankey" && isFALSE(flow)) {
    x_axis_breaks <- x_axis_breaks - 0.5
  }

  delta_plot <- switch(main_violin_type,
    "multicolour" =
      ggplot2::ggplot() +
        geom_halfviolin(
          na.rm = TRUE,
          data = df_for_violin,
          ggplot2::aes(x = y, y = x, fill = tag)
        ),
    "singlecolour" =
      ggplot2::ggplot() +
        geom_halfviolin(
          na.rm = TRUE,
          data = df_for_violin,
          ggplot2::aes(x = y, y = x, group = tag)
        )
  )

  #### Add scaling Component ####
  raw_ylim <- plot_kwargs$swarm_ylim
  raw_ylim <- if (is.null(raw_ylim)) {
    c(raw_y_min, raw_y_max)
  } else {
    raw_ylim
  }

  ## Add labels ##
  if (isTRUE(minimeta)) {
    delta_x_labels <- append(delta_x_labels, "Weighted\nDelta")
  }
  if (isTRUE(delta2)) {
    delta_x_labels <- append(delta_x_labels, "delta-delta")
  }

  if (isTRUE(float_contrast)) {
    difference <- boot_result$difference

    if (main_plot_type == "unpaired proportions") {
      raw_y_range_vector <- c(0, 1)
    }
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
        xlim = c(1.8, delta_x_max + 0.4),
        expand = FALSE
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
    if (isTRUE(minimeta) || isTRUE(delta2)) {
      delta_x_max <- delta_x_max + 2
    }
    ## Custom contrast_ylim
    delta_ylim <- plot_kwargs$contrast_ylim
    if (isFALSE(is.null(delta_ylim))) {
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
        xlim = c(delta_x_min, delta_x_max + delta_x_scalar),
        expand = FALSE
      ) +
      ggplot2::scale_x_continuous(
        breaks = x_axis_breaks,
        labels = delta_x_labels
      )
  }

  #### Add bootci Component ####
  # if isFALSE(show_delta = FALSE) || isFALSE(show_mini_meta)
  if (delta2 != dabest_effectsize_obj$delta2 || minimeta != dabest_effectsize_obj$minimeta) {
    boot_result <- boot_result[-nrow(boot_result), ]
  }
  ci_low <- boot_result$bca_ci_low
  ci_high <- boot_result$bca_ci_high
  difference <- boot_result$difference

  if (isTRUE(is_bootci)) {
    delta_plot <- delta_plot +
      geom_bootci(ggplot2::aes(
        x = x_axis_breaks,
        ymin = ci_low,
        ymax = ci_high,
        middle = difference,
        dotsize = es_marker_size,
        linesize = es_line_size
      ))
  }

  #### Add zero_dot Component ####
  # removes extra dot if isTRUE(show_delta2) || isTRUE(show_mini_meta)
  if (isTRUE(delta2) || isTRUE(minimeta)) {
    zero_dot_x_breaks <- zero_dot_x_breaks[-length(zero_dot_x_breaks)]
  }

  if (isTRUE(is_zero_dot)) {
    delta_plot <- delta_plot +
      geom_bootci(ggplot2::aes(
        x = zero_dot_x_breaks,
        ymin = 0,
        ymax = 0,
        middle = 0,
        dotsize = es_marker_size,
        linesize = es_line_size
      ))
  }

  #### Add baseline_error_curve Component ####
  if (isTRUE(is_baseline_ec)) {
    # Add violinplot Component
    baseline_ec_boot_result <- dabest_effectsize_obj$baseline_ec_boot_result

    baseline_boots <- baseline_ec_boot_result$bootstraps
    baseline_ci_low <- baseline_ec_boot_result$bca_ci_low
    baseline_ci_high <- baseline_ec_boot_result$bca_ci_high
    baseline_difference <- baseline_ec_boot_result$difference

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
    # Add bootci Component
    delta_plot <- delta_plot +
      geom_bootci(ggplot2::aes(
        x = zero_dot_x_breaks,
        ymin = baseline_ci_low,
        ymax = baseline_ci_high,
        middle = baseline_difference,
        dotsize = es_marker_size,
        linesize = es_line_size
      ))
  }

  #### Add summary lines Component ####
  if (isTRUE(is_summary_lines)) {
    delta_plot <- delta_plot +
      ggplot2::geom_segment(
        colour = "black",
        linewidth = 0.3,
        ggplot2::aes(
          x = 1.8,
          xend = delta_x_max + 0.4,
          y = difference,
          yend = difference
        )
      ) +
      ggplot2::geom_segment(
        colour = "black",
        linewidth = 0.3,
        ggplot2::aes(
          x = 1.8,
          xend = delta_x_max + 0.4,
          y = 0,
          yend = 0
        )
      )
  }

  #### Remove xaxis and redraw xaxis component ####
  if (isTRUE(float_contrast)) {
    delta_plot <- delta_plot +
      float_contrast_theme +
      ggplot2::geom_hline(
        linewidth = 0.8,
        yintercept = min_y_coords
      )
  } else {
    # Obtain xaxis line and ticks elements for xaxis redraw
    if (main_plot_type == "sankey" && isFALSE(flow)) {
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
      dfs_for_xaxis_redraw <- create_dfs_for_xaxis_redraw(idx)
      df_for_line <- dfs_for_xaxis_redraw$df_for_line
      df_for_ticks <- dfs_for_xaxis_redraw$df_for_ticks
    }

    delta_plot <- delta_plot +
      non_float_contrast_theme +

      # Redraw xaxis line
      ggplot2::geom_segment(
        data = df_for_line,
        linewidth = 0.5,
        lineend = "square",
        color = "black",
        ggplot2::aes(
          x = x,
          xend = xend,
          y = delta_y_min - delta_y_mean / 22,
          yend = delta_y_min - delta_y_mean / 22
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
          y = delta_y_min - delta_y_mean / 22,
          yend = delta_y_min - delta_y_mean / 10
        )
      )
  }

  #### Add y = 0 line Component ####
  if (isFALSE(float_contrast)) {
    zero_line_xend <- delta_x_max + 0.3
    if (isTRUE(is_deltadelta)) {
      zero_line_xend <- zero_line_xend + 0.2
    }
    delta_plot <- delta_plot +
      ggplot2::geom_segment(
        colour = "black",
        linewidth = 0.3,
        ggplot2::aes(
          x = 0.6,
          xend = zero_line_xend,
          y = 0,
          yend = 0
        )
      )
  }

  #### Add y_labels Component ####
  delta_plot <- delta_plot +
    ggplot2::labs(y = delta_y_labels)

  #### Add extra_axis Componenet ####
  if (isTRUE(is_deltadelta)) {
    delta_plot <- delta_plot +
      ggplot2::scale_y_continuous(sec.axis = ggplot2::dup_axis(name = "delta-delta"))
  }

  #### Adjust font sizes ####
  delta_plot <- delta_plot +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = contrast_x_text),
      axis.title.y = ggplot2::element_text(size = contrast_y_text)
    )

  return(list(delta_plot = delta_plot, delta_range = c(delta_y_min - delta_y_mean / 10, delta_y_max)))
}
