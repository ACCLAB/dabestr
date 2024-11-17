# TODO Add documentation
create_sankey_bars <- function(prop, enquo_x, enquo_y, idx) {
  sankey_bars <- tibble::tibble()

  for (group in idx) {
    group_length <- length(group)

    for (i in 1:(group_length - 1)) {
      ctrl <- group[i]
      treat <- group[i + 1]
      temp_row_ctrl <- prop %>%
        dplyr::group_by(!!enquo_x) %>%
        dplyr::filter(!!enquo_x == ctrl)

      temp_row_treat <- prop %>%
        dplyr::group_by(!!enquo_x) %>%
        dplyr::filter(!!enquo_x == treat)
      pair_rows <- rbind(temp_row_ctrl, temp_row_treat)
      sankey_bars <- dplyr::bind_rows(sankey_bars, pair_rows)
    }
  }

  return(sankey_bars)
}

# TODO Add documentation
create_sankey_flows <- function(raw_data, enquo_x, enquo_y, enquo_id_col, idx, N, means_c_t, gap, bar_width, x_padding, scale_factor_sig) {
  flow_success_to_failure <- tibble::tibble()
  flow_success_to_success <- tibble::tibble()
  flow_failure_to_success <- tibble::tibble()
  flow_failure_to_failure <- tibble::tibble()

  x_start <- 1
  ind <- 1

  for (group in idx) {
    group_length <- length(group)

    for (i in 1:(group_length - 1)) {
      # redraw_x_axis <- append(redraw_x_axis, x_start)
      success_success <- raw_data %>%
        dplyr::group_by(!!enquo_id_col) %>%
        dplyr::summarise(
          success_change =
            any(!!enquo_y == 1 & !!enquo_x == group[i]) &
              any(!!enquo_y == 1 &
                !!enquo_x == group[i + 1])
        ) %>%
        dplyr::filter(success_change) %>%
        dplyr::summarise(SS = dplyr::n() / N)

      success_failure <- raw_data %>%
        dplyr::group_by(!!enquo_id_col) %>%
        dplyr::summarise(
          sf_change =
            any(!!enquo_y == 1 & !!enquo_x == group[i]) &
              any(!!enquo_y == 0 &
                !!enquo_x == group[i + 1])
        ) %>%
        dplyr::filter(sf_change) %>%
        dplyr::summarise(SF = dplyr::n() / N)

      failure_failure <- raw_data %>%
        dplyr::group_by(!!enquo_id_col) %>%
        dplyr::summarise(
          failure_change =
            any(!!enquo_y == 0 & !!enquo_x == group[i]) &
              any(!!enquo_y == 0 &
                !!enquo_x == group[i + 1])
        ) %>%
        dplyr::filter(failure_change) %>%
        dplyr::summarise(FF = dplyr::n() / N)

      failure_success <- raw_data %>%
        dplyr::group_by(!!enquo_id_col) %>%
        dplyr::summarise(
          failure_change =
            any(!!enquo_y == 0 & !!enquo_x == group[i]) &
              any(!!enquo_y == 1 &
                !!enquo_x == group[i + 1])
        ) %>%
        dplyr::filter(failure_change) %>%
        dplyr::summarise(FS = dplyr::n() / N)
      # find values for lower flow success to failure flow
      ss <- success_success$SS[1]
      ff <- failure_failure$FF[1]
      sf <- success_failure$SF[1]
      fs <- failure_success$FS[1]
      sf_start1 <- ss
      sf_start2 <- means_c_t[ind] - gap / 2
      sf_end1 <- means_c_t[ind + 1] + gap / 2
      sf_end2 <- 1 - ff


      # find values for upper flppied flow success to failure flow
      fs_start1 <- 1 - ff
      fs_start2 <- means_c_t[ind] + gap / 2
      fs_end1 <- means_c_t[ind + 1] - gap / 2
      fs_end2 <- ss

      # form dataframes from sigmoid / flippedSig functions and the rectangles, later fit into sankeyflow
      sig_success_failure_bot <- sigmoid(
        x_start + bar_width - x_padding,
        scale_factor_sig,
        sf_start1 - 0.002,
        sf_end1 + 0.002
      )
      sig_success_failure_top <- sigmoid(
        x_start + bar_width - x_padding,
        scale_factor_sig,
        sf_start2 - 0.002,
        sf_end2 + 0.002
      )
      sig_success_failure_bot <- dplyr::arrange(sig_success_failure_bot, dplyr::desc(x))
      sig_failure_success_top <- flipped_sig(
        x_start + bar_width - x_padding,
        scale_factor_sig,
        fs_start1 + 0.002,
        fs_end1 - 0.002
      )
      sig_failure_success_bot <- flipped_sig(
        x_start + bar_width - x_padding,
        scale_factor_sig,
        fs_start2 + 0.002,
        fs_end2 - 0.002
      )
      sig_failure_success_bot <- dplyr::arrange(sig_failure_success_bot, dplyr::desc(x))


      # For datasets with purely 1s or 0s
      if (sf == 0) {
        sig_success_failure_top <- data.frame(x = NaN, y = NaN)
        sig_success_failure_bot <- sig_success_failure_top
      }
      if (fs == 0) {
        sig_failure_success_top <- data.frame(x = NaN, y = NaN)
        sig_failure_success_bot <- sig_failure_success_top
      }

      # number of points of data points
      N_points <- length(sig_success_failure_bot)

      # generate the tag column for all of these
      tag <- rep(ind, N_points)
      sankey_success_failure <- rbind(
        sig_success_failure_top,
        sig_success_failure_bot
      )
      sankey_success_failure <- cbind(sankey_success_failure, tag)

      sankey_failure_success <- rbind(
        sig_failure_success_top,
        sig_failure_success_bot
      )
      sankey_failure_success <- cbind(sankey_failure_success, tag)

      rect_flow_x <- c(x_start, x_start + 1)

      sankey_failure_failure <- data.frame(
        x = c(rect_flow_x, rev(rect_flow_x)),
        y = c(1, 1, rep(fs_start1, 2)),
        tag = c(rep(ind, 4))
      )
      sankey_success_success <- data.frame(
        x = c(rect_flow_x, rev(rect_flow_x)),
        y = c(rep(ss, 2), 0, 0),
        tag = c(rep(ind, 4))
      )

      x_start <- x_start + 1

      ind <- ind + 1

      # update the 4 sankey flow dfs for plotting
      flow_success_to_failure <- dplyr::bind_rows(
        flow_success_to_failure,
        sankey_success_failure
      )
      flow_success_to_success <- dplyr::bind_rows(
        flow_success_to_success,
        sankey_success_success
      )
      flow_failure_to_success <- dplyr::bind_rows(
        flow_failure_to_success,
        sankey_failure_success
      )
      flow_failure_to_failure <- dplyr::bind_rows(
        flow_failure_to_failure,
        sankey_failure_failure
      )
    }

    x_start <- x_start + 1
    ind <- ind + 1
  }

  return(list(
    flow_success_to_failure = flow_success_to_failure,
    flow_success_to_success = flow_success_to_success,
    flow_failure_to_success = flow_failure_to_success,
    flow_failure_to_failure = flow_failure_to_failure
  ))
}

#' Initializes the raw plot
#'
#' Sets up the initial raw plot with various components and data.
#'
#' @param plot_kwargs Keyword arguments for plot customization.
#' @param plot_components List of components for the plot.
#' @param dabest_effectsize_obj Object containing effect size data.
#' @param df_for_proportion_bar Dataframe for proportion bars.
#' @param sankey_df Dataframe for Sankey plot.
#' @param sankey_bars Dataframe for Sankey bars.
#' @param idx Index or grouping information.
#' @param float_contrast Boolean value for plot customization.
#'
#' @return Initialized raw plot object.
#' @noRd
initialize_raw_plot <- function(plot_kwargs, plot_components, dabest_effectsize_obj,
                                df_for_proportion_bar, sankey_df, sankey_bars, idx,
                                float_contrast) {
  raw_data <- dabest_effectsize_obj$raw_data
  enquo_x <- dabest_effectsize_obj$enquo_x
  enquo_y <- dabest_effectsize_obj$enquo_y
  enquo_colour <- dabest_effectsize_obj$enquo_colour
  proportional <- dabest_effectsize_obj$proportional
  raw_marker_size <- plot_kwargs$raw_marker_size
  raw_marker_alpha <- plot_kwargs$raw_marker_alpha
  raw_marker_spread <- plot_kwargs$raw_marker_spread
  raw_marker_side_shift <- plot_kwargs$raw_marker_side_shift
  asymmetric_side <- plot_kwargs$asymmetric_side
  asymmetric_side <- ifelse(asymmetric_side == "right", -1, 1)
  asymmetric_x_adjustment <- asymmetric_side
  horizontal <- plot_kwargs$horizontal
  minimeta <- plot_kwargs$show_mini_meta
  delta2 <- plot_kwargs$show_delta2
  raw_x_max <- length(unlist(idx))
  x_axis_raw <- c(seq(1, raw_x_max, 1))
  raw_y_range_vector <- dabest_effectsize_obj$ylim
  Ns <- dabest_effectsize_obj$Ns
  control_summary <- dabest_effectsize_obj$control_summary
  test_summary <- dabest_effectsize_obj$test_summary
  flow <- plot_kwargs$flow
  paired <- dabest_effectsize_obj$paired
  raw_flow_alpha <- plot_kwargs$raw_flow_alpha
  main_plot_type <- plot_components$main_plot_type
  is_summary_lines <- plot_components$is_summary_lines

  # Extend x_axis if minimeta/deltadelta is being plotted.
  if (minimeta || delta2) {
    raw_x_max <- raw_x_max + 2
  }

  ## Adjustment of labels ##
  if (ggplot2::as_label(enquo_colour) == "NULL" && main_plot_type != "slope") {
    enquo_colour <- enquo_x
  }

  # sankey params
  if (!is.null(sankey_df)) {
    flow_success_to_failure <- sankey_df$flow_success_to_failure
    flow_failure_to_success <- sankey_df$flow_failure_to_success
    flow_success_to_success <- sankey_df$flow_success_to_success
    flow_failure_to_failure <- sankey_df$flow_failure_to_failure

    # replicate adjustment on the x_axis_raw as in api
    if (!(flow)) {
      separated_idx <- separate_idx(idx, paired)
      raw_x_max <- length(unlist(separated_idx))
      x_axis_raw <- c(seq(2, raw_x_max, 2)) - 0.5
    }
  }


  raw_plot <- switch(main_plot_type,
    "swarmplot" =
      ggplot2::ggplot() +
        ggbeeswarm::geom_beeswarm(
          data = raw_data,
          ggplot2::aes(
            x = x_axis_raw + asymmetric_x_adjustment * raw_marker_side_shift,
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
  if (horizontal) {
    raw_x_labels <- Ns$horizontal_swarmticklabs
  } else {
    raw_x_labels <- Ns$swarmticklabs
  }

  if (main_plot_type == "sankey" && !(flow)) {
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
  if (!(float_contrast) && !(proportional)) {
    raw_y_min <- raw_y_min - (raw_y_max - raw_y_min) / 15
  }
  raw_y_range <- raw_y_max - raw_y_min

  raw_x_min <- ifelse(float_contrast, 0.6, 0.6)
  raw_x_scalar <- ifelse(float_contrast, 0.5, 0.3)

  delta_text_space <- 0
  if (!(float_contrast) && (plot_kwargs$delta_text) && (plot_kwargs$params_delta_text$x_location == "right")) {
    delta_text_space <- 0.4
  }
  raw_plot <- raw_plot +
    ggplot2::theme_classic() +
    ggplot2::coord_cartesian(
      ylim = c(raw_y_min, raw_y_max),
      xlim = c(raw_x_min, raw_x_max + raw_x_scalar + delta_text_space),
      expand = FALSE,
      clip = "off"
    )

  if (horizontal) {
    raw_plot <- raw_plot +
      ggplot2::scale_x_reverse(
        breaks = c(x_axis_raw),
        labels = raw_x_labels
      )
  } else {
    raw_plot <- raw_plot +
      ggplot2::scale_x_continuous(
        breaks = c(x_axis_raw),
        labels = raw_x_labels
      )
  }
  #### Add summary_lines component ####
  if (is_summary_lines) {
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
  return(list(raw_plot, raw_y_range, raw_y_min, x_axis_raw))
}

#' Adds Swarm Bars to a Raw Data Plot
#'
#' This function takes a `dabest_effectsize_obj` and enhances its raw data plot by adding swarm bars.
#' It utilizes the provided plotting parameters along with specific x and y values to customize the
#' appearance and positioning of the swarm bars. The `y_min` parameter ensures that the swarm bars
#' are appropriately placed within the plot's y-axis limits.
#'
#' @param dabest_effectsize_obj A `dabest_effectsize_obj` created by the [effect_size()] function.
#' @param plot_kwargs A list of parameters used to adjust and control the appearance of the plot.
#'   (Refer to [plot_kwargs] for all possible adjustment parameters.)
#' @param x_values A numeric or character vector specifying the x-axis values where the swarm bars
#'   should be added.
#' @param y_values A numeric vector specifying the y-axis values corresponding to the swarm bars.
#' @param y_min A numeric value indicating the minimum y-axis limit to position the swarm bars appropriately.
#'
#' @return A `ggplot` object with the swarm bars to be added to the raw data plot.
#'
#' @noRd
add_swarm_bars_to_raw_plot <- function(dabest_effectsize_obj, plot_kwargs, x_values, y_values, y_min, main_plot_type) {
  stopifnot(length(x_values) == length(y_values))
  # getting the parameters
  params_swarm_bars <- plot_kwargs$params_swarm_bars
  bars_color <- params_swarm_bars$color
  alpha <- params_swarm_bars$alpha

  is_paired <- dabest_effectsize_obj$is_paired
  is_colour <- dabest_effectsize_obj$is_colour

  custom_colour <- NULL
  if (!is.null(bars_color)) {
    swarm_bars_colours <- rep(bars_color, length(x_values))
    custom_colour <- bars_color
    # this is the same as
  } else if ((main_plot_type == "slope") || is_colour || is_paired) {
    swarm_bars_colours <- rep("black", length(x_values))
    custom_colour <- "black"
  } else {
    swarm_bars_colours <- as.character(x_values)
  }

  # Define width and height for each rectangle
  width <- 0.5

  # Calculate xmin, xmax, ymin, ymax for each rectangle
  rectangles <- data.frame(
    xmin = x_values - (width / 2),
    xmax = x_values + (width / 2),
    ymin = rep(y_min, length(x_values)),
    ymax = y_values, # Heights as provided
    fill_colour = swarm_bars_colours
  )

  # custom colour
  if (!is.null(custom_colour)) {
    return(ggplot2::geom_rect(
      data = rectangles,
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = custom_colour,
      alpha = alpha,
      show.legend = FALSE
    ))
  }
  return(ggplot2::geom_rect(
    data = rectangles,
    ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill_colour),
    alpha = alpha,
    show.legend = FALSE
  ))
}

#' Adds Contrast Bars to a Delta Plot
#'
#' This function takes a `dabest_effectsize_obj` and augments its delta plot by adding contrast bars.
#' It utilizes the provided plotting parameters and specific x and y values to customize the appearance
#' of the contrast bars based on the main violin plot type.
#'
#' @param dabest_effectsize_obj A `dabest_effectsize_obj` created by the [effect_size()] function.
#' @param plot_kwargs A list of parameters used to adjust and control the appearance of the plot.
#'   (Refer to [plot_kwargs] for all possible adjustment parameters.)
#' @param x_values A numeric or character vector specifying the x-axis values where the contrast bars
#'   should be added.
#' @param y_values A numeric vector specifying the y-axis values corresponding to the contrast bars.
#' @param main_violin_type A string indicating the type of violin plot used in the main plot.
#'   Determines the styling and positioning of the contrast bars.
#'
#' @return A `ggplot` object with contrast bars to be added to the delta plot.
#'
#' @noRd
add_contrast_bars_to_delta_plot <- function(dabest_effectsize_obj, plot_kwargs, x_values, y_values, main_violin_type) {
  # Assert that both vectors have the same length
  stopifnot(length(x_values) == length(y_values))

  # getting the parameters
  params_contrast_bars <- plot_kwargs$params_contrast_bars
  bars_color <- params_contrast_bars$color
  alpha <- params_contrast_bars$alpha

  is_paired <- dabest_effectsize_obj$is_paired
  color_col <- plot_kwargs$color_col
  custom_colour <- NULL
  if (!is.null(bars_color)) {
    contrast_bars_colours <- rep(bars_color, length(x_values))
    custom_colour <- bars_color
    # this is the same as
  } else if (!is.null(color_col) || is_paired) {
    contrast_bars_colours <- rep("black", length(x_values))
    custom_colour <- "black"
  } else {
    # use the default palette colours of the ggplot violin plot object
    contrast_bars_colours <- as.character((x_values))
    # contrast_bars_colours <- factor(as.character(x_values), levels = group_levels)
  }

  # Define width and height for each rectangle
  width <- 0.5

  # Calculate xmin, xmax, ymin, ymax for each rectangle
  rectangles <- data.frame(
    xmin = x_values - (width / 2),
    xmax = x_values + (width / 2),
    ymin = rep(0, length(x_values)), # All rectangles start at y = 0
    ymax = y_values, # Heights as provided
    group = contrast_bars_colours
  )

  # custom colour
  if (!is.null(custom_colour)) {
    return(ggplot2::geom_rect(
      data = rectangles,
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = custom_colour,
      alpha = alpha
    ))
  }
  if (main_violin_type == "multicolour") {
    return(ggplot2::geom_rect(
      data = rectangles,
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = group),
      alpha = alpha
    ))
  }

  # Single colour
  return(ggplot2::geom_rect(
    data = rectangles,
    ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, group = group),
    alpha = alpha
  ))
}

#' Adds Delta Text to a Delta Plot
#'
#' This function enhances a delta plot by adding delta text using the provided parameters.
#' It takes a delta plot, a dabest_effectsize_obj, plotting parameters, and specific x and y values.
#' The function also considers the main violin type and float contrast to customize the delta text.
#'
#' @param delta_plot A ggplot object representing the delta plot.
#' @param dabest_effectsize_obj A dabest_effectsize_obj created by the effect_size() function.
#' @param plot_kwargs A list of parameters used to adjust the appearance of the plot.
#' @param x_values A numeric or character vector specifying the x-axis values for delta text positioning.
#' @param y_values A numeric vector specifying the y-axis values for delta text positioning.
#' @param main_violin_type A character string indicating the type of main violin plot ("violin" or "box").
#' @param float_contrast A logical value indicating whether to float the contrast labels.
#'
#' @return A ggplot object with the delta text added to the delta plot.
#'
#' @noRd
add_delta_text_to_delta_plot <- function(delta_plot,
                                         dabest_effectsize_obj,
                                         plot_kwargs,
                                         x_values,
                                         y_values,
                                         main_violin_type,
                                         float_contrast) {
  # Assert that both vectors have the same length
  stopifnot(length(x_values) == length(y_values))

  params_delta_text <- plot_kwargs$params_delta_text
  # getting the parameters
  text_color <- params_delta_text$color
  alpha <- params_delta_text$alpha
  fontsize <- params_delta_text$fontsize
  hjust <- params_delta_text$ha
  vjust <- params_delta_text$va
  rotation <- params_delta_text$rotation
  x_location <- params_delta_text$x_location
  x_adjust <- params_delta_text$x_adjust

  if (float_contrast) {
    x_location <- "left"
    if (y_values[[1]] >= 0) {
      vjust <- "bottom"
    } else {
      vjust <- "top"
    }
  }
  x_coordinates <- x_values
  if (!is.null(params_delta_text$x_coordinates)) {
    x_coordinates <- params_delta_text$x_coordinates
  } else {
    # check x_location and x_adjust
    # width of the contrast_bars is 0.5
    if (x_location == "right") {
      margin <- 0.38
    } else {
      margin <- -0.28
    }
    x_adjust <- x_adjust + margin
  }

  y_coordinates <- y_values
  if (float_contrast) {
    # Adding a bit extra space not to touch the horizontal lines
    y_coordinates <- y_coordinates + 0.01
  }
  if (!is.null(params_delta_text$y_coordinates)) {
    y_coordinates <- params_delta_text$y_coordinates
  }

  is_paired <- dabest_effectsize_obj$is_paired
  color_col <- plot_kwargs$color_col
  custom_colour <- NULL
  if (!is.null(text_color)) {
    delta_text_colours <- rep(text_color, length(x_values))
    custom_colour <- text_color
    # this is the same as
  } else if (!is.null(color_col) || is_paired) {
    delta_text_colours <- rep("black", length(x_values))
    custom_colour <- "black"
  } else {
    # use the default palette colours of the ggplot violin plot object
    colours <- get_palette_colours(plot_kwargs$custom_palette, max(x_values))
    # Select colors at positions specified by x_values
    delta_text_colours <- colours[x_values]
  }
  labels <- sprintf("%+.2f", y_values)

  # Prepare the text for each coordinate
  texts <- data.frame(
    x = x_coordinates + x_adjust,
    y = y_coordinates,
    text = sprintf("%+.2f", y_values),
    group = delta_text_colours
  )

  # custom colour
  if (!is.null(custom_colour)) {
    delta_plot <- delta_plot +
      ggplot2::geom_text(
        data = texts,
        ggplot2::aes(x = x, y = y, label = text),
        colour = custom_colour,
        alpha = alpha,
        check_overlap = TRUE,
        size.unit = "pt",
        size = fontsize,
        vjust = vjust,
        hjust = hjust,
        angle = rotation
      )
  } else if (main_violin_type == "multicolour") {
    for (i in seq_along(x_coordinates)) {
      x_i <- x_coordinates[i] + x_adjust
      y_i <- y_coordinates[i]
      label_i <- labels[i]
      text_colour <- delta_text_colours[i]
      delta_plot <- delta_plot + ggplot2::geom_text(
        data = texts,
        x = x_i,
        y = y_i,
        label = label_i,
        colour = text_colour,
        alpha = alpha,
        check_overlap = TRUE,
        size.unit = "pt",
        size = fontsize,
        vjust = vjust,
        hjust = hjust,
        angle = rotation
      )
    }
  } else {
    delta_plot <- delta_plot +
      ggplot2::geom_text(
        data = texts,
        ggplot2::aes(x = x, y = y, label = text, group = group),
        alpha = alpha,
        check_overlap = TRUE,
        size.unit = "pt",
        size = fontsize,
        vjust = vjust,
        hjust = hjust,
        angle = rotation
      )
  }
  return(delta_plot)
}

create_delta_dots_data <- function(dabest_effectsize_obj, x_axis_breaks) {
  # getting params
  is_paired <- dabest_effectsize_obj$is_paired
  raw_data <- dabest_effectsize_obj$raw_data
  delta_x_labels <- dabest_effectsize_obj$delta_x_labels
  x <- rlang::as_name(dabest_effectsize_obj$enquo_x)
  y <- rlang::as_name(dabest_effectsize_obj$enquo_y)
  color_col <- NULL
  if (dabest_effectsize_obj$is_colour) {
    color_col <- rlang::as_name(dabest_effectsize_obj$enquo_colour)
  }

  delta_dots_data <- lapply(1:length(delta_x_labels), function(i) {
    x_value <- x_axis_breaks[[i]]
    label <- delta_x_labels[[i]]
    values <- strsplit(label, split = "\nminus\n")
    test_label <- values[[1]][1]
    control_label <- values[[1]][2]
    test_samples <- raw_data %>%
      dplyr::filter(!!rlang::sym(x) == test_label) %>%
      dplyr::pull(!!rlang::sym(y))
    control_samples <- raw_data %>%
      dplyr::filter(!!rlang::sym(x) == control_label) %>%
      dplyr::pull(!!rlang::sym(y))
    stopifnot(length(test_samples) == length(control_samples))
    diff_samples <- test_samples - control_samples
    if (!is.null(color_col)) {
      # test and control should have the same colour label
      color_samples <- raw_data %>%
        dplyr::filter(!!rlang::sym(x) == control_label) %>%
        dplyr::pull(!!rlang::sym(color_col))
      data.frame(x_var = rep(x_value, length(diff_samples)), y_var = diff_samples, colour_var = color_samples)
    } else {
      data.frame(x_var = rep(x_value, length(diff_samples)), y_var = diff_samples)
    }
  }) %>%
    dplyr::bind_rows()

  return(delta_dots_data)
}

get_test_labels <- function(delta_x_labels) {
  test_labels <- sapply(delta_x_labels, function(label) {
    values <- strsplit(label, split = "\nminus\n")[[1]]
    values[1]
  })
  return(test_labels)
}

get_matching_labels <- function(plot_raw_labels, test_labels) {
  matching_labels <- c()
  for (test_label in test_labels) {
    matches <- plot_raw_labels[startsWith(plot_raw_labels, test_label)]
    matching_labels <- c(matching_labels, matches)
  }
  return(matching_labels)
}

#' Adds Delta Dots to a Delta Plot
#'
#' This function enhances a delta plot by adding delta dots using the provided parameters.
#' It takes a delta plot, a dabest_effectsize_obj, plotting parameters, x-axis breaks,
#' and delta dots data. The function also considers the main violin type to customize the delta dots.
#'
#' @param delta_plot A ggplot object representing the delta plot.
#' @param dabest_effectsize_obj A dabest_effectsize_obj created by the effect_size() function.
#' @param plot_kwargs A list of parameters used to adjust the appearance of the plot.
#' @param x_axis_breaks A numeric or character vector specifying the x-axis breaks.
#' @param main_violin_type A character string indicating the type of main violin plot ("violin" or "box").
#' @param delta_dots_data A data frame containing the data for delta dots.
#'
#' @return A ggplot object with the delta dots added to the delta plot.
#'
#' @noRd
add_delta_dots_to_delta_plot <- function(delta_plot,
                                         dabest_effectsize_obj,
                                         plot_kwargs,
                                         x_axis_breaks,
                                         main_violin_type,
                                         delta_dots_data) {
  # get delta dots params
  params_delta_dots <- plot_kwargs$params_delta_dots
  pch <- params_delta_dots$pch
  alpha <- params_delta_dots$alpha
  # this parameter is only used for horizontal plots
  cex <- params_delta_dots$cex
  size <- params_delta_dots$size
  side <- params_delta_dots$side
  if (side == "right") {
    side <- 1
  } else {
    side <- -1 # left
  }

  # handling color
  if (dabest_effectsize_obj$is_colour) {
    delta_plot <- delta_plot +
      ggbeeswarm::geom_beeswarm(
        data = delta_dots_data,
        ggplot2::aes(
          x = x_var,
          y = y_var,
          color = colour_var,
        ),
        cex = cex,
        method = "swarm",
        pch = pch,
        side = side,
        size = size,
        alpha = alpha,
        corral = "omit"
      )
  } else {
    if (main_violin_type == "multicolor") {
      delta_plot <- delta_plot +
        ggbeeswarm::geom_beeswarm(
          data = delta_dots_data,
          ggplot2::aes(
            x = x_var,
            y = y_var,
            color = x_var,
          ),
          cex = cex,
          method = "swarm",
          pch = pch,
          side = side,
          size = size,
          alpha = alpha,
          corral = "omit"
        )
    } else {
      delta_plot <- delta_plot +
        ggbeeswarm::geom_beeswarm(
          data = delta_dots_data,
          ggplot2::aes(
            x = x_var,
            y = y_var,
          ),
          cex = cex,
          method = "swarm",
          pch = pch,
          side = side,
          size = size,
          alpha = alpha,
          corral = "omit"
        )
    }
  }

  return(delta_plot)
}

adjust_x_axis_in_delta_plot <- function(delta_plot, main_plot_type, flow, idx, x, delta_y_min, delta_y_mean) {
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
  return(delta_plot)
}

#' Generates horizontal delta texts with customization options.
#'
#' @param x_axis_breaks X-axis breaks for positioning the texts.
#' @param y_values Y-values corresponding to the delta values.
#' @param zero_line_xend max height of the rectangle.
#' @param is_delta2 Bool TRUE if the plot is delta-delta
#' @param colour Fill color for the delta texts. Default is "#FFFFD4", light yellow.
#' @param alpha Transparency level for the delta texts. Default is 0.5.
#' @param fontsize Font size for the delta texts. Default is 12.
#' @param text_colour Color of the delta text. Default is "black".
#' @param text_units Units for the delta values.
#' @param label Label for the delta texts. Default is "Δ".
#'
#' @return List containing data and parameters for delta texts.
#' @noRd
create_horizontal_delta_texts <- function(x_axis_breaks, y_values, zero_line_xend,
                                          is_delta2,
                                          colour = "#FFFFD4",
                                          alpha = 0.5, fontsize = 12, text_colour = "black",
                                          text_units = NULL, label = "Δ") {
  max_x <- zero_line_xend - 0.3
  # Prepare the text for each coordinate
  if (!is.null(text_units)) {
    text_labels <- paste0(sprintf("%+.2f", y_values), text_units)
  } else {
    text_labels <- sprintf("%+.2f", y_values)
  }

  # get the correct position of the labels based on the extreme value of the x axis and the breaks
  correct_position <- max_x - rev(x_axis_breaks)

  texts <- data.frame(
    x = rep(0.15, length(y_values)),
    y = correct_position,
    text = rev(text_labels)
  )
  min_y_value <- min(min(y_values), 0)
  bar_plot <- ggplot2::ggplot() +
    ggplot2::geom_rect(ggplot2::aes(xmin = 0, xmax = 0.35, ymin = min_y_value, ymax = zero_line_xend),
      fill = colour
    )

  bar_plot <- bar_plot +
    ggplot2::geom_text(
      data = texts,
      ggplot2::aes(x = x, y = y, label = text),
      alpha = alpha,
      check_overlap = TRUE,
      size.unit = "pt",
      size = fontsize
    ) +
    ggplot2::theme_void() +
    ggplot2::labs(x = label) +
    ggplot2::theme(axis.title.x = ggplot2::element_text())
  return(bar_plot)
}
