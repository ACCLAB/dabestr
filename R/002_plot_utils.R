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

# TODO Add documentation
initialize_raw_plot <- function(plot_kwargs, plot_components, dabest_effectsize_obj, df_for_proportion_bar, sankey_df, sankey_bars, idx, float_contrast) {
  raw_data <- dabest_effectsize_obj$raw_data
  enquo_x <- dabest_effectsize_obj$enquo_x
  enquo_y <- dabest_effectsize_obj$enquo_y
  enquo_colour <- dabest_effectsize_obj$enquo_colour
  proportional <- dabest_effectsize_obj$proportional
  raw_marker_size <- plot_kwargs$raw_marker_size
  raw_marker_alpha <- plot_kwargs$raw_marker_alpha
  raw_marker_spread <- plot_kwargs$raw_marker_spread
  raw_marker_side_shift <- plot_kwargs$raw_marker_side_shift
  es_marker_size <- plot_kwargs$es_marker_size
  swarm_x_text <- plot_kwargs$swarm_x_text
  swarm_y_text <- plot_kwargs$swarm_y_text
  asymmetric_side <- plot_kwargs$asymmetric_side
  asymmetric_side <- ifelse(asymmetric_side == "right", -1, 1)
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
  return(list(raw_plot, raw_y_range, raw_y_min))
}
