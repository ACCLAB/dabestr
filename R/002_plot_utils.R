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

create_dfs_for_sankey2 <- function(
    float_contrast = FALSE,
    raw_data,
    proportional_data,
    enquo_x,
    enquo_y,
    enquo_id_col,
    idx,
    scale_factor_sig = 0.8,
    bar_width = 0.15,
    gap,
    sankey = TRUE,
    flow = TRUE,
    N) {
  type <- ifelse(length(unlist(idx)) <= 2,
                 "single sankey",
                 "multiple sankeys"
  )
  
  flow_success_to_failure <- tibble::tibble()
  flow_success_to_success <- tibble::tibble()
  flow_failure_to_success <- tibble::tibble()
  flow_failure_to_failure <- tibble::tibble()
  
  bar_width <- ifelse(float_contrast, 0.15, 0.03)
  
  scale_factor_sig <- switch(type,
                             "single sankey" = if (float_contrast) 0.72 else 0.95,
                             "multiple sankeys" = 0.92
  )
  x_padding <- ifelse(float_contrast, 0.008, 0.006)
  
  prop <- proportional_data
  
  # Prepare sankey_bars
  sankey_bars <- if (flow) {
    proportional_data
  } else {
    create_sankey_bars(proportional_data, enquo_x, enquo_y, idx)
  }
  
  if (sankey) {
    means_c_t <- sankey_bars$proportion_success
    sankey_flows <- create_sankey_flows(
      raw_data,
      enquo_x,
      enquo_y,
      enquo_id_col,
      idx,
      N,
      means_c_t,
      gap,
      bar_width,
      x_padding,
      scale_factor_sig
    )
    
    flow_success_to_failure <- sankey_flows$flow_success_to_failure
    flow_success_to_success <- sankey_flows$flow_success_to_success
    flow_failure_to_success <- sankey_flows$flow_failure_to_success
    flow_failure_to_failure <- sankey_flows$flow_failure_to_failure
  }else{
    flow_success_to_failure <- data.frame(x = NaN, y = NaN, tag = NaN)
    flow_failure_to_success <- data.frame(x = NaN, y = NaN, tag = NaN)
    flow_success_to_success <- data.frame(x = NaN, y = NaN, tag = NaN)
    flow_failure_to_failure <- data.frame(x = NaN, y = NaN, tag = NaN)
  }
  
  redraw_x_axis <- c(1:length(unlist(idx)))
  #redraw_x_axis <- seq_along(unlist(idx))
  
  dfs_for_sankeys <- list(
    flow_success_to_failure = flow_success_to_failure,
    flow_failure_to_success = flow_failure_to_success,
    flow_success_to_success = flow_success_to_success,
    flow_failure_to_failure = flow_failure_to_failure,
    sankey_bars = sankey_bars,
    redraw_x_axis = redraw_x_axis
  )
  
  return(dfs_for_sankeys)
}

