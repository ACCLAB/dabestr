# Helper functions that generate dfs for plot
# 
# Contains functions `df_for_tufte`, `create_dfs_for_sankey`, `create_dfs_for_xaxis_redraw`, `create_dfs_for_proportion_bar` for generation of dfs.
# 

# Function for creation of df for tuftelines plot
create_df_for_tufte <- function(raw_data, enquo_x, enquo_y, proportional){
  tufte_lines_df <- raw_data %>%
    dplyr::group_by(!!enquo_x) %>%
    dplyr::summarize(mean = mean(!!enquo_y),
                     median = stats::median(!!enquo_y),
                     sd = stats::sd(!!enquo_y),
                     lower_quartile = stats::quantile(!!enquo_y)[2],
                     upper_quartile = stats::quantile(!!enquo_y)[4])
  
  if(isTRUE(proportional)){
    tufte_lines_df <- tufte_lines_df %>%
      dplyr::mutate(sd = sd/7)
  }
  tufte_lines_df <- tufte_lines_df %>%
    dplyr::mutate(lower_sd = mean - sd, upper_sd = mean + sd)
  
  return(tufte_lines_df)
}

# Function for creation of df for flow = FALSE plot
create_dfs_for_nonflow_tufte_lines <- function(idx,
                                               tufte_lines_df,
                                               enquo_x){
  new_tufte_lines_df <- tibble::tibble()
  total_length <- length(unlist(idx))
  temp_idx <- unlist(idx)
  for (i in (1: total_length)){
    group_name <- temp_idx[i]
    row <- tufte_lines_df %>% 
      dplyr::filter(!!enquo_x == group_name)
    new_tufte_lines_df <- dplyr::bind_rows(new_tufte_lines_df, row)
  }
  return (new_tufte_lines_df)
}

# Function for creation of df for sankey plot
create_dfs_for_sankey <-  function(
    float_contrast = FALSE,
    raw_data,
    proportional_data,
    enquo_x,
    enquo_y,
    enquo_id_col,
    x_axis_raw,
    idx,
    scale_factor_sig = 0.8,
    bar_width = 0.15,
    gap,
    sankey = TRUE,
    flow = TRUE,
    N
    ){
  
  type <- ifelse(length(unlist(idx)) <= 2, 
                 "single sankey", 
                 "multiple sankeys")
  
  flow_success_to_failure = tibble::tibble()
  flow_success_to_success = tibble::tibble()
  flow_failure_to_success = tibble::tibble()
  flow_failure_to_failure = tibble::tibble()
  
  bar_width <- ifelse(float_contrast, 0.15, 0.03)
  
  if(type == "single sankey" && float_contrast){
    scale_factor_sig <- 0.72
  } 
  else if(type == "multiple sankeys"){
    scale_factor_sig <- 0.92
  }
  else{
    scale_factor_sig <- 0.95
  }
  x_padding <- ifelse(float_contrast, 0.008, 0.006)
  
  prop <- proportional_data
  ind <- 1
  x_start <- 1
  
  sankey_bars <- prop
  
  if (isFALSE(flow)){
    sankey_bars <- tibble::tibble()
    
    for (group in idx){
      group_length <- length(group)
      
      for (i in 1: (group_length - 1)){
        ctrl <- group[i]
        treat <- group[i+1]
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
  }
  
  means_c_t <- sankey_bars$proportion_success
  if (isTRUE(sankey)){
    for (group in idx) {
      group_length <- length(group)
      
      for (i in 1: (group_length - 1)) {
        #redraw_x_axis <- append(redraw_x_axis, x_start)
        success_success <- raw_data %>%
          dplyr::group_by(!!enquo_id_col) %>%
          dplyr::summarise(success_change =
                      any(!!enquo_y == 1 & !!enquo_x == group[i]) &
                      any(!!enquo_y == 1 &
                            !!enquo_x == group[i + 1])) %>%
          dplyr::filter(success_change) %>%
          dplyr::summarise(SS = dplyr::n() / N)
      
        success_failure <- raw_data %>%
          dplyr::group_by(!!enquo_id_col) %>%
          dplyr::summarise(sf_change =
                      any(!!enquo_y == 1 & !!enquo_x == group[i]) &
                      any(!!enquo_y == 0 &
                            !!enquo_x == group[i + 1])) %>%
          dplyr::filter(sf_change) %>%
          dplyr::summarise(SF = dplyr::n() / N)
        
        failure_failire <- raw_data %>%
          dplyr::group_by(!!enquo_id_col) %>%
          dplyr::summarise(failure_change =
                      any(!!enquo_y == 0 & !!enquo_x == group[i]) &
                      any(!!enquo_y == 0 &
                            !!enquo_x == group[i + 1])) %>%
          dplyr::filter(failure_change) %>%
          dplyr::summarise(FF = dplyr::n() / N)
        
        failure_success <- raw_data %>%
          dplyr::group_by(!!enquo_id_col) %>%
          dplyr::summarise(failure_change =
                      any(!!enquo_y == 0 & !!enquo_x == group[i]) &
                      any(!!enquo_y == 1 &
                            !!enquo_x == group[i + 1])) %>%
          dplyr::filter(failure_change) %>%
          dplyr::summarise(FS = dplyr::n() / N)
        # find values for lower flow success to failure flow
        ss <- success_success$SS[1]
        ff <- failure_failire$FF[1]
        sf <- success_failure$SF[1]
        fs <- failure_success$FS[1]
        sf_start1 <- ss 
        sf_start2 <- means_c_t[ind] - gap/2 
        sf_end1 <- means_c_t[ind + 1] + gap/2 
        sf_end2 <- 1 - ff 
        
        
        # find values for upper flppied flow success to failure flow
        fs_start1 <- 1 - ff
        fs_start2 <- means_c_t[ind] + gap/2 
        fs_end1 <- means_c_t[ind + 1] - gap/2 
        fs_end2 <- ss
        
        # form dataframes from sigmoid / flippedSig functions and the rectangles, later fit into sankeyflow
        sig_success_failure_bot <- sigmoid(x_start + bar_width - x_padding,
                                           scale_factor_sig,
                                           sf_start1 - 0.002,
                                           sf_end1 + 0.002)
        sig_success_failure_top <- sigmoid(x_start + bar_width - x_padding,
                                           scale_factor_sig,
                                           sf_start2 - 0.002,
                                           sf_end2 + 0.002)
        sig_success_failure_bot <- dplyr::arrange(sig_success_failure_bot, dplyr::desc(x))
        sig_failure_success_top <- flipped_sig(x_start + bar_width - x_padding,
                                               scale_factor_sig,
                                               fs_start1 + 0.002,
                                               fs_end1 - 0.002)
        sig_failure_success_bot <- flipped_sig(x_start + bar_width - x_padding,
                                               scale_factor_sig,
                                               fs_start2 + 0.002,
                                               fs_end2 - 0.002)
        sig_failure_success_bot <- dplyr::arrange(sig_failure_success_bot, dplyr::desc(x))
        
        #number of points of data points
        N_points <- length(sig_success_failure_bot)
        # generate the tag column for all of these
        tag <- rep(ind, N_points)
        sankey_success_failure <- rbind(sig_success_failure_top,
                                        sig_success_failure_bot)
        sankey_success_failure <- cbind(sankey_success_failure, tag)
        
        sankey_failure_success <- rbind(sig_failure_success_top,
                                        sig_failure_success_bot)
        sankey_failure_success <- cbind(sankey_failure_success, tag)
        
        rect_flow_x <- c(x_start, x_start + 1)
        
        sankey_failure_failure <- data.frame(x = c(rect_flow_x, rev(rect_flow_x)),
                                             y = c(1, 1, rep(fs_start1, 2)),
                                             tag = c(rep(ind, 4)))
        sankey_success_success <- data.frame(x = c(rect_flow_x, rev(rect_flow_x)),
                                             y = c(rep(ss, 2), 0, 0),
                                             tag = c(rep(ind, 4)))
        
        x_start <- x_start + 1
        
        ind <- ind + 1
        #` update the 4 sankey flow dfs for plotting
        flow_success_to_failure <- dplyr::bind_rows(flow_success_to_failure,
                                             sankey_success_failure)
        flow_success_to_success <- dplyr::bind_rows(flow_success_to_success,
                                             sankey_success_success)
        flow_failure_to_success <- dplyr::bind_rows(flow_failure_to_success,
                                             sankey_failure_success)
        flow_failure_to_failure <- dplyr::bind_rows(flow_failure_to_failure,
                                             sankey_failure_failure)
      }
      
      x_start <- x_start + 1
      ind <- ind + 1
    }
  } else {
    flow_success_to_failure = data.frame(x = NaN, y = NaN, tag = NaN)
    flow_failure_to_success = data.frame(x = NaN, y = NaN, tag = NaN)
    flow_success_to_success = data.frame(x = NaN, y = NaN, tag = NaN)
    flow_failure_to_failure = data.frame(x = NaN, y = NaN, tag = NaN)
  }
  
  redraw_x_axis <- c(1 : length(unlist(idx)))
  dfs_for_sankeys <- list(flow_success_to_failure = flow_success_to_failure,
                          flow_failure_to_success = flow_failure_to_success,
                          flow_success_to_success = flow_success_to_success,
                          flow_failure_to_failure = flow_failure_to_failure,
                          sankey_bars = sankey_bars,
                          redraw_x_axis = redraw_x_axis)
  
  return(dfs_for_sankeys)
}

# Function for creation of df for xaxis redraw for float_contrast FALSE plot
create_dfs_for_xaxis_redraw <- function(idx) {
  x_axis_pointer <- 0
  xaxis_line_x_vector <- c()
  xaxis_line_xend_vector <- c()
  xaxis_ticks_x_vector <- c()
  
  for (j in 1:length(idx)) {
    # calculate xaxis line x coords
    x_coord <- x_axis_pointer + 1
    xaxis_line_x_vector <- append(xaxis_line_x_vector, x_coord)
    xend_coord <- x_axis_pointer + length(idx[[j]])
    xaxis_line_xend_vector <- append(xaxis_line_xend_vector, xend_coord)
    
    # calculate xaxis ticks x coords
    for (k in 1:length(idx[[j]])) {
      x_coord <- x_axis_pointer + k
      xaxis_ticks_x_vector <- append(xaxis_ticks_x_vector, x_coord)
    }
    x_axis_pointer <- x_axis_pointer + length(idx[[j]])
  }
  
  dfs_for_xaxis_redraw <- list(
    df_for_line = data.frame(x = xaxis_line_x_vector,
                             xend = xaxis_line_xend_vector),
    df_for_ticks = data.frame(x = xaxis_ticks_x_vector)
  )
  
  return(dfs_for_xaxis_redraw)
}

create_dfs_for_proportion_bar <- function(proportion_success, bar_width = 0.3, gap = 0) {
  df_for_proportion_bar <- data.frame(
    x_failure = NA,
    y_failure = NA,
    x_success = NA,
    y_success = NA,
    tag = NA
  )
  
  for (x in 1:length(proportion_success)) {
    y <- proportion_success[x]
    
    x_failure_success <- c(x-bar_width/2, x+bar_width/2, x+bar_width/2, x-bar_width/2)
    y_success <- c(y-gap/2, y-gap/2, 0, 0)
    y_failure <- c(1, 1, y+gap/2, y+gap/2)
    temp_df_proportion_bar <- data.frame(
      x_failure = x_failure_success,
      y_failure = y_failure,
      x_success = x_failure_success,
      y_success = y_success,
      tag = rep(toString(x), 4)
    )
    
    df_for_proportion_bar <- rbind(df_for_proportion_bar, temp_df_proportion_bar)
  }
  df_for_proportion_bar <- df_for_proportion_bar %>% stats::na.omit()
  
  return(df_for_proportion_bar)
}

create_dfs_for_baseline_ec_violin <- function(boots, x_idx_position, float_contrast, flow = TRUE) {
  df_for_violin <- data.frame(
    x = NA,
    y = NA,
    tag = NA
  )
  x_axis_scalar <- ifelse(flow, 0, 0.5)
  curr_boot_idx <- 1
  
  for (i in x_idx_position) {
    ci_coords <- stats::density(boots[[curr_boot_idx]])
    
    x_coords_ci <- ci_coords$x
    y_coords_ci <- ci_coords$y
    
    # Standardise y
    y_coords_ci <- (y_coords_ci - min(y_coords_ci))/(max(y_coords_ci) - min(y_coords_ci))
    y_coords_ci <- y_coords_ci/6
    
    if (isFALSE(float_contrast)) {
      y_coords_ci <- y_coords_ci/1.5
    }
    
    y_coords_ci <- y_coords_ci + i - x_axis_scalar
    
    temp_df_violin <- data.frame(x = x_coords_ci,
                                 y = y_coords_ci,
                                 tag = rep(toString(i), 512))
    
    df_for_violin <- rbind(df_for_violin, temp_df_violin)
    
    curr_boot_idx <- curr_boot_idx + 1
  }
  df_for_violin <- df_for_violin %>%
    dplyr::arrange(tag, x , y)
  
  return(df_for_violin)
}