#' Helper functions that generate dfs for plot
#' 
#' @description
#' Contains functions `df_for_tufte`, `create_dfs_for_sankey`, `create_dfs_for_xaxis_redraw` for generation of dfs.
#' 
#' Also includes `plot_slopegraph` (for plotting of slopegraph).

#' Function for creation of df for tuftelines plot
df_for_tufte <- function(raw_data, enquo_x, enquo_y, proportional){
  tufte_lines_df <- raw_data %>%
    dplyr::group_by(!!enquo_x) %>%
    dplyr::summarize(mean = mean(!!enquo_y),
                     median = median(!!enquo_y),
                     sd = sd(!!enquo_y),
                     lower_quartile = stats::quantile(!!enquo_y)[2],
                     upper_quartile = stats::quantile(!!enquo_y)[4])
  
  if(isTRUE(proportional)){
    tufte_lines_df <- tufte_lines_df %>%
      dplyr::mutate(sd = sd/10)
  }
  tufte_lines_df <- tufte_lines_df %>%
    dplyr::mutate(lower_sd = mean - sd, upper_sd = mean + sd)
  
  return(tufte_lines_df)
}

#' Function for creation of df for sankey plot
create_dfs_for_sankey <- function(float_contrast = FALSE, 
                                  raw_data, 
                                  proportional_data, 
                                  enquo_id_col, 
                                  x_axis_raw, 
                                  ind = 1,
                                  scale_factor_sig = 0.8,
                                  gap
) {
  ind <- 1
  bar_width <- ifelse(float_contrast, 0.15, 0.10)
  means_c_t <- proportional_data$proportion_success
  #for() will use for loops for multiple plot groups
  success_success <- raw_data %>%
    group_by(!!enquo_id_col) %>%
    summarise(success_change = 
                any(Success == 1 & Group == "Control1") & 
                any(Success == 1 & Group == "Test1")) %>%
    filter(success_change) %>%
    summarise(C1T1 = n()/N)
  failure_failire <- raw_data %>%
    group_by(!!enquo_id_col) %>%
    summarise(success_change = 
                any(Success == 0 & Group == "Control1") & 
                any(Success == 0 & Group == "Test1")) %>%
    filter(success_change) %>%
    summarise(C1T1F = n()/N)
  
  # find values for lower flow success to failure flow
  ss <- success_success$C1T1[1]
  value_start1 <- success_success$C1T1[1] - gap/8
  value_start2 <- means_c_t[1]- gap/2 - gap/8
  value_end1 <- means_c_t[2] + gap/2 +gap/8
  value_end2 <- 1- failure_failire$C1T1F[1] + gap/8
  
  
  # find values for upper flppied flow success to failure flow
  flow_start1 <- 1- failure_failire$C1T1F[1]
  flow_end1 <- means_c_t[2] - gap/2
  flow_start2 <- means_c_t[1] + gap/2
  flow_end2 <- success_success$C1T1[1]
  
  # form dataframes from sigmoid/ flippedSig functions and the rectangles, later fit into sankeyflow
  sig1 <- sigmoid(ind + bar_width, scale_factor_sig, value_start1, value_end1)
  sig2 <- sigmoid(ind + bar_width, scale_factor_sig, value_start2, value_end2)
  sig1 <- arrange(sig1, desc(x))
  sig3 <- flipped_sig(ind + bar_width, scale_factor_sig, flow_start1, flow_end1)
  sig4 <- flipped_sig(ind + bar_width, scale_factor_sig, flow_start2, flow_end2)
  sig4 <- arrange(sig4, desc(x))
  data_for_flow1 <- rbind(sig2, sig1)
  data_for_flow2 <- rbind(sig3, sig4)
  data_for_rect_top <- data.frame(
    x = c(x_axis_raw, rev(x_axis_raw)), 
    y = c(1, 1, rep(flow_start1, 2)))
  data_for_rect_bot <- data.frame(
    x = c(x_axis_raw, rev(x_axis_raw)), 
    y = c(rep(ss,2), 0, 0))
  
  #prepare data for bargraphs of paired proportional data
  data_for_bars <- proportional_data
  
  
  list_of_dfs <- list(
    flow1 = data_for_flow1,
    flow2 = data_for_flow2, 
    rect_top = data_for_rect_top, 
    rect_bot = data_for_rect_bot,
    bars = proportional_data
  )
  list_of_dfs
}

#' Function for creation of df for xaxis redraw for float_contrast FALSE plot
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