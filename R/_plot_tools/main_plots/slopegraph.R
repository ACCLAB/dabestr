#' Functions that generate main_plots based on the main_plot_type of `create_rawplot_components`.
#' 
#' @description
#' Contains function `plot_slopegraph`.


#' Function that plots slopegraph
plot_slopegraph <- function(dabest_effectsize_obj, plot_kwargs) {
  raw_data <- dabest_effectsize_obj$raw_data
  
  raw_marker_size <- plot_kwargs$raw_marker_size
  raw_marker_alpha <- plot_kwargs$raw_marker_alpha
  
  enquo_x = dabest_effectsize_obj$enquo_x
  enquo_y = dabest_effectsize_obj$enquo_y
  enquo_id_col = dabest_effectsize_obj$enquo_id_col
  enquo_colour = dabest_effectsize_obj$enquo_colour
  
  name_x <- as_name(enquo_x)
  name_y <- as_name(enquo_y)
  
  idx = dabest_effectsize_obj$idx
  
  raw_plot <- ggplot()
  slopegraph_params <- list(linewidth = raw_marker_size, alpha = raw_marker_alpha)
  
  for(subplot_groups in idx) {
    # Assign subplot.
    subplot <- dplyr::filter(raw_data, !!enquo_x %in% subplot_groups)
    
    subplot[[name_x]] <-
      subplot[[name_x]] %>%
      factor(subplot_groups, ordered = TRUE)
    
    slopegraph_params[["data"]] <- subplot
    
    # Assign aesthetic mappings.
    if(rlang::quo_is_null(enquo_colour)) {
      slopegraph_aes <- ggplot2::aes(x_axis_raw, !!enquo_y,
                                     group = !!enquo_id_col)
    } else {
      slopegraph_aes <- ggplot2::aes(x_axis_raw, !!enquo_y,
                                     group = !!enquo_id_col,
                                     col = !!enquo_colour)
    }
    
    slopegraph_params[["mapping"]] <- slopegraph_aes
    
    # Create slopegraph
    raw_plot <-
      raw_plot +
      do.call(ggplot2::geom_line, slopegraph_params)
  }
  
  return(raw_plot)
}