#' Main plotting api
#' 
#' @description
#' Contains function `dabest_plot`.
#' 
#' To be used after calculation of effect sizes with the various `effect_size` functions in _stat_tools/effsize.R.

dabest_plot <- function(dabest_obj.mean_diff, float_contrast = TRUE, ...) {
  plot_kwargs <- list(...)
  plot_kwargs <- assign_plot_kwargs(plot_kwargs)
  
  custom_palette <- plot_kwargs$custom_palette
  
  is_colour <- dabest_obj.mean_diff$is_colour
  idx <- dabest_obj.mean_diff$idx
  raw_legend <- NULL
  
  if(length(unlist(idx)) >= 3) {
    float_contrast <- FALSE
  }
  
  if (isFALSE(float_contrast)) {
    
    raw_plot <- apply_palette(plot_raw(dabest_obj.mean_diff, float_contrast=FALSE, plot_kwargs), custom_palette)
    delta_plot <- apply_palette(plot_delta(dabest_obj.mean_diff, float_contrast=FALSE, plot_kwargs), custom_palette)
    
    if(isTRUE(is_colour)) {
      raw_legend <- get_legend(raw_plot + 
                                 guides(alpha = "none") +
                                 theme(legend.box.margin = margin(0, 0, 0, 0)))
    }
    
    final_plot <- cowplot::plot_grid(
      plotlist   = list(raw_plot + theme(legend.position="none"), 
                        raw_legend, 
                        delta_plot + theme(legend.position="none"),
                        NULL),
      nrow       = 2,
      ncol       = 2,
      rel_widths = c(0.9, 0.1),
      axis       = "lr",
      align      = "b")
    
    return(final_plot)
    
  } else {
    
    raw_plot <-  apply_palette(plot_raw(dabest_obj.mean_diff, float_contrast=TRUE, plot_kwargs), custom_palette)
    delta_plot <- apply_palette(plot_delta(dabest_obj.mean_diff, float_contrast=TRUE, plot_kwargs), custom_palette)
    
    if(isTRUE(is_colour)) {
      raw_legend <- get_legend(raw_plot + 
                                 guides(color = guide_legend(nrow = 1),
                                        alpha = "none") + 
                                 theme(legend.position = "bottom"))
    }
    
    final_plot <- cowplot::plot_grid(
      plotlist   = list(raw_plot + theme(legend.position="none"), 
                        delta_plot + theme(legend.position="none")),
      nrow       = 1,
      ncol       = 2,
      rel_widths = c(0.8, 0.2),
      axis       = "lr",
      align      = "h")
    
    final_plot <- cowplot::plot_grid(final_plot, raw_legend, ncol = 1, rel_heights = c(1, 0.1))
    
    return(final_plot)
  }
  
  final_plot
}