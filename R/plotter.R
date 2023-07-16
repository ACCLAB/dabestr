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
  is_deltadelta <- dabest_obj.mean_diff$delta2
  idx <- dabest_obj.mean_diff$idx
  raw_legend <- NULL
  
  if(length(unlist(idx)) >= 3) {
    float_contrast <- FALSE
  }
  
  if(isFALSE(float_contrast)) {
    raw_plot <- apply_palette(plot_raw(dabest_obj.mean_diff, float_contrast=FALSE, plot_kwargs), custom_palette)
    delta_plot <- apply_palette(plot_delta(dabest_obj.mean_diff, float_contrast=FALSE, plot_kwargs), custom_palette)
    
    raw_legend <- get_legend(raw_plot + 
                               guides(alpha = "none") +
                               theme(legend.box.margin = margin(0, 0, 0, 0)))
    
    if(isTRUE(is_deltadelta)) {
      raw_plot <- cowplot::plot_grid(
        plotlist = list(raw_plot + theme(legend.position="none"), 
                        NULL), 
        nrow = 1, 
        ncol = 2, 
        rel_widths = c(0.9, 0.1)
      )
    }
    
    final_plot <- cowplot::plot_grid(
      plotlist   = list(raw_plot + theme(legend.position="none"), 
                        delta_plot + theme(legend.position="none")),
      nrow       = 2,
      ncol       = 1,
      axis       = "tblr",
      align      = "vh"
    )
    
    if(isTRUE(is_colour)) {
      legend_plot <- cowplot::plot_grid(
        plotlist = list(raw_legend, NULL),
        nrow = 2,
        ncol = 1,
        rel_heights = c(0.1, 0.9)
      )
      
      final_plot <- cowplot::plot_grid(final_plot, legend_plot, ncol = 2, nrow = 1, rel_widths = c(0.9, 0.1))
    }
    
    return(final_plot)
    
  } else {
    #isTRUE(float_contrast)
    raw_plot <-  apply_palette(plot_raw(dabest_obj.mean_diff, float_contrast=TRUE, plot_kwargs), custom_palette)
    delta_plot <- apply_palette(plot_delta(dabest_obj.mean_diff, float_contrast=TRUE, plot_kwargs), custom_palette)
    
    final_plot <- cowplot::plot_grid(
      plotlist   = list(raw_plot + theme(legend.position="none"), 
                        delta_plot + theme(legend.position="none")),
      nrow       = 1,
      ncol       = 2,
      rel_widths = c(0.8, 0.2),
      axis       = "lr",
      align      = "h"
    )
    
    if(isTRUE(is_colour)) {
      raw_legend <- get_legend(raw_plot + 
                                 guides(color = guide_legend(nrow = 1),
                                        alpha = "none") + 
                                 theme(legend.position = "bottom"))
      
      final_plot <- cowplot::plot_grid(final_plot, raw_legend, ncol = 1, rel_heights = c(0.9, 0.1))
    }
    return(final_plot)
  }
}