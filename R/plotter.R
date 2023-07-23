#' Main plotting api
#' 
#' @description
#' Contains function `dabest_plot`.
#' 
#' To be used after calculation of effect sizes with the various `effect_size` functions in _stat_tools/effsize.R.

dabest_plot <- function(dabest_effectsize_obj, float_contrast = TRUE, ...) {
  
  if (class(dabest_effectsize_obj)!="dabest_effectsize") {
    cli::cli_abort(c("{.field dabest_effectsize_obj} must be a {.cls dabest_effectsize} object."),
                   "x" = "Please supply a {.cls dabest_effectsize} object.")
  }
  
  plot_kwargs <- list(...)
  plot_kwargs <- assign_plot_kwargs(dabest_effectsize_obj, plot_kwargs)
  
  custom_palette <- plot_kwargs$custom_palette
  
  is_colour <- dabest_effectsize_obj$is_colour
  is_deltadelta <- plot_kwargs$show_delta2
  idx <- dabest_effectsize_obj$idx
  raw_legend <- NULL
  
  if(length(unlist(idx)) >= 3) {
    float_contrast <- FALSE
  }
  
  if(isFALSE(float_contrast)) {
    raw_plot <- plot_raw(dabest_effectsize_obj, float_contrast=FALSE, plot_kwargs)
    delta_plot <- plot_delta(dabest_effectsize_obj, float_contrast=FALSE, plot_kwargs)
    
    delta_range <- delta_plot$delta_range
    delta_plot <- delta_plot$delta_plot
    
    raw_plot <- apply_palette(raw_plot, custom_palette)
    delta_plot <- apply_palette(delta_plot, custom_palette)
    
    raw_legend <- get_legend(raw_plot + 
                               guides(alpha = "none") +
                               theme(legend.box.margin = margin(0, 0, 0, 0)))
    
    # if (isTRUE(is_deltadelta)) {
    #   raw_plot <- plot_grid(raw_plot, ggplot(), nrow = 1, rel_widths = c(0.9, 0.1))
    # }
    
    final_plot <- cowplot::plot_grid(
      plotlist   = list(raw_plot + theme(legend.position="none"), 
                        delta_plot + theme(legend.position="none")),
      nrow       = 2,
      ncol       = 1,
      axis       = "tblr",
      align      = "vh"
    )
    
    if(isTRUE(is_deltadelta)) {
      extra_yaxis_plot <- plot_extra_yaxis(delta_range, "right", 0, "delta-delta")
      
      extra_yaxis_plot <- cowplot::plot_grid(NULL, extra_yaxis_plot, NULL, ncol = 1, rel_heights = c(0.51, 0.42, 0.07))
      
      final_plot <- cowplot::plot_grid(
        plotlist = list(final_plot, extra_yaxis_plot),
        nrow = 1,
        ncol = 2,
        rel_widths = c(0.9, 0.1)
      )
    }
    
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
    raw_plot <- plot_raw(dabest_effectsize_obj, float_contrast=TRUE, plot_kwargs)
    delta_plot <- plot_delta(dabest_effectsize_obj, float_contrast=TRUE, plot_kwargs)
    
    delta_plot_range <- delta_plot$delta_range
    delta_plot <- delta_plot$delta_plot
    
    raw_plot <- apply_palette(raw_plot, custom_palette)
    delta_plot <- apply_palette(delta_plot, custom_palette)
    
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