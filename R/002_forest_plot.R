#' Apply Effectsize
#'
#' @description Generates the dabest effectsize objects given the specified effect size
#'
#' @param contrasts A list of contrast objects.
#' @param effect_size Type of effect size (e.g., "mean_diff", "median_diff").
#'
#' @return The list of updated dabest effectsize objects
#' @noRd
#'
apply_effectsize <- function(contrast_objects, effect_size = "mean_diff") {
  effect_attr_map <- list(
    mean_diff = "mean_diff",
    median_diff = "median_diff",
    cliffs_delta = "cliffs_delta",
    cohens_d = "cohens_d",
    hedges_g = "hedges_g"
  )

  effect_attr <- effect_attr_map[[effect_size]]

  if (is.null(effect_attr)) {
    stop(paste0("Invalid effect_size: ", effect_size))
  }
  contrasts_updated <- list()
  # Apply effect_size attributes
  for (i in seq_along(contrast_objects)) {
    contrast_obj <- contrast_objects[[i]]
    if (effect_attr == "mean_diff") {
      contrast_obj <- contrast_obj %>% dabestr::mean_diff()
    } else if (effect_attr == "median_diff") {
      contrast_obj <- contrast_obj %>% dabestr::median_diff()
    } else if (effect_attr == "cohens_d") {
      contrast_obj <- contrast_obj %>% dabestr::cohens_d()
    } else if (effect_attr == "hedges_g") {
      contrast_obj <- contrast_obj %>% dabestr::hedges_g()
    } else if (effect_attr == "cliffs_delta") {
      contrast_obj <- contrast_obj %>% dabestr::cliffs_delta()
    }
    contrasts_updated[[i]] <- contrast_obj
  }
  return(contrasts_updated)
}

check_contrast_attributes <- function(contrasts, contrast_type) {
  for (i in seq_along(contrasts)) {
    contrast_obj <- contrasts[[i]]
    condition_to_check <- list(
      delta2 = (contrast_obj$delta2 == FALSE),
      minimeta = (contrast_obj$minimeta == FALSE)
    )
    condition <- condition_to_check[[contrast_type]]
    if (condition) {
      cli::cli_abort(c("The contrast type {.field contrast_type}
            is not TRUE in the given contrast object"))
    }
  }
}

get_y_title <- function(effect_size) {
  effect_attr_map <- list(
    mean_diff = "Mean Difference",
    median_diff = "Median Difference",
    cliffs_delta = "Cliffs Delta",
    cohens_d = "Cohens d",
    hedges_g = "Hedges g"
  )
  y_title <- effect_attr_map[[effect_size]]

  if (is.null(y_title)) {
    stop(paste0("Invalid effect_size: ", effect_size))
  }
  return(y_title)
}

get_forest_plot_data <- function(contrast_objects, contrast_type, x_axis_breaks) {
  bootstraps <- list()
  differences <- c()
  bca_lows <- c()
  bca_highs <- c()
  df_for_violin <- data.frame(x = NULL, contrast_label = NULL)
  for (i in seq_along(contrast_objects)) {
    dabest_effectsize_obj <- contrast_objects[[i]]
    # Obtain values
    boot_result <- dabest_effectsize_obj$boot_result
    #  Get the row specific for the contrast_type
    if (contrast_type == "delta2") {
      row <- boot_result %>% dplyr::filter(grepl("Delta2", .data$control_group))
    } else {
      row <- boot_result %>% dplyr::filter(grepl("Minimeta", .data$control_group))
    }
    bootstraps[[i]] <- row$bootstraps[[1]]
    differences <- c(differences, row$difference)
    bca_lows <- c(bca_lows, row$bca_ci_low)
    bca_highs <- c(bca_highs, row$bca_ci_high)
  }
  df_for_violin <- create_dfs_for_baseline_ec_violin(
    bootstraps,
    x_axis_breaks,
    float_contrast = TRUE
  )
  plotting_data <- list(
    df_for_violin = df_for_violin,
    differences = differences,
    bca_lows = bca_lows,
    bca_highs = bca_highs
  )
  return(plotting_data)
}

create_violin_plot <- function(df_for_violin, violin_kwargs, alpha_violin_plot, custom_palette) {
  stat <- "identity"
  position <- "identity"

  if (!is.null(violin_kwargs)) {
    if (!is.null(violin_kwargs$stat)) {
      stat <- violin_kwargs$stat
    }
    if (!is.null(violin_kwargs$position)) {
      position <- violin_kwargs$position
    }
  }
  forest_plot <-
    ggplot2::ggplot() +
    geom_halfviolin(
      na.rm = TRUE,
      data = df_for_violin,
      ggplot2::aes(x = y, y = x, fill = tag),
      alpha = alpha_violin_plot,
      stat = stat,
      position = position
    )
  if (!is.null(custom_palette)) {
    forest_plot <-
      forest_plot +
      ggplot2::scale_fill_manual(values = custom_palette)
  }
  return(forest_plot)
}

#'  Generates a Forest Plot
#'
#'  This function creates a forest plot summarizing a list of contrasts.
#'
#' @param contrasts A list of contrast objects. These objects should contain the
#'   statistical information for each comparison (e.g., estimates, standard errors).
#' @param contrast_labels A list of labels for the contrast objects. E.g.,
#'   c('Drug1', 'Drug2', 'Drug3') These labels will be used to identify each comparison on the plot.
#' @param contrast_type Select between "delta2" (for delta-delta) or "minimeta"
#'   for mini-meta analysis. This determines the type of effect size calculation
#'   used in the plot.
#' @param effect_size Character string specifying the effect size metric to display.
#'   Valid options include "mean_diff", "median_diff", "cliffs_delta", "cohens_d",
#'   "hedges_g", or "delta_g". The default is "mean_diff".
#' @param ylabel Character string specifying the axis label for the dependent
#'   variable (Y-axis for vertical layout, X-axis for horizontal layout).
#'    The default is "value".
#' @param title Character string specifying the title for the forest plot.
#'   The default is "Delta delta Forest".
#' @param fontsize Font size for text elements in the plot. Default is 12.
#' @param title_font_size Font size for text of plot title. Defaults is 16.
#' @param violin_kwargs Additional arguments for violin plot customization. Default is NULL
#' @param marker_size Marker size for plotting mean differences or effect sizes. Default is 20.
#' @param ci_line_width Width of confidence interval lines. Default is 2.5.
#' @param custom_palette A list or key:value pair of colors, one for each contrast object.
#' E.g., c('gray', 'blue', 'green') or c('Drug1'='gray', 'Drug2'='blue', 'Drug3'='green'). Default NULL.
#' @param rotation_for_xlabels Rotation angle for x-axis labels, improving readability. Default is 45.
#' @param alpha_violin_plot Transparency level for violin plots. Default is 0.8
#' @return A ggplot object representing the forest plot.
#'
#' @export forest_plot
forest_plot <- function(
    contrasts, 
    contrast_labels,
    contrast_type = "delta2",
    effect_size = "mean_diff",
    ylabel = "effect size",
    title = "Delta Delta Forest",
    fontsize = 12,
    title_font_size = 16,
    violin_kwargs = NULL,
    marker_size = 1.1,
    ci_line_width = 1.3,
    custom_palette = NULL,
    rotation_for_xlabels = 0,
    alpha_violin_plot = 0.8) {

  contrast_objects <- contrasts # keeping the interface of using contrasts as param but contrast_objects interally for clarity.
  
  if (is.null(contrast_objects)) {
    stop("Error: no contrast parameter was found")
  }
  if (is.null(contrast_labels)) {
    stop("Error: no contrast labels parameter was found")
  }
  # Assert that both vectors have the same length
  stopifnot(length(contrast_objects) == length(contrast_labels))

  if ((contrast_type != "delta2") && (contrast_type != "minimeta")) {
    stop(paste0("Invalid contrast_type: ", contrast_type, ". Available options: [delta2, minimeta]"))
  }

  # Apply the corresponding effect size to all objects
  contrast_objects <- apply_effectsize(contrast_objects, effect_size)

  # Check that the contrast attributes are correct
  check_contrast_attributes(contrast_objects, contrast_type)

  # get y title
  y_title <- get_y_title(effect_size)

  delta2 <- (contrast_type == "delta2")
  minimeta <- (contrast_type == "minimeta")
  x_axis_breaks <- seq_along(contrast_objects)
  # get data for plotting
  plotting_data <- get_forest_plot_data(contrast_objects, contrast_type, x_axis_breaks)
  df_for_violin <- plotting_data$df_for_violin
  df_for_violin <- df_for_violin %>% tidyr::drop_na()
  differences <- plotting_data$differences
  bca_lows <- plotting_data$bca_lows
  bca_highs <- plotting_data$bca_highs

  forest_plot <- create_violin_plot(df_for_violin, violin_kwargs, alpha_violin_plot, custom_palette)

  # Add bci components
  forest_plot <- add_bootci_component_to_delta_plot(
    forest_plot, x_axis_breaks,
    bca_lows, bca_highs, differences,
    marker_size, ci_line_width
  )

  ### White background
  forest_plot <- forest_plot +
    ggplot2::theme_classic()

  ## Adjust axis, title and labels
  forest_plot <- forest_plot +
    ggplot2::coord_cartesian(
      ylim = c(min(df_for_violin$x), max(df_for_violin$x)),
      xlim = c(0.8, length(contrast_labels) + 0.5),
      expand = FALSE,
      clip = "off"
    ) +
    ggplot2::ylab(y_title) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_text(size = fontsize),
      axis.line.x = ggplot2::element_blank() ## Remove the x-axis line
    ) +
    ggplot2::scale_x_continuous(breaks = seq(1, length(contrast_labels), by = 1), labels = contrast_labels)

  forest_plot <- forest_plot +
    ggplot2::ggtitle(title) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5), # centered
      axis.text.x = ggplot2::element_text(size = fontsize, angle = rotation_for_xlabels, vjust = -0.01),
      axis.text.y = ggplot2::element_text(size = fontsize),
      legend.position = "none",
      plot.margin = ggplot2::margin(t = 0.5, r = 0, b = 1, l = 0.5, unit = "lines")
    )
  #### Add y = 0 line Component ####
  zero_line_xend <- length(contrast_labels) + 0.3
  forest_plot <- forest_plot +
    ggplot2::geom_segment(
      colour = "black",
      linewidth = 0.3,
      ggplot2::aes(
        x = 0.8,
        xend = zero_line_xend,
        y = 0,
        yend = 0
      )
    )

  return(forest_plot)
}
