#' Apply Effectsize
#'
#' @description Generates the dabest effectsize objects given the specified effect size
#'
#' @param contrasts A list of contrast objects.
#' @param effect_size Type of effect size (e.g., "mean_diff", "median_diff").
#'
#' @return The list of updated dabest effectsize objects
#'
apply_effectsize <- function(contrasts, effect_size = "mean_diff") {
    print("applying effect size to the contrast objects")
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
    for (i in seq_along(contrasts)) {
        contrast_obj <- contrasts[[i]]
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
    print("checking parameters of effectsize")
    for (i in seq_along(contrasts)) {
        contrast_obj <- contrasts[[i]]
        condition_to_check <- list(
            delta2 = (contrast_obj$delta2 == FALSE),
            minimeta = (contrast_obj$minimeta == FALSE)
        )
        condition <- condition_to_check[[contrast_type]]
        if (condition) {
            cli::cli_abort(c("The contrast type {.field} contrast_type}
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

get_forest_plot_data <- function(contrasts, contrast_type, x_axis_breaks) {
    print("get_forest_plot_data")
    bootstraps <- list()
    differences <- c()
    bca_lows <- c()
    bca_highs <- c()
    df_for_violin <- data.frame(x = NULL, contrast_label = NULL)
    for (i in seq_along(contrasts)) {
        print(paste("i", i))
        dabest_effectsize_obj <- contrasts[[i]]
        # Obtain values
        boot_result <- dabest_effectsize_obj$boot_result
        #  Get the row specific for the contrast_type
        if (contrast_type == "delta2") {
            row <- boot_result %>% dplyr::filter(grepl("Delta2", control_group))
        } else {
            row <- boot_result %>% dplyr::filter(grepl("Minimeta", control_group))
        }
        bootstraps[[i]] <- row$bootstraps[[1]]
        differences <- c(differences, row$difference)
        bca_lows <- c(bca_lows, row$bca_ci_low)
        bca_highs <- c(bca_highs, row$bca_ci_high)
    }
    print(paste("length of bootstraps", length(bootstraps)))
    print(paste("x axis breaks", x_axis_breaks))
    df_for_violin <- create_dfs_for_baseline_ec_violin(
        bootstraps,
        x_axis_breaks,
        float_contrast = FALSE
    )
    plotting_data <- list(
        df_for_violin = df_for_violin,
        differences = differences,
        bca_lows = bca_lows,
        bca_highs = bca_highs
    )
    return(plotting_data)
}

create_violin_forest_plot <- function() {

}

#'  Generates a Forest Plot
#'
#'  This function creates a forest plot summarizing a list of contrasts.
#'
#' @param contrasts A list of contrast objects. These objects should contain the
#'   statistical information for each comparison (e.g., estimates, standard errors).
#' @param contrast_labels A list of labels for the contrast objects. E.g.,
#'   ['Drug1', 'Drug2', 'Drug3'] These labels will be used to identify each comparison on the plot.
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
#'   The default is "ΔΔ Forest".
#' @param fontsize Font size for text elements in the plot. Default is 12.
#' @param title_font_size Font size for text of plot title. Defaults is 16.
#' @param violin_kwargs Additional arguments for violin plot customization. Default is NULL
#' @param marker_size Marker size for plotting mean differences or effect sizes. Default is 20.
#' @param ci_line_width Width of confidence interval lines. Default is 2.5.
#' @param custom_palette A list or dictionary of colors, one for each contrast object.
#' E.g., ['gray', 'blue', 'green'] or {'Drug1':'gray', 'Drug2':'blue', 'Drug3':'green'}. Default NULL.
#' @param rotation_for_xlabels Rotation angle for x-axis labels, improving readability. Default is 45.
#' @param alpha_violin_plot Transparency level for violin plots. Default is 0.8
#' @return A ggplot object representing the forest plot.
#'
#' @noRd
forest_plot <- function(
    contrasts, contrast_labels,
    contrast_type = "delta2",
    effect_size = "mean_diff",
    ylabel = "effect size", title = "ΔΔ Forest",
    fontsize = 12,
    title_font_size = 16,
    # violin_kwargs = NULL,
    marker_size = 20,
    ci_line_width = 2.5,
    custom_palette = NULL,
    rotation_for_xlabels = 45,
    alpha_violin_plot = 0.8) {
    # TODO check parameters

    if ((contrast_type != "delta2") && (contrast_type != "minimeta")) {
        stop(paste0("Invalid contrast_type: ", contrast_type, ". Available options: [delta2, minimeta]"))
    }

    # Apply the corresponding effect size to all objects
    contrasts <- apply_effectsize(contrasts, effect_size)

    # Check that the contrast attributes are correct
    check_contrast_attributes(contrasts, contrast_type)

    # get y title
    y_title <- get_y_title(effect_size)

    delta2 <- (contrast_type == "delta2")
    minimeta <- (contrast_type == "minimeta")
    x_axis_breaks <- seq_along(contrasts)
    print(x_axis_breaks)
    # get data for plotting
    plotting_data <- get_forest_plot_data(contrasts, contrast_type, x_axis_breaks)
    df_for_violin <- plotting_data$df_for_violin
    differences <- plotting_data$differences
    bca_lows <- plotting_data$bca_lows
    bca_highs <- plotting_data$bca_highs

    print("Painting the baseline violinplot")
    forest_plot <-
        ggplot2::ggplot() +
        geom_halfviolin(
            na.rm = TRUE,
            data = df_for_violin,
            ggplot2::aes(x = y, y = x, fill = tag),
            alpha = alpha_violin_plot
        )

    # TODO scale axis the same way as in "add_scaling_component_to_delta_plot"
    es_marker_size <- 0.5
    es_line_size <- 0.8
    # TODO Add bci components. Ask about this function
    forest_plot <- add_bootci_component_to_delta_plot(
        forest_plot, x_axis_breaks,
        bca_lows, bca_highs, differences,
        es_marker_size, es_line_size
    )


    ## Adjust x,y title and labels
    forest_plot <- forest_plot +
        ggplot2::ylab(y_title) +
        ggplot2::theme(axis.title.x = ggplot2::element_blank()) +
        ggplot2::scale_x_discrete(label = contrast_labels)

    #### Adjust font size and title ####
    forest_plot <- forest_plot +
        ggplot2::theme(
            axis.text.x = ggplot2::element_text(size = fontsize),
            axis.text.y = ggplot2::element_text(size = fontsize)
        ) +
        ggplot2::ggtitle(title)

    return(forest_plot)
}
