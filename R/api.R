#' Main api
#' 
#' @description
#' Contains unified `load()` function that checks the data for errors and configures it to 
#' the proper format for effect size calculations.

load <- function(
    data,
    x = NULL,
    y = NULL,
    experiment = NULL,
    idx = NULL,
    paired = NULL,
    id_col = NULL,
    ci = 95,
    colour = NULL,
    proportional = FALSE,
    minimeta = FALSE,
    delta2 = FALSE
){
  
  # Storing plotting params as quosures
  enquo_x <- enquo(x)
  enquo_y <- enquo(y)
  enquo_id_col <- enquo(id_col)
  enquo_colour <- enquo(colour)
  
  is_colour <- isTRUE(as_label(enquo_colour) != "NULL")
  is_paired <- isFALSE(is.null(paired))
  
  # to add: if is_paired is TRUE --> id_col cannot be NULl
  # to add: paired must be either "baseline" or "sequential"
  
  name_x <- as_name(enquo_x)
  name_y <- as_name(enquo_y)
  
  if (isTRUE(delta2)) {
    enquo_experiment <- enquo(experiment)
    name_experiment <- as_name(enquo_experiment)
    
    # Make sure that data is a 2x2 ANOVA case
    if (length(unique(data[[name_experiment]]))!=2) {
      stop("`experiment` does not have a length of 2")
    } 
    
    if (length(unique(data[[name_x]]))!=2) {
      stop("`x` does not have a length of 2")
    }
    
    data <- data %>%
      mutate(grouping = !!enquo_x) %>%
      unite(!!enquo_experiment,c(!!enquo_x,!!enquo_experiment),sep = " ",remove=FALSE)
    if (as_label(enquo_colour) == "NULL") {
      enquo_colour <- enquo_x
    }
    enquo_x <- enquo_experiment
    name_x <- as_name(enquo_x)
    is_colour <- TRUE
  }
  
  unlist_idx <- unlist(idx)
  
  if (!is.null(idx)){
    raw_data <- data %>%
      filter(!!enquo_x %in% unlist_idx) %>%
      mutate(x_axis_raw = 0)
    
    raw_data[[name_x]] = factor(x = raw_data[[name_x]], levels = unlist_idx)
    
    for (i in 1:length(unlist_idx)) {
      raw_data <- raw_data %>%
        mutate(x_axis_raw = ifelse(
          !!enquo_x == unlist_idx[i], i, x_axis_raw 
        ))
    }
    
    # Obtain raw_y_range_vector
    ylim <- range(raw_data[[name_y]])
    
    # Creation of x-axis label
    Ns <- raw_data %>%
      dplyr::group_by(!!enquo_x) %>%
      dplyr::count()
    Ns$swarmticklabs <- do.call(paste, c(Ns[c(name_x, "n")], sep = "\nN = "))
    
    if(isTRUE(proportional)){
      ## include checks here for data to see if it is proportional data
      proportional_data <- raw_data %>%
        select(!!enquo_x, !!enquo_y, !!enquo_id_col, !!enquo_colour) %>%
        group_by(!!enquo_x) %>%
        summarise(proportion_success = mean(!!enquo_y),
                  y_success = proportion_success/2,
                  y_failure = (1+proportion_success)/2)
      
      control_summary <- proportional_data$proportion_success[1]
      test_summary <- proportional_data$proportion_success[2]
      
    } else {
      
      # Extending ylim for plotting
      ylim[1] <- ylim[1] - (ylim[2]-ylim[1])/25
      ylim[2] <- ylim[2] + (ylim[2]-ylim[1])/25
      
      # Calculation of summary lines
      summaries <- raw_data %>%
        group_by(!!enquo_x) %>%
        summarise(summary_stats = mean(!!enquo_y))
      
      # Only currently works for two-groups, if needed for extended features in future, to be changed
      control_summary <- summaries$summary_stats[1]
      test_summary <- summaries$summary_stats[2]
      
      proportional_data <- NULL
    }
    
    dabest_object <- list(
      raw_data = raw_data,
      proportional_data = proportional_data,
      enquo_x = enquo_x,
      enquo_y = enquo_y,
      enquo_id_col = enquo_id_col,
      enquo_colour = enquo_colour,
      proportional = proportional,
      minimeta = minimeta,
      delta2 = delta2,
      idx = idx,
      is_paired = is_paired,
      is_colour = is_colour,
      paired = paired,
      ci = ci,
      Ns = Ns,
      control_summary = control_summary,
      test_summary = test_summary,
      ylim = ylim
    )
    
    class(dabest_object) <- c("dabest")
    
    return(dabest_object)
  }
  stop()
}