#' Main api
#' 
#' @description
#' Contains unified `load()` function that checks the data for errors and configures it to 
#' the proper format for effect size calculations.

load <- function(
    data,
    x,
    y,
    idx = NULL,
    paired = NULL,
    id_col = NULL,
    ci = 95,
    colour = NULL,
    proportional = FALSE,
    minimeta = FALSE,
    delta2 = FALSE,
    experiment = NULL,
    experiment_label = NULL,
    x1_level = NULL
){
  
  # Storing plotting params as quosures
  enquo_x <- enquo(x)
  enquo_y <- enquo(y)
  enquo_id_col <- enquo(id_col)
  enquo_colour <- enquo(colour)
  
  is_colour <- isFALSE(quo_is_null(enquo_colour))
  is_id_col <- isFALSE(quo_is_null(enquo_id_col))
  is_paired <- isFALSE(is.null(paired))
  
  name_x <- as_name(enquo_x)
  name_y <- as_name(enquo_y)
  
  #### Checking Validity of params ####
  if (isFALSE(name_x %in% colnames(data))) {
    cli::cli_abort(c("Column {.field x} is {.emph not} in {.field data}.", 
                     "x" = "Please enter a valid entry for {.field x} in {.fun load}."))
  }
  if (isFALSE(name_y %in% colnames(data))) {
    cli::cli_abort(c("Column {.field y} is {.strong not} in {.field data}.", 
                     "x" = "Please enter a valid entry for {.field y} in {.fun load}."))
  }
  if (isTRUE(is_id_col)) {
    if (isFALSE(as_name(enquo_id_col) %in% colnames(data))) {
      cli::cli_abort(c("Column {.field id_col} is {.strong not} in {.field data}.", 
                       "x" = "Please enter a valid entry for {.field id_col} in {.fun load}."))
    }
  }
  if (isTRUE(is_colour)) {
    if (isFALSE(as_name(enquo_colour) %in% colnames(data))) {
      cli::cli_abort(c("Column {.field colour} is {.strong not} in {.field data}.", 
                       "x" = "Please enter a valid entry for {.field colour} in {.fun load}."))
    }
  }
  
  if (isFALSE(delta2)) {
    if (is.list(idx)) {
      general_idx_lengths <- sapply(idx,length)
      if (any(general_idx_lengths<2)==TRUE) {
        cli::cli_abort(c("Some {.field idx} does not consist of at least 2 groups",
                         "x" = "Make sure each nested group in {.field idx} has length >=2."))
      }
    } else {
      general_idx_lengths <- length(idx)
      if (any(general_idx_lengths<2)==TRUE) {
        cli::cli_abort(c("Some {.field idx} does not consist of at least 2 groups",
                         "x" = "Make sure each nested group in {.field idx} has length >=2."))
      }
    }
  }  
  
  
  ## Check that data is proportional
  if (isTRUE(proportional)) {
    values <- unique(data[[name_y]])
    if (isFALSE(setequal(c(0,1), values))) {
      cli::cli_abort(c("{.field proportional} is {.strong TRUE} but {.field data} is not proportional.", 
                       "x" = "{.field y} Column of {.field data} should only contain 1 and 0."))
    }
  }
  
  ## Check that id_col is not NULL if is_paired is TRUE
  if (isTRUE(is_paired) & isFALSE(is_id_col)) {
    cli::cli_abort(c("{.field paired} is {.strong TRUE} but no {.field id_col} was supplied.", 
                     "x" = "Please enter an entry for {.field id_col} in {.fun load}."))
  }
  
  ## Check that paired must be either "baseline" or "sequential"
  if (isTRUE(is_paired)) {
    if (isFALSE(paired %in% c("baseline","sequential"))) {
      cli::cli_abort(c("{.field paired} is not 'baseline' or 'sequential'.", 
                       "x" = "{.field paired} can only be 'baseline' or 'sequential'."))
    }
  }
  
  ## Check for valid mini-meta 
  if (isTRUE(minimeta)) {
    if (isTRUE(proportional)) {
      cli::cli_abort(c("{.field proportional} is {.strong TRUE} but {.field minimeta} is also {.strong TRUE}.", 
                       "x" = "{.field proportional} and {.field minimeta} cannot be {.strong TRUE} at the same time."))
    } else if (isTRUE(delta2)) {
      cli::cli_abort(c("{.field delta2} is {.strong TRUE} but {.field minimeta} is also {.strong TRUE}.", 
                       "x" = "{.field delta2} and {.field minimeta} cannot be {.strong TRUE} at the same time."))
    }
    
    minimeta_idx_lengths <- sapply(idx,length)
    if (any(minimeta_idx_lengths!=2)==TRUE) {
      cli::cli_abort(c("{.field minimeta} is {.strong TRUE}, but some {.field idx} does not consist of exactly 2 groups",
                       "x" = "You can only put in exactly 2 groups in {.field idx} when {.field minimeta} is {.strong TRUE}."))
    }
  }
  
  if (isTRUE(delta2)) {
    if (isTRUE(proportional)) {
      cli::cli_abort(c("{.field delta2} is {.strong TRUE} but {.field proportional} is also {.strong TRUE}.", 
                       "x" = "{.field delta2} and {.field proportional} cannot be {.strong TRUE} at the same time."))
    }
    
    enquo_experiment <- enquo(experiment)
    name_experiment <- as_name(enquo_experiment)
    
    # Make sure that data is a 2x2 ANOVA case
    if (length(unique(data[[name_experiment]]))!=2) {
      cli::cli_abort(c("{.field experiment} does not have a length of 2.", 
                       "x" = "There can only be 2 groups in {.field experiment} when {.field delta2} is {.strong TRUE}."))
    } else if (length(unique(data[[name_x]]))!=2) {
      cli::cli_abort(c("{.field x} does not have a length of 2.", 
                       "x" = "There can only be 2 groups in {.field x} when {.field delta2} is {.strong TRUE}."))
    }
    
    # Check for idx, experiment_label and x1_level
    if (isTRUE(is.null(idx))) {
      # Set levels for experiment and x if they are present
      if (isFALSE(is.null(experiment_label))) {
        data[[name_experiment]] = factor(x = data[[name_experiment]], levels = experiment_label)
      }
      if (isFALSE(is.null(x1_level))) {
        data[[name_x]] = factor(x = data[[name_x]], levels = x1_level)
      }
      data <- data %>%
        arrange(!!enquo_experiment, !!enquo_x)
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
    
    # Obtain idx if is null
    if (isTRUE(is.null(idx))) {
      spread_idx <- unique(data[[name_experiment]])
      idx <- list()
      delta_group_size <- 2
      curr_group_size <- 0
      curr_group_vector <- c()
      for (group_name in spread_idx) {
        if (curr_group_size == delta_group_size) {
          curr_group_size <- 0
          idx <- c(idx, list(curr_group_vector))
          curr_group_vector <- c()
        }
        curr_group_vector <- append(curr_group_vector, group_name)
        curr_group_size <- curr_group_size + 1
      }
      idx <- c(idx, list(curr_group_vector))
    }
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
    
    # Extending ylim for plotting
    ylim[1] <- ylim[1] - (ylim[2]-ylim[1])/25
    ylim[2] <- ylim[2] + (ylim[2]-ylim[1])/25
    
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