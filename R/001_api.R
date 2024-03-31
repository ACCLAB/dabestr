# TODO
############## REMOVE THE SOURCE lines, ONLY FOR LOCAL TESTING. AFTER BUILDING IT IS NOT REQUIRED ###################
library(here)
source(file.path(here::here("R"), "001_utils.R"))

#' Loading data with dabestr
#'
#' @description
#' Processes and converts a tidy dataset into the dabestr format.
#' The output of this function is then used as an input for various procedural
#' functions within dabestr to create estimation plots.
#'
#' @param data A tidy dataframe.
#' @param x Column in `data` that contains the treatment groups.
#' @param y Column in `data` that contains the measurement values.
#' @param idx List of control-test groupings for which the
#' effect size will be computed for.
#' @param paired Paired ("sequential" or "baseline"). Used for plots for experiments
#' with repeated-measures designs.
#'
#' If "sequential", comparison happens between each measurement to the one directly
#' preceding it. (control vs group i)
#'
#' If "baseline", comparison happens between each group to a shared control.
#' (group i vs group i+1)
#'
#' @param id_col Column in `data` indicating the identity of the
#'  datapoint if the data is tagged. Compulsory parameter if paired is TRUE.
#' @param ci Default 95. Determines the range of the confidence interval for effect size
#' and bootstrap calculations. Only accepts values between 0 to 100 (inclusive).
#' @param resamples The number of resamples to be used to generate the effect size bootstraps.
#' @param colour Column in `data` that determines the groupings for colour of the
#' swarmplot as opposed to `x`.
#' @param proportional Boolean value determining if proportion plots are being
#' produced.
#' @param minimeta Boolean value determining if mini-meta analysis is conducted.
#' @param delta2 Boolean value determining if delta-delta analysis for
#' 2 by 2 experimental designs is conducted.
#' @param experiment Experiment column name for delta-delta analysis.
#' @param experiment_label String specifying the experiment label that is used to
#' distinguish the experiment and the factors (being used in the plotting labels).
#' @param x1_level String setting the first factor level in
#' a 2 by 2 experimental design.
#'
#' @return
#' Returns a `dabest_obj` list with 18 elements. The following are the elements contained within:
#'
#' - `raw_data` The tidy dataset passed to [load()] that was cleaned and altered for plotting.
#' - `proportional_data` List of calculations related to the plotting of proportion plots.
#' - `enquo_x`  Quosure of x as initially passed to [load()].
#' - `enquo_y`  Quosure of y as initially passed to [load()].
#' - `enquo_id_col` Quosure of id_col as initially passed to [load()].
#' - `enquo_colour` Quosure of colour as initially passed to [load()].
#' - `proportional` Boolean value determining if proportion plots are being
#' produced.
#' - `minimeta` Boolean value determining if mini-meta analysis is conducted.
#' - `delta2` Boolean value determining if delta-delta analysis for
#' 2 by 2 experimental designs is conducted.
#' - `idx` List of control-test groupings for which the
#' effect size will be computed for.
#' - `resamples` The number of resamples to be used to generate the effect size bootstraps.
#' - `is_paired` Boolean value determining if it is a paired plot.
#' - `is_colour` Boolean value determining if there is a specified colour column
#' for the plot.
#' - `paired` Paired ("sequential" or "baseline") as initially passed to [load()].
#' - `ci` Numeric value which determines the range of the confidence interval for effect size
#' and bootstrap calculations. Only accepts values between 0 to 100 (inclusive).
#' - `Ns` List of labels for x-axis of the rawdata swarm plot.
#' - `control_summary` Numeric value for plotting of control summary lines for float_contrast= TRUE.
#' - `test_summary` Numeric value for plotting of test summary lines for float_contrast = TRUE.
#' - `ylim` Vector containing the y limits for the rawdata swarm plot.
#'
#' @examples
#' # Loading in of the dataset
#' data(non_proportional_data)
#'
#' # Creating a dabest object
#' dabest_obj <- load(
#'   data = non_proportional_data, x = Group, y = Measurement,
#'   idx = c("Control 1", "Test 1")
#' )
#'
#' # Printing dabest object
#' print(dabest_obj)
#'
#' @export load
#'
load <- function(
    data,
    x,
    y,
    idx = NULL,
    paired = NULL,
    id_col = NULL,
    ci = 95,
    resamples = 5000,
    colour = NULL,
    proportional = FALSE,
    minimeta = FALSE,
    delta2 = FALSE,
    experiment = NULL,
    experiment_label = NULL,
    x1_level = NULL) {
  # Storing plotting params as quosures
  enquo_x <- rlang::enquo(x)
  enquo_y <- rlang::enquo(y)
  enquo_id_col <- rlang::enquo(id_col)
  enquo_colour <- rlang::enquo(colour)

  is_colour <- !(rlang::quo_is_null(enquo_colour))
  is_id_col <- !(rlang::quo_is_null(enquo_id_col))
  is_paired <- !(is.null(paired))

  name_x <- rlang::as_name(enquo_x)
  name_y <- rlang::as_name(enquo_y)

  #### Checking Validity of params ####
  validate_load_params(data, name_x, name_y, id_col, enquo_id_col, is_id_col, colour, enquo_colour, is_colour, delta2, idx, paired, proportional)


  ## Make idx into a list if it is a vector
  if (typeof(idx) != "list" && !(is.null(idx))) {
    idx <- list(idx)
  }

  ## Check for valid mini-meta
  if (minimeta) {
    if (proportional) {
      cli::cli_abort(c(
        "{.field proportional} is {.strong TRUE} but {.field minimeta} is also {.strong TRUE}.",
        "x" = "{.field proportional} and {.field minimeta} cannot be {.strong TRUE} at the same time."
      ))
    } else if (delta2) {
      cli::cli_abort(c(
        "{.field delta2} is {.strong TRUE} but {.field minimeta} is also {.strong TRUE}.",
        "x" = "{.field delta2} and {.field minimeta} cannot be {.strong TRUE} at the same time."
      ))
    }

    minimeta_idx_lengths <- sapply(idx, length)
    if (any(minimeta_idx_lengths != 2)) {
      cli::cli_abort(c(
        "{.field minimeta} is {.strong TRUE}, but some {.field idx} does not consist of exactly 2 groups",
        "x" = "You can only put in exactly 2 groups in {.field idx} when {.field minimeta} is {.strong TRUE}."
      ))
    }
  }

  if (delta2) {
    if (proportional) {
      cli::cli_abort(c(
        "{.field delta2} is {.strong TRUE} but {.field proportional} is also {.strong TRUE}.",
        "x" = "{.field delta2} and {.field proportional} cannot be {.strong TRUE} at the same time."
      ))
    }

    enquo_experiment <- rlang::enquo(experiment)
    name_experiment <- rlang::as_name(enquo_experiment)

    # Make sure that data is a 2x2 ANOVA case
    if (length(unique(data[[name_experiment]])) != 2) {
      cli::cli_abort(c(
        "{.field experiment} does not have a length of 2.",
        "x" = "There can only be 2 groups in {.field experiment} when {.field delta2} is {.strong TRUE}."
      ))
    }
    if (length(unique(data[[name_x]])) != 2) {
      cli::cli_abort(c(
        "{.field x} does not have a length of 2.",
        "x" = "There can only be 2 groups in {.field x} when {.field delta2} is {.strong TRUE}."
      ))
    }

    # Check for idx, experiment_label and x1_level
    if (is.null(idx)) {
      # Set levels for experiment and x if they are present
      if (!(is.null(experiment_label))) {
        data[[name_experiment]] <- factor(x = data[[name_experiment]], levels = experiment_label)
      }
      if (!(is.null(x1_level))) {
        data[[name_x]] <- factor(x = data[[name_x]], levels = x1_level)
      }
      data <- data %>%
        dplyr::arrange(!!enquo_experiment, !!enquo_x)
    }
    if (is.null(experiment_label)) {
      experiment_label <- unique(data[[name_experiment]])
    }

    data <- data %>%
      dplyr::mutate(grouping = !!enquo_x) %>%
      tidyr::unite(!!enquo_experiment, c(!!enquo_x, !!enquo_experiment), sep = " ", remove = FALSE)
    if (dplyr::as_label(enquo_colour) == "NULL") {
      enquo_colour <- enquo_x
    }
    enquo_x <- enquo_experiment
    name_x <- rlang::as_name(enquo_x)
    is_colour <- TRUE

    # Obtain idx if is null
    if (is.null(idx)) {
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

  ## Check to ensure that each treatment group in idx is present in the x column
  unique_x <- unique(data[[name_x]])
  for (i in 1:length(unlist_idx)) {
    if (!(unlist_idx[i] %in% unique_x)) {
      cli::cli_abort(c(
        "{unlist_idx[i]} not present in {.field x}.",
        "x" = "Ensure that idx does not have any control/treatment groups not present in dataset."
      ))
    }
  }

  raw_data <- data %>%
    dplyr::filter(!!enquo_x %in% unlist_idx) %>%
    dplyr::mutate(x_axis_raw = 0)

  raw_data[[name_x]] <- factor(x = raw_data[[name_x]], levels = unlist_idx)

  for (i in 1:length(unlist_idx)) {
    raw_data <- raw_data %>%
      dplyr::mutate(x_axis_raw = ifelse(
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

  ## Check to ensure control & treatment groups have the same sample size if is_paired is TRUE
  if (is_paired) {
    if (length(unique(Ns$n)) > 1) {
      cli::cli_abort(c("{.field data} is paired, as indicated by {.field paired} but size of control and treatment groups are not equal.",
        "x" = "Ensure that the size of control and treatment groups are the same for paired comparisons."
      ))
    }
  }

  # Extending ylim for plotting
  ylim[1] <- ylim[1] - (ylim[2] - ylim[1]) / 25
  ylim[2] <- ylim[2] + (ylim[2] - ylim[1]) / 25

  if (proportional) {
    proportional_data <- raw_data %>%
      dplyr::select(!!enquo_x, !!enquo_y, !!enquo_id_col, !!enquo_colour) %>%
      dplyr::group_by(!!enquo_x) %>%
      dplyr::summarise(
        proportion_success = mean(!!enquo_y),
        y_success = proportion_success / 2,
        y_failure = (1 + proportion_success) / 2
      )

    control_summary <- proportional_data$proportion_success[1]
    test_summary <- proportional_data$proportion_success[2]
  } else {
    # Calculation of summary lines
    summaries <- raw_data %>%
      dplyr::group_by(!!enquo_x) %>%
      dplyr::summarise(summary_stats = mean(!!enquo_y))

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
    experiment_label = experiment_label,
    minimeta = minimeta,
    delta2 = delta2,
    idx = idx,
    resamples = resamples,
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

#' Print a `dabest` object
#'
#' @noRd
#'
#' @param dabest_obj a list
#' @param ... S3 signature for generic plot function.
#'
#' @return A summary of the experimental designs.
#'
#' @examples
#' # Loading in of the dataset
#' data(twogroup_data)
#'
#' # Creating a dabest object
#' dabest_obj <- load(
#'   data = twogroup_data, x = Group, y = Measurement,
#'   idx = c("Control1", "Group1")
#' )
#'
#' # Display the results in a user-friendly format.
#' print(dabest_obj)
#'
#' @export
print.dabest <- function(x, ...) {
  if (class(x)[1] != "dabest") {
    cli::cli_abort(c("Only dabest class can be used.",
      "x" = "Please enter a valid entry into the function."
    ))
  }

  dabest_obj <- x
  print_greeting_header()

  paired <- dabest_obj$paired
  ci <- dabest_obj$ci

  # Use a lookup table for rm_status and paired_status
  rm_status_lookup <- c(NULL = "", "sequential" = "for the sequential design of repeated-measures experiment \\n", "baseline" = "for repeated measures against baseline \\n")
  paired_status_lookup <- c(NULL = "E", "sequential" = "Paired e", "baseline" = "Paired e")

  rm_status <- rm_status_lookup[paired]
  paired_status <- paired_status_lookup[paired]

  # Create strings
  line1 <- paste0(paired_status, "ffect size(s) ", rm_status)
  line2 <- paste0("with ", ci, "% confidence intervals will be computed for:")
  cat(line1)
  cat(line2)
  cat("\n")
  print_each_comparism(dabest_obj)
  print_ending(dabest_obj)
}
