#' Validates and loads parameters
#'
#' Performs validation checks on the input parameters for the load function.
#'
#' @param data Input data.
#' @param name_x Name of the x-axis variable.
#' @param name_y Name of the y-axis variable.
#' @param id_col Name of the ID column.
#' @param enquo_id_col Quoted name of the ID column.
#' @param is_id_col Bool indicating if the ID column is present.
#' @param colour Name of the column for color mapping.
#' @param enquo_colour Quoted name of the color column.
#' @param is_colour Bool indicating if color mapping is used.
#' @param delta2 Value for delta calculation.
#' @param idx Index or grouping information.
#' @param paired Bool indicating if the data is paired.
#' @param proportional Bool indicating if proportions are used.
#'
#' @noRd
validate_load_params <- function(data, name_x, name_y,
                                 id_col, enquo_id_col, is_id_col,
                                 colour, enquo_colour, is_colour,
                                 delta2, idx, paired,
                                 proportional) {
  is_paired <- !(is.null(paired))

  if (!name_x %in% colnames(data)) {
    cli::cli_abort(c("Column {.field x} is {.emph not} in {.field data}.",
      "x" = "Please enter a valid entry for {.field x} in {.fun load}."
    ))
  }

  if (!name_y %in% colnames(data)) {
    cli::cli_abort(c("Column {.field y} is {.strong not} in {.field data}.",
      "x" = "Please enter a valid entry for {.field y} in {.fun load}."
    ))
  }

  if (is_id_col) {
    if (!(rlang::as_name(enquo_id_col) %in% colnames(data))) {
      cli::cli_abort(c("Column {.field id_col} is {.strong not} in {.field data}.",
        "x" = "Please enter a valid entry for {.field id_col} in {.fun load}."
      ))
    }
  }

  if (is_colour) {
    if (!(rlang::as_name(enquo_colour) %in% colnames(data))) {
      cli::cli_abort(c("Column {.field colour} is {.strong not} in {.field data}.",
        "x" = "Please enter a valid entry for {.field colour} in {.fun load}."
      ))
    }
  }

  if (!delta2) {
    if (is.null(idx)) {
      cli::cli_abort(c("Column {.field idx} is currently NULL.",
        "x" = "Please enter a valid entry for {.field idx} in {.fun load}."
      ))
    }
    if (is.list(idx)) {
      general_idx_lengths <- sapply(idx, length)
      if (any(general_idx_lengths < 2)) {
        cli::cli_abort(c("Some {.field idx} does not consist of at least 2 groups",
          "x" = "Make sure each nested group in {.field idx} has length >=2."
        ))
      }
    } else {
      general_idx_lengths <- length(idx)
      if (any(general_idx_lengths < 2)) {
        cli::cli_abort(c("Some {.field idx} does not consist of at least 2 groups",
          "x" = "Make sure each nested group in {.field idx} has length >=2."
        ))
      }
    }
  }

  ## Check that data is proportional
  if (proportional) {
    values <- unique(data[[name_y]])
    if (!(setequal(c(0, 1), values))) {
      cli::cli_abort(c(
        "{.field proportional} is {.strong TRUE} but {.field data} is not proportional.",
        "x" = "{.field y} Column of {.field data} should only contain 1 and 0."
      ))
    }
  }

  ## Check that id_col is not NULL if is_paired is TRUE
  if (is_paired & !(is_id_col)) {
    cli::cli_abort(c(
      "{.field paired} is {.strong TRUE} but no {.field id_col} was supplied.",
      "x" = "Please enter an entry for {.field id_col} in {.fun load}."
    ))
  }

  ## Check that paired must be either "baseline" or "sequential"
  if (is_paired) {
    if (!paired %in% c("baseline", "sequential")) {
      cli::cli_abort(c(
        "{.field paired} is not 'baseline' or 'sequential'.",
        "x" = "{.field paired} can only be 'baseline' or 'sequential'."
      ))
    }
  }
}

#' Validates parameters for minimeta analysis
#'
#' Checks and validates parameters specific to the minimeta analysis.
#'
#' @param proportional Bool indicating if proportions are used.
#' @param delta2 Boolean value determining if delta-delta analysis for
#' 2 by 2 experimental designs is conducted.
#' @param minimeta_idx_lengths Lengths of index vectors for minimeta analysis.
#'
#' @noRd
validate_minimeta_params <- function(proportional, delta2, minimeta_idx_lengths) {
  if (proportional) {
    cli::cli_abort(c(
      "{.field proportional} is {.strong TRUE} but {.field minimeta} is also {.strong TRUE}.",
      "x" = "{.field proportional} and {.field minimeta} cannot be {.strong TRUE} at the same time."
    ))
  }
  if (delta2) {
    cli::cli_abort(c(
      "{.field delta2} is {.strong TRUE} but {.field minimeta} is also {.strong TRUE}.",
      "x" = "{.field delta2} and {.field minimeta} cannot be {.strong TRUE} at the same time."
    ))
  }

  if (any(minimeta_idx_lengths != 2)) {
    cli::cli_abort(c(
      "{.field minimeta} is {.strong TRUE}, but some {.field idx} does not consist of exactly 2 groups",
      "x" = "You can only put in exactly 2 groups in {.field idx} when {.field minimeta} is {.strong TRUE}."
    ))
  }
}

#' TODO Add documentation
#' @noRd
check_dabest_object <- function(dabest_obj) {
  if (!inherits(dabest_obj, "dabest")) {
    cli::cli_abort(c("{.field dabest_obj} must be a {.cls dabest} object."),
      "x" = "Please supply a {.cls dabest} object."
    )
  }
}

#' TODO Add documentation
#' @noRd
check_effectsize_object <- function(dabest_effectsize_obj) {
  if (!inherits(dabest_effectsize_obj, "dabest_effectsize")) {
    cli::cli_abort(c("{.field dabest_effectsize_obj} must be a {.cls dabest_effectsize} object.",
      "x" = "Please supply a {.cls dabest_effectsize} object."
    ))
  }
}
