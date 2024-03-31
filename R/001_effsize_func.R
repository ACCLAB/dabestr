# TODO
############## REMOVE THE SOURCE lines, ONLY FOR LOCAL TESTING. AFTER BUILDING IT IS NOT REQUIRED ###################
library(here)
source(file.path(here::here("R"), "001_utils.R"))

# TODO Add documentation
effect_size_mean_func <- function(control, test, paired) {
  mean_diff <- mean(test) - mean(control)
  if (paired == TRUE) {
    mean_diff <- mean(test - control)
  }
  mean_diff
}

# TODO Add documentation
effect_size_median_func <- function(control, test, paired) {
  median_diff <- stats::median(test) - stats::median(control)
  if (paired == TRUE) {
    median_diff <- stats::median(test - control)
  }
  median_diff
}



# TODO Add documentation
effect_size_cohen_func <- function(control, test, paired) {
  return(effsize::cohen.d(test, control, paired = paired)$estimate)
}

# TODO Add documentation
cohens_d_ <- function(control, test, paired) {
  return(effsize::cohen.d(test, control, paired = paired)$estimate)
}

# TODO Add documentation
hedges_correction <- function(x1, x2) {
  n1 <- length(x1)
  n2 <- length(x2)

  deg.freedom <- n1 + n2 - 2
  numer <- gamma(deg.freedom / 2)
  denom0 <- gamma((deg.freedom - 1) / 2)
  denom <- sqrt((deg.freedom / 2)) * denom0

  if (is.infinite(numer) | is.infinite(denom)) {
    # Occurs when df is too large.
    # Applies Hedges and Olkin's approximation.
    df.sum <- n1 + n2
    denom <- (4 * df.sum) - 9
    out <- 1 - (3 / denom)
  } else {
    out <- numer / denom
  }

  return(out)
}

# TODO Add documentation
effect_size_hedges_func <- function(control, test, paired) {
  cd <- cohens_d_(test, control, paired = paired)
  corr.factor <- -hedges_correction(test, control)
  return(cd * corr.factor)
}

# TODO Add documentation
effect_size_cliff_func <- function(control, test, paired = NA) {
  return(effsize::cliff.delta(test, control)$estimate)
}

# TODO Add documentation
effect_size_cohens_h_func <- function(control, test, paired) {
  # remove nas and nulls later on
  prop_control <- mean(control)
  prop_test <- mean(test)

  # Arcsine transformation
  phi_control <- 2 * asin(sqrt(prop_control))
  phi_test <- 2 * asin(sqrt(prop_test))
  result <- phi_test - phi_control
  return(result)
}

#' Calculating effect sizes
#'
#' @name effect_size
#'
#' @description
#' Computes the effect size for each control-test group pairing in `idx`.
#' The resampling bootstrap distribution of the effect size is then subjected
#' to Bias-corrected and accelerated bootstrap (BCa) correction.
#'
#' The following effect sizes `mean_diff`, `median_diff`, `cohens_d`, `hedges_g` and `cliffs_delta`
#' are used for most plot types.
#'
#' @param dabest_obj A dabest_obj created by loading in dataset along with other
#' specified parameters with the [load()] function.
#' @param perm_count The number of reshuffles of control and test labels to be performed for each p-value.
#'
#' @returns
#' Returns a `dabest_effectsize_obj` list with 22 elements. The following are the elements contained within:

#' - `raw_data` The tidy dataset passed to [load()] that was cleaned and altered for plotting.
#' - `idx` The list of control-test groupings as initially passed to [load()].
#' - `delta_x_labels` Vector containing labels for the x-axis of the delta plot.
#' - `delta_y_labels` String label for the y-axis of the delta plot.
#' - `Ns` List of labels for x-axis of the raw plot.
#' - `raw_y_labels` Vector containing labels for the y-axis of the raw plot.
#' - `is_paired` Boolean value determining if it is a paired plot.
#' - `is_colour` Boolean value determining if there is a colour column for the plot.
#' - `paired` Paired ("sequential" or "baseline") as initially passed to [load()].
#' - `resamples` The number of resamples to be used to generate the effect size bootstraps.
#' - `control_summary` Numeric value for plotting of control summary lines for float_contrast = `TRUE`.
#' - `test_summary` Numeric value for plotting of control summary lines for float_contrast = `TRUE`.
#' - `ylim` Vector containing the y limits for the raw plot.
#' - `enquo_x` Quosure of x as initially passed to [load()].
#' - `enquo_y` Quosure of y as initially passed to [load()].
#' - `enquo_id_col` Quosure of id_col as initially passed to [load()].
#' - `enquo_colour` Quosure of colour as initially passed to [load()].
#' - `proportional` Boolean value as initially passed to [load()].
#' - `minimeta` Boolean value as initially passed to [load()].
#' - `delta` Boolean value as initially passed to [load()].
#' - `proportional_data` List of calculations related to the plotting of proportion plots.
#' - `boot_result` List containing values related to the calculation of the effect sizes,
#' bootstrapping and BCa correction.
#' - `baseline_ec_boot_result` List containing values related to the calculation
#' of the effect sizes, bootstrapping and BCa correction for the baseline error
#' curve.
#' - `permtest_pvals` List containing values related to the calculations of permutation
#' t tests and the corresponding p values, and p values for different types of effect sizes
#' and different statistical tests.
#'
#' @details
#' The plot types listed under here are limited to use only the following effect sizes.
#' * Proportion plots offers only `mean_diff` and `cohens_h`.
#' * Mini-Meta Delta plots offers only `mean_diff`.
#'
#' The other plots are able to use all given basic effect sizes as listed in the Description.
#'
#' @examples
#' # Loading of the dataset
#' data(non_proportional_data)
#'
#' # Applying effect size to the dabest object
#' dabest_obj <- load(non_proportional_data,
#'   x = Group, y = Measurement,
#'   idx = c("Control 1", "Test 1")
#' )
#' dabest_obj.mean_diff <- mean_diff(dabest_obj)
#'
#' # Printing dabest effectsize object
#' print(dabest_obj.mean_diff)
#' @export
mean_diff <- function(dabest_obj, perm_count = 5000) {
  check_dabest_object(dabest_obj)

  effect_size_type <- "mean_diff"
  is_paired <- dabest_obj$is_paired
  reps <- dabest_obj$resamples

  if (is_paired) {
    main_results <- bootstrap(dabest_obj, effect_size_mean_func, boot_labs = "Paired\nmean difference", reps = reps)
  } else {
    main_results <- bootstrap(dabest_obj, effect_size_mean_func, boot_labs = "Mean difference", reps = reps)
  }
  permtest_and_pvalues <- Pvalues_statistics(dabest_obj,
    ef_size_fn = effect_size_mean_func,
    effect_size_type = effect_size_type,
    perm_count = perm_count
  )
  output <- c(main_results, list(effect_size_type = effect_size_type), permtest_and_pvalues)

  class(output) <- c("dabest_effectsize")

  return(output)
}

#' @rdname effect_size
#' @export
median_diff <- function(dabest_obj, perm_count = 5000) {
  check_dabest_object(dabest_obj)

  effect_size_type <- "median_diff"
  is_paired <- dabest_obj$is_paired
  reps <- dabest_obj$resamples

  if (is_paired) {
    main_results <- bootstrap(dabest_obj, effect_size_median_func, boot_labs = "Paired\nmedian difference", reps = reps)
  } else {
    main_results <- bootstrap(dabest_obj, effect_size_median_func, boot_labs = "Median difference", reps = reps)
  }

  permtest_and_pvalues <- Pvalues_statistics(dabest_obj,
    ef_size_fn = effect_size_median_func,
    effect_size_type = effect_size_type,
    perm_count = perm_count
  )
  output <- c(main_results, list(effect_size_type = effect_size_type), permtest_and_pvalues)

  class(output) <- c("dabest_effectsize")

  return(output)
}


#' @rdname effect_size
#' @export
cohens_d <- function(dabest_obj, perm_count = 5000) {
  check_dabest_object(dabest_obj)

  effect_size_type <- "cohens_d"

  reps <- dabest_obj$resamples

  main_results <- bootstrap(dabest_obj,
    effect_size_cohen_func,
    boot_labs = "Cohen's d",
    reps = reps
  )
  permtest_and_pvalues <- Pvalues_statistics(dabest_obj,
    ef_size_fn = effect_size_cohen_func,
    effect_size_type = effect_size_type,
    perm_count = perm_count
  )
  output <- c(main_results, list(effect_size_type = effect_size_type), permtest_and_pvalues)

  class(output) <- c("dabest_effectsize")

  return(output)
}

#' @rdname effect_size
#' @export
hedges_g <- function(dabest_obj, perm_count = 5000) {
  check_dabest_object(dabest_obj)

  effect_size_type <- "hedges_g"
  reps <- dabest_obj$resamples

  main_results <- bootstrap(dabest_obj,
    effect_size_hedges_func,
    boot_labs = "Hedges' g",
    reps = reps
  )
  permtest_and_pvalues <- Pvalues_statistics(dabest_obj,
    ef_size_fn = effect_size_hedges_func,
    effect_size_type = effect_size_type,
    perm_count = perm_count
  )
  output <- c(main_results, list(effect_size_type = effect_size_type), permtest_and_pvalues)

  class(output) <- c("dabest_effectsize")

  return(output)
}

#' @rdname effect_size
#' @export
cliffs_delta <- function(dabest_obj, perm_count = 5000) {
  check_dabest_object(dabest_obj)

  effect_size_type <- "cliffs_delta"

  reps <- dabest_obj$resamples

  main_results <- bootstrap(dabest_obj,
    effect_size_cliff_func,
    boot_labs = "Cliffs' delta",
    reps = reps
  )
  permtest_and_pvalues <- Pvalues_statistics(dabest_obj,
    ef_size_fn = effect_size_cliff_func,
    effect_size_type = effect_size_type,
    perm_count = perm_count
  )
  output <- c(main_results, list(effect_size_type = effect_size_type), permtest_and_pvalues)

  class(output) <- c("dabest_effectsize")

  return(output)
}

#' @rdname effect_size
#' @export
cohens_h <- function(dabest_obj, perm_count = 5000) {
  check_dabest_object(dabest_obj)

  effect_size_type <- "cohens_h"

  reps <- dabest_obj$resamples

  main_results <- bootstrap(dabest_obj,
    effect_size_cohens_h_func,
    boot_labs = "Cohen's h",
    reps = reps
  )
  permtest_and_pvalues <- Pvalues_statistics(dabest_obj,
    ef_size_fn = effect_size_cohens_h_func,
    effect_size_type = effect_size_type,
    perm_count = perm_count
  )
  output <- c(main_results, list(effect_size_type = effect_size_type), permtest_and_pvalues)

  class(output) <- c("dabest_effectsize")

  return(output)
}


#' Print a `dabest_effectsize_obj` object
#'
#' @noRd
#'
#' @param dabest_effectsize_obj a list object created by `effect_size()` functions
#' @param ... S3 signature for generic plot function.
#'
#' @return A summary of the effect sizes and respective confidence intervals.
#'
#' @examples
#' # Loading in of the dataset
#' data(twogroup_data)
#'
#' # Preparing the data to be plotted
#' dabest_obj <- load(twogroup_data,
#'   x = Group, y = Measurement,
#'   idx = c("Control1", "Group1")
#' )
#'
#' dabest_obj.mean_diff <- mean_diff(dabest_obj)
#'
#' # Display the results in a user-friendly format.
#' print(dabest_obj.mean_diff)
#'
#' @export
print.dabest_effectsize <- function(dabest_effectsize_obj, ...) {
  check_effectsize_object(dabest_effectsize_obj)

  print_greeting_header()

  es <- dabest_effectsize_obj$effect_size_type
  print_each_comparism_effectsize(dabest_effectsize_obj, es)
  print_ending(dabest_effectsize_obj)
}
