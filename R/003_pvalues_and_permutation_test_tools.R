#' Generates permutation test results.
#'
#' This function returns a list that include permutations results:
#' its corresponding permutations, variance of permutations, p value and effect size
#' (depending on the effect size).
#'
#' @param control Vector, the control group data.
#' @param test Vector, the test group data.
#' @param effect_size String. Any one of the following are accepted inputs:
#' 'mean_diff', 'median_diff', 'cohens_d', 'hedges_g', or 'cliffs_delta'.
#' @param is_paired Boolean value as initially passed to [load()].
#' @param permutation_count Integer value specifying the number of permutations being carried out.
#' @param random_seed Integer value specifying the random seed for permutations to be carried out.
#' @param ef_size_fn A function that calculates the specific type of effect size.
#'
#' @returns a list for permutation test results for a pair of control and test data points.
#' @noRd
PermutationTest <- function(control,
                            test,
                            effect_size,
                            is_paired,
                            permutation_count = 5000,
                            random_seed = 12345,
                            ef_size_fn) {
  # Check if the arrays have the same length for paired test
  if (isTRUE(is_paired) && length(control) != length(test)) {
    stop("The two arrays do not have the same length.")
  }

  # Initialize random number generator
  set.seed(random_seed)

  # Set required constants and variables
  control <- as.numeric(control)
  test <- as.numeric(test)

  control_sample <- control
  test_sample <- test

  BAG <- c(control, test)
  CONTROL_LEN <- length(control)
  EXTREME_COUNT <- 0
  THRESHOLD <- abs(ef_size_fn(control, test, is_paired))
  permutations <- vector("numeric", length = permutation_count)
  permutations_var <- vector("numeric", length = permutation_count)

  for (i in 1:permutation_count) {
    if (isTRUE(is_paired)) {
      # Select which control-test pairs to swap.
      random_idx <- sample(1:CONTROL_LEN,
        size = sample(1:CONTROL_LEN, 1),
        replace = TRUE
      )

      # Perform swap.
      for (idx in random_idx) {
        placeholder <- control_sample[idx]
        control_sample[idx] <- test_sample[idx]
        test_sample[idx] <- placeholder
      }
    } else {
      # Shuffle the bag and assign to control and test groups.
      shuffled <- sample(BAG)
      control_sample <- shuffled[1:CONTROL_LEN]
      test_sample <- shuffled[(CONTROL_LEN + 1):length(shuffled)]
    }

    es <- ef_size_fn(control_sample, test_sample, is_paired)

    control_var <- stats::var(control_sample, na.rm = TRUE)
    test_var <- stats::var(test_sample, na.rm = TRUE)
    control_N <- length(control_sample)
    test_N <- length(test_sample)
    var <- calculate_group_variance(control_var, control_N, test_var, test_N)

    permutations[i] <- es
    permutations_var[i] <- var

    if (abs(es) > THRESHOLD) {
      EXTREME_COUNT <- EXTREME_COUNT + 1
    }
  }

  pvalue <- EXTREME_COUNT / permutation_count

  perm_results <- list(
    permutations = permutations,
    permutations_var = permutations_var,
    pvalue = pvalue,
    es = es
  )

  return(perm_results)
}
#' Generates statistical test results for possible hypothesis testings.
#'
#' This function returns a list that include statistical test results:
#' its corresponding statistics and p values
#'
#' @param control Vector, the control group data.
#' @param test Vector, the test group data.
#' @param is_paired Boolean value as initially passed to [load()].
#' @param proportional Boolean value as initially passed to [load()].
#' @param effect_size String. Any one of the following are accepted inputs:
#' 'mean_diff', 'median_diff', 'cohens_d', 'hedges_g', or 'cliffs_delta'.
#'
#' @returns a list for statistical test results and p values for the
#' corresponding tests of a pair of control and test data points.
#' @noRd
Pvalues_statistics <- function(dabest_object,
                               seed = 12345,
                               perm_count = 5000,
                               ef_size_fn = NULL,
                               effect_size_type) {
  permtest_pvals <- tibble::tibble()

  if (is.null(ef_size_fn)) {
    stop("No effect size calculation methods are supplied.")
  }

  raw_data <- dabest_object$raw_data
  idx <- dabest_object$idx

  if (isFALSE(is.list(idx))) {
    idx <- list(idx)
  }
  enquo_x <- dabest_object$enquo_x
  enquo_y <- dabest_object$enquo_y
  ci <- dabest_object$ci
  paired <- dabest_object$paired
  is_paired <- dabest_object$is_paired

  proportional <- dabest_object$proportional

  quoname_x <- rlang::as_name(enquo_x)
  quoname_y <- rlang::as_name(enquo_y)

  minimeta <- dabest_object$minimeta
  delta2 <- dabest_object$delta2

  if (isFALSE(is_paired) || isTRUE(paired == "baseline")) {
    for (group in idx) {
      control_group <- group[1]
      group_length <- length(group)

      ctrl_tibble <- raw_data %>%
        dplyr::filter(!!enquo_x == !!group[1])
      ctrl_measurement <- ctrl_tibble[[quoname_y]]

      tests <- group[2:group_length]

      for (test_group in tests) {
        test_tibble <- raw_data %>%
          dplyr::filter(!!enquo_x == !!test_group)
        test_measurement <- test_tibble[[quoname_y]]

        xlabels <- paste(test_group, group[1], sep = "\nminus\n")

        test_size <- length(test_measurement)

        es <- ef_size_fn(ctrl_measurement, test_measurement, paired = is_paired)

        # do permutation tests accordingly
        PermutationTest_result <- PermutationTest(ctrl_measurement,
          test_measurement,
          effect_size = effect_size_type,
          is_paired = is_paired,
          permutation_count = perm_count,
          random_seed = 12345,
          ef_size_fn = ef_size_fn
        )

        # calculate p values
        # If minimeta is TRUE, perform minimeta permutation test
        if (isTRUE(minimeta)) {
          permutations <- PermutationTest_result$permutations
          permutations_var <- PermutationTest_result$permutations_var
          permutations_weighted_delta <- calculate_minimeta(permutations, permutations_var)
          
          threshold <- abs(es)
          pvalue_minimeta <- calculate_minimeta_pvalue(permutations_weighted_delta, threshold, perm_count)
          
          PermutationTest_result$pvalue <- pvalue_minimeta
          PermutationTest_result$weighted_delta <- permutations_weighted_delta
        }

        pvals_and_stats <- pvals_statistics(ctrl_measurement,
          test_measurement,
          is_paired = is_paired,
          proportional = proportional,
          effect_size = effect_size_type
        )

        pval_row <- list(
          control_group = control_group,
          test_group = test_group,
          ef_sz_real = es,
          pval_permtest = PermutationTest_result$pvalue,
          permutation_test_results = list(PermutationTest_result),
          pval_for_tests = pvals_and_stats[1],
          pvalues = list(pvals_and_stats)
        )

        permtest_pvals <- dplyr::bind_rows(permtest_pvals, pval_row)
      }
    }
  } else {
    for (group in idx) {
      group_length <- length(group)
      for (i in 1:(group_length - 1)) {
        control_group <- group[i]
        test_group <- group[i + 1]

        ctrl_tibble <- raw_data %>%
          dplyr::filter(!!enquo_x == !!control_group)
        ctrl_measurement <- ctrl_tibble[[quoname_y]]

        test_tibble <- raw_data %>%
          dplyr::filter(!!enquo_x == !!test_group)
        test_measurement <- test_tibble[[quoname_y]]
        xlabels <- paste(test_group, control_group, sep = "\nminus\n")

        control_test_measurement <- list(
          control = ctrl_measurement,
          test = test_measurement
        )
        # add weights column
        ctrl_size <- length(ctrl_measurement)
        ctrl_var <- var_w_df(ctrl_measurement, ctrl_size)
        test_size <- length(test_measurement)
        test_var <- var_w_df(test_measurement, test_size)
        grp_var <- calculate_group_variance(
          ctrl_var = ctrl_var,
          ctrl_N = ctrl_size,
          test_var = test_var,
          test_N = test_size
        )

        es <- ef_size_fn(ctrl_measurement, test_measurement, paired)

        # do permutation tests accordingly
        PermutationTest_result <- PermutationTest(ctrl_measurement,
          test_measurement,
          effect_size = effect_size_type,
          is_paired = is_paired,
          permutation_count = perm_count,
          random_seed = 12345,
          ef_size_fn = ef_size_fn
        )

        # If minimeta is TRUE, perform minimeta permutation test
        if (isTRUE(minimeta)) {
          permutations <- PermutationTest_result$permutations
          permutations_var <- PermutationTest_result$permutations_var
          permutations_weighted_delta <- calculate_minimeta(permutations, permutations_var)
          
          threshold <- abs(es)
          pvalue_minimeta <- calculate_minimeta_pvalue(permutations_weighted_delta, threshold, perm_count)
          
          PermutationTest_result$pvalue <- pvalue_minimeta
          PermutationTest_result$weighted_delta <- permutations_weighted_delta
        }

        pvals_and_stats <- pvals_statistics(ctrl_measurement,
          test_measurement,
          is_paired = is_paired,
          proportional = proportional,
          effect_size = effect_size_type
        )

        pval_row <- list(
          control_group = control_group,
          test_group = test_group,
          ef_sz_real = es,
          pval_permtest = PermutationTest_result$pvalue,
          permutation_test_results = list(PermutationTest_result),
          pval_for_tests = pvals_and_stats[1],
          pvalues = list(pvals_and_stats)
        )

        permtest_pvals <- dplyr::bind_rows(permtest_pvals, pval_row)
      }
    }
  }

  return(list(permtest_pvals = permtest_pvals))
}

#' Calculate Weighted Delta for Mini-Meta Analysis
#'
#' This function calculates the weighted delta across multiple groups for a mini-meta analysis.
#' The weights are determined by the inverse of the variance of the permutations for each group.
#' The function returns the weighted average delta for each permutation.
#'
#' @param permutations A matrix where each row represents a group, and each column represents a permutation.
#' @param permutations_var A matrix of the same dimensions as `permutations`, containing the variances of each group for each permutation.
#'
#' @return A numeric vector representing the weighted delta for each permutation.
#' @noRd
calculate_minimeta <- function(permutations, permutations_var) {
  all_num <- numeric(ncol(permutations))
  all_denom <- numeric(ncol(permutations))
  
  groups <- nrow(permutations)
  
  for (i in seq_len(ncol(permutations))) {
    weight <- sapply(seq_len(groups), function(j) 1/permutations_var[j, i])
    all_num[i] <- sum(weight * permutations[, i])
    all_denom[i] <- sum(weight)
  }
  
  output <- all_num / all_denom
  return(output)
}

#' Calculate P-value for Weighted Delta in Mini-Meta Analysis
#'
#' This function calculates the p-value for the weighted delta in a mini-meta analysis.
#' The p-value is computed based on the number of weighted deltas that exceed a given threshold.
#'
#' @param permutations_weighted_delta A numeric vector of weighted deltas for each permutation.
#' @param threshold A numeric value representing the threshold for significance.
#' @param permutation_count An integer representing the total number of permutations performed.
#'
#' @return A numeric value representing the p-value.
#' @noRd
calculate_minimeta_pvalue <- function(permutations_weighted_delta, threshold, permutation_count) {
  count <- sum(abs(permutations_weighted_delta) > threshold)
  pvalue <- count / permutation_count
  return(pvalue)
}
