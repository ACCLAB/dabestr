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
pvals_statistics <- function(control,
                             test,
                             is_paired,
                             proportional,
                             effect_size) {
  pvals_stats <- list()
  if (isTRUE(is_paired) && !proportional) {
    # Wilcoxon test (non-parametric version of the paired T-test)
    wilcoxon <- stats::wilcox.test(control, test)
    pvalue_wilcoxon <- wilcoxon$p.value
    statistic_wilcoxon <- wilcoxon$statistic

    paired_t <- NA
    pvalue_paired_students_t <- NA
    statistic_paired_students_t <- NA

    if (effect_size != "median_diff") {
      # Paired Student's t-test
      paired_t <- stats::t.test(control, test, paired = TRUE, na.rm = TRUE)
      pvalue_paired_students_t <- paired_t$p.value
      statistic_paired_students_t <- paired_t$statistic
    }
    pvals_stats <- list(
      pvalue_wilcoxon = pvalue_wilcoxon,
      wilcoxon = wilcoxon,
      statistic_wilcoxon = statistic_wilcoxon,
      paired_t = paired_t,
      pvalue_paired_students_t = pvalue_paired_students_t,
      statistic_paired_students_t = statistic_paired_students_t
    )
  } else if (isTRUE(is_paired) && proportional) {
    # McNemar's test for binary paired data
    table <- matrix(
      c(
        sum(control == 0 & test == 0), sum(control == 0 & test == 1),
        sum(control == 1 & test == 0), sum(control == 1 & test == 1)
      ),
      nrow = 2, byrow = TRUE
    )
    mcnemar_result <- stats::mcnemar.test(table, correct = TRUE)
    pvalue_mcnemar <- mcnemar_result$p.value
    statistic_mcnemar <- mcnemar_result$statistic
    pvals_stats <- list(
      pvalue_mcnemar = pvalue_mcnemar,
      statistic_mcnemar = statistic_mcnemar
    )
  } else if (effect_size == "cliffs_delta") {
    # Brunner-Munzel test
    brunner_munzel <- brunnermunzel::brunnermunzel.test(control, test, na.rm = TRUE)
    pvalue_brunner_munzel <- brunner_munzel$p.value
    statistic_brunner_munzel <- brunner_munzel$statistic
    pvals_stats <- list(
      pvalue_brunner_munzel = pvalue_brunner_munzel,
      statistic_brunner_munzel = statistic_brunner_munzel
    )
  } else if (effect_size == "median_diff") {
    # Kruskal-Wallis H-test
    kruskal <- stats::kruskal.test(list(control, test))
    pvalue_kruskal <- kruskal$p.value
    statistic_kruskal <- kruskal$statistic
    pvals_stats <- list(
      pvalue_kruskal = pvalue_kruskal,
      statistic_kruskal = statistic_kruskal
    )
  } else {
    # For mean difference, Cohen's d, and Hedges' g
    # Welch's t-test (equal_var = FALSE) to not assume equal variances
    welch <- stats::t.test(control, test, equal.var = FALSE, na.rm = TRUE)
    pvalue_welch <- welch$p.value
    statistic_welch <- welch$statistic

    # Student's t-test (equal_var = TRUE) to assume equal variances
    students_t <- stats::t.test(control, test, equal.var = TRUE, na.rm = TRUE)
    pvalue_students_t <- students_t$p.value
    statistic_students_t <- students_t$statistic

    # Mann-Whitney test: non-parametric, does not assume normality of distributions
    tryCatch(
      {
        mann_whitney <- stats::wilcox.test(control, test, alternative = "two.sided")
        pvalue_mann_whitney <- mann_whitney$p.value
        statistic_mann_whitney <- mann_whitney$statistic
      },
      error = function(e) {
        # Occurs when the control and test are exactly identical in terms of rank (e.g., all zeros).
        pvalue_mann_whitney <- NA
        statistic_mann_whitney <- NA
      }
    )

    standardized_es <- effsize::cohen.d(control, test, is_paired = NULL)

    # Cohen's h calculation for binary categorical data
    if (isTRUE(proportional)) {
      tryCatch(
        {
          cohens_h_cal <- function(control, test) {
            # remove nas and nulls later on
            prop_control <- mean(control)
            prop_test <- mean(test)

            # Arcsine transformation
            phi_control <- 2 * asin(sqrt(prop_control))
            phi_test <- 2 * asin(sqrt(prop_test))
            result <- phi_test - phi_control
            return(result)
          }
          proportional_difference <- cohens_h_cal(control, test)
        },
        error = function(e) {
          # Occur only when the data consists not only 0's and 1's.
          proportional_difference <- NA
        }
      )
    }

    pvals_stats <- list(
      pvalue_welch = pvalue_welch,
      statistic_welch = statistic_welch,
      # Student's t-test (equal_var = TRUE) to assume equal variances
      students_t = students_t,
      pvalue_students_t = pvalue_students_t,
      statistic_students_t = statistic_students_t,
      # Mann-Whitney test: non-parametric, does not assume normality of distributions

      pvalue_mann_whitney = pvalue_mann_whitney,
      statistic_mann_whitney = statistic_mann_whitney
    )
  }

  return(pvals_stats)
}
#' Generates collated permutaion test results and statistical test results.
#'
#' This function returns a tibble (list) that includes statistical test results:
#' its corresponding statistics and p values.
#'
#' @param dabest_object A "dabest_obj" list created by loading in dataset along with other
#' specified parameters with the [load()] function.
#' @param seed Integer specifying random seed that will be passed to the
#' [PermutationTest()] function.
#' @param permutation_count Integer value specifying the number of permutations
#'  being carried out in the [PermutationTest()] function.
#' @param ef_size_fn The effect size function passed to [PermutationTest()] that
#'  help calculate the specific type of effect size.
#' @param effect_size_type String. Any one of the following are accepted inputs:
#' 'mean_diff', 'median_diff', 'cohens_d', 'hedges_g', or 'cliffs_delta'.
#'
#' @returns Tibble for statistical test and permutation test results for
#' all pairs of control and test datasets based on the experimental design
#' initially specified when passed to the [load()] function.
#' @noRd
Pvalues_statistics <- function(dabest_object,
                               seed = 12345,
                               perm_count = 5000,
                               ef_size_fn = NULL,
                               effect_size_type) {
  permtest_pvals <- tibble::tibble()

  # check if effect size function is supplied
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
        test_group <- test_group
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
        # calculate p values
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
