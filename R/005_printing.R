# TODO add proper documentation
print_greeting_header <- function() {
  dabest_ver <- utils::packageVersion("dabestr")
  line1 <- paste("DABESTR v", dabest_ver, sep = "")

  now <- Sys.time()

  now_hour <- as.integer(format(now, "%H"))

  if (now_hour < 12) {
    greeting <- "Good morning!"
  } else if (now_hour >= 12 & now_hour < 18) {
    greeting <- "Good afternoon!"
  } else {
    greeting <- "Good evening!"
  }

  curr_time <- paste("The current time is", format(now, "%R %p on %A %B %d, %Y."))

  cat(line1)
  cat("\n")
  cat(rep("=", nchar(line1)), sep = "")
  cat("\n\n")
  cat(greeting, curr_time, sep = "\n")
  cat("\n")
}

# TODO add proper documentation
print_each_comparism <- function(dabest_obj) {
  check_dabest_object(dabest_obj)

  i <- 1
  if (is.list(dabest_obj$idx)) {
    for (group in dabest_obj$idx) {
      # Get test groups (everything else in group), loop through them and compute
      # the difference between group[1] and each group.
      # Test groups are the 2nd element of group onwards.
      control_group <- group[1]
      test_groups <- group[2:length(group)]

      if (is.null(dabest_obj$paired) || dabest_obj$paired == "baseline") {
        control_group <- group[1]
        test_groups <- group[2:length(group)]
        for (current_test_group in test_groups) {
          cat(stringr::str_interp("${i}. ${current_test_group} minus ${control_group}\n"))
          i <- i + 1
        }
      } else {
        for (n in 1:(length(group) - 1)) {
          current_group <- group[n + 1]
          previous_group <- group[n]
          cat(stringr::str_interp("${i}. ${current_group} minus ${previous_group}\n"))
          i <- i + 1
        }
      }
    }

    if (dabest_obj$minimeta) {
      cat(stringr::str_interp("${i}. weighted delta (only for mean difference)\n"))
      i <- i + 1
    }

    if (dabest_obj$delta2) {
      experiment1 <- dabest_obj$experiment_label[2]
      experiment2 <- dabest_obj$experiment_label[1]

      cat(stringr::str_interp("${i}. ${experiment1} minus ${experiment2} (only for mean difference)\n"))
    }
  } else {
    control_group <- dabest_obj$idx[1]
    test_groups <- dabest_obj$idx[2:length(dabest_obj$idx)]

    for (current_test_group in test_groups) {
      cat(stringr::str_interp("  ${i}. ${current_test_group} minus ${control_group}\n"))
      i <- i + 1
    }
  }
  cat("\n")
}

# TODO add proper documentation
print_each_comparism_effectsize <- function(dabest_effectsize_obj, effectsize) {
  es_lookup <- c(
    "mean_diff" = "mean difference",
    "median_diff" = "median difference",
    "cohens_d" = "Cohen's d",
    "hedges_g" = "Hedges'g",
    "cliffs_delta" = "Cliff's delta"
  )
  tryCatch(
    {
      es <- es_lookup[effectsize]
    },
    error = function(e) {
      # default value
      es <- "Cohen's h"
    }
  )

  check_effectsize_object(dabest_effectsize_obj)
  i <- 1
  paired <- dabest_effectsize_obj$paired
  difference <- round(dabest_effectsize_obj$boot_result$difference, 3)
  bca_low <- round(dabest_effectsize_obj$boot_result$bca_ci_low, 3)
  bca_high <- round(dabest_effectsize_obj$boot_result$bca_ci_high, 3)
  ci <- dabest_effectsize_obj$boot_result$ci
  pvalue <- dabest_effectsize_obj$permtest_pvals$pval_for_tests

  # Use a lookup table for rm_status and paired_status
  rm_status_lookup <- c(NULL = "", "sequential" = "for the sequential design of repeated-measures experiment \n", "baseline" = "for repeated measures against baseline \n")
  paired_status_lookup <- c(NULL = "unpaired", "sequential" = "paired", "baseline" = "paired")

  rm_status <- rm_status_lookup[[format(paired)]] # make sure even NULL gets converted to string
  paired_status <- paired_status_lookup[[format(paired)]] # make sure even NULL gets converted to string

  if (is.list(dabest_effectsize_obj$idx)) {
    for (group in dabest_effectsize_obj$idx) {
      # Get test groups (everything else in group), loop through them and compute
      # the difference between group[1] and each group.
      # Test groups are the 2nd element of group onwards.

      control_group <- group[1]
      test_groups <- group[2:length(group)]

      if (is.null(dabest_effectsize_obj$paired) || dabest_effectsize_obj$paired == "baseline") {
        control_group <- group[1]
        test_groups <- group[2:length(group)]
        for (current_test_group in test_groups) {
          current_paired <- paired[i]
          current_difference <- difference[i]
          current_bca_low <- bca_low[i]
          current_bca_high <- bca_high[i]
          current_ci <- ci[i]
          current_pval <- pvalue[i]

          cat(stringr::str_interp("The ${paired_status} ${es} between ${current_test_group} and ${control_group} is ${current_difference}, ${current_ci}% CI [${current_bca_low}, ${current_bca_high}].\n"))
          cat(stringr::str_interp("The p-value of the two-sided permutation t-test is ${sprintf(current_pval, fmt = '%#.4f')}, calculated for legacy purposes only."))
          cat("\n\n")
          i <- i + 1
        }
      } else {
        for (n in 1:(length(group) - 1)) {
          current_group <- group[n + 1]
          previous_group <- group[n]
          current_paired <- paired[i]
          current_difference <- difference[i]
          current_bca_low <- bca_low[i]
          current_bca_high <- bca_high[i]
          current_ci <- ci[i]
          current_pval <- pvalue[i]

          cat(stringr::str_interp("The ${paired_status} ${es} between ${current_group} and ${previous_group} is ${current_difference}, ${current_ci}% CI [${current_bca_low}, ${current_bca_high}].\n"))
          cat(stringr::str_interp("The p-value of the two-sided permutation t-test is ${sprintf(current_pval, fmt = '%#.4f')}, calculated for legacy purposes only."))
          cat("\n\n")
          i <- i + 1
        }
      }
    }
  } else {
    control_group <- dabest_effectsize_obj$idx[1]
    test_groups <- dabest_effectsize_obj$idx[2:length(dabest_effectsize_obj$idx)]

    for (current_test_group in test_groups) {
      cat(stringr::str_interp("The ${paired_status} ${es} between ${current_test_group} and ${control_group} is ${difference}, ${ci}% CI [${bca_low}, ${bca_high}].\n"))
      cat(stringr::str_interp("The p-value of the two-sided permutation t-test is ${sprintf(current_pval, fmt = '%#.4f')}, calculated for legacy purposes only.\n"))
    }
  }
}

# TODO add proper documentation. If the parameter can be sth that is not a dabest_object maybe change the name
print_ending <- function(dabest_obj) {
  if (inherits(dabest_obj, "dabest")) {
    nboots <- dabest_obj$resamples
    cat(stringr::str_interp("${nboots} resamples will be used to generate the effect size bootstraps.\n\n"))
  } else {
    nboots <- dabest_obj$resamples
    nreshuffles <- length(dabest_obj$permtest_pvals$permutation_test_results[[1]]$permutations)
    cat(stringr::str_interp("${nboots} bootstrap samples were taken; the confidence interval is bias-corrected and accelerated.\n"))
    cat("Any p-value reported is the probability of observing the effect size (or greater),\n")
    cat("assuming the null hypothesis of zero difference is true.\n")
    cat(stringr::str_interp("For each p-value, ${nreshuffles} reshuffles of the control and test labels were performed.\n"))
    cat("\n")
  }
}
