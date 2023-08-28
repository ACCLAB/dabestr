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

print_each_comparism <- function(dabest_object) {
  i <- 1
  if (is.list(dabest_object$idx)) {
    for (group in dabest_object$idx) {
      # Get test groups (everything else in group), loop through them and compute
      # the difference between group[1] and each group.
      # Test groups are the 2nd element of group onwards.

      control_group <- group[1]
      test_groups <- group[2:length(group)]

      for (current_test_group in test_groups) {
        cat(stringr::str_interp("${i}. ${current_test_group} minus ${control_group}\n"))
        i <- i + 1
      }
    }

    if (isTRUE(dabest_object$minimeta)) {
      cat(stringr::str_interp("${i}. weighted delta (only for mean difference)\n"))
      i <- i + 1
    }

    if (isTRUE(dabest_object$delta2)) {
      experiment1 <- dabest_object$experiment_label[2]
      experiment2 <- dabest_object$experiment_label[1]

      cat(stringr::str_interp("${i}. ${experiment1} minus ${experiment2} (only for mean difference)\n"))
    }
  } else {
    control_group <- dabest_object$idx[1]
    test_groups <- dabest_object$idx[2:length(dabest_object$idx)]

    for (current_test_group in test_groups) {
      cat(stringr::str_interp("  ${i}. ${current_test_group} minus ${control_group}\n"))
      i <- i + 1
    }
  }
  cat("\n")
}

print_each_comparism_effectsize <- function(dabest_object, effectsize) {
  if (effectsize == "mean_diff") {
    es <- "mean difference"
  } else if (effectsize == "median_diff") {
    es <- "median difference"
  } else if (effectsize == "cohens_d") {
    es <- "Cohen's d"
  } else if (effectsize == "hedges_g") {
    es <- "Hedges'g"
  } else if (effectsize == "cliffs_delta") {
    es <- "Cliff's delta"
  } else {
    es <- "Cohen's h"
  }

  i <- 1
  pvalue_index <- 1
  paired <- dabest_object$paired
  difference <- round(dabest_object$boot_result$difference, 3)
  bca_low <- round(dabest_object$boot_result$bca_ci_low, 3)
  bca_high <- round(dabest_object$boot_result$bca_ci_high, 3)
  ci <- dabest_object$boot_result$ci
  pvalue <- dabest_object$permtest_pvals$pvalues

  if (is.null(paired)) {
    rm_status <- ""
  } else if (paired == "sequential") {
    rm_status <- "for the sequential design of repeated-measures experiment \n"
  } else if (paired == "baseline") {
    rm_status <- "for repeated measures against baseline \n"
  }

  if (is.null(paired)) {
    paired_status <- "unpaired"
  } else if (paired == "sequential") {
    paired_status <- "paired"
  } else if (paired == "baseline") {
    paired_status <- "paired"
  }

  if (is.list(dabest_object$idx)) {
    for (group in dabest_object$idx) {
      # Get test groups (everything else in group), loop through them and compute
      # the difference between group[1] and each group.
      # Test groups are the 2nd element of group onwards.

      control_group <- group[1]
      test_groups <- group[2:length(group)]

      for (current_test_group in test_groups) {
        current_paired <- paired[i]
        current_difference <- difference[i]
        current_bca_low <- bca_low[i]
        current_bca_high <- bca_high[i]
        current_ci <- ci[i]
        current_pval <- pvalue[[i]][pvalue_index]

        cat(stringr::str_interp("The ${paired_status} ${es} between ${current_test_group} and ${control_group} is ${current_difference} [${current_ci}%CI ${current_bca_low}, ${current_bca_high}].\n"))
        cat(stringr::str_interp("The p-value of the two-sided permutation t-test is ${sprintf(current_pval, fmt = '%#.4f')}, calculated for legacy purposes only."))
        cat("\n\n")
        i <- i + 1
      }
    }
  } else {
    control_group <- dabest_object$idx[1]
    test_groups <- dabest_object$idx[2:length(dabest_object$idx)]

    for (current_test_group in test_groups) {
      cat(stringr::str_interp("The ${paired_status} ${es} between ${current_test_group} and ${control_group} is ${difference} [${ci}%CI ${bca_low}, ${bca_high}].\n"))
      cat(stringr::str_interp("The p-value of the two-sided permutation t-test is ${sprintf(current_pval, fmt = '%#.4f')}, calculated for legacy purposes only.\n"))
    }
  }
}

print_ending <- function(dabest_object) {
  if (class(dabest_object) == "dabest") {
    nboots <- dabest_object$resamples
    cat(stringr::str_interp("${nboots} resamples will be used to generate the effect size bootstraps.\n\n"))
  } else {
    nboots <- dabest_object$resamples
    nreshuffles <- length(dabest_object$permtest_pvals$permutation_test_results[[1]]$permutations)
    cat(stringr::str_interp("${nboots} bootstrap samples were taken; the confidence interval is bias-corrected and accelerated.\n"))
    cat("Any p-value reported is the probability of observing the effect size (or greater),\n")
    cat("assuming the null hypothesis of zero difference is true.\n")
    cat(stringr::str_interp("For each p-value, ${nreshuffles} reshuffles of the control and test labels were performed.\n"))
    cat("\n")
  }
}
