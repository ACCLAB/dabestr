

bootdiff <- function(data, control, test, paired, ci = 0.95, reps = 5000,
                     func = mean) {
  #' Difference between two groups with bootstrap confidence intervals
  #'
  #' \code{bootdiff} returns the difference between 2 groups.
  #'
  #' @param data A data.frame or list.
  #' @param control A column or item in `data`. This is the control group.
  #' @param test A column or item in `data`. This is the test group.
  #' @param paired boolean. If TRUE, the two groups are treated as paired
  #' samples. The \code{control} group is treated as pre-intervention
  #' and the \code{control} group is considered post-intervention.
  #' @param ci float, default 0.95. The level of the confidence intervals
  #' produced. The default \code{ci = 0.95} produces 95 percent CIs.
  #' @param reps integer, default 5000. The number of bootstrap resamples that
  #' will be generated.
  #' @param func function, default mean. This function will be applied to
  #' \code{control} and \code{test} individually, and the difference will be
  #' saved as a single bootstrap resample.
  #'
  #' @return A list of the class "boot.diff", with the following 9 elements:
  #'
  #' \item{func}{
  #' The \code{func} passed to \code{bootdiff}.
  #' }
  #' \item{difference}{
  #' The difference between the two groups;
  #' effectively \code{func(test) - func(control)}.
  #' }
  #' \item{ci}{
  #' The \code{ci} passed to the \code{bootdiff}.
  #' }
  #' \item{bca_ci_low}{
  #' The lower limit of the Bias Corrected and Accelerated bootstrap confidence
  #' interval.}
  #'
  #' \item{bca_ci_high}{
  #'   The upper limit of the Bias Corrected and Accelerated bootstrap confidence
  #'   interval.}
  #'
  #' \item{pct_ci_low}{
  #'   The upper limit of the percentile bootstrap confidence interval.
  #'   }
  #' \item{pct_ci_high}{
  #'   The lower limit of the percentile bootstrap confidence interval.
  #'   }
  #'
  #' \item{bootstraps}{
  #'   The array of bootstrap resamples generated.}
  #'
  #' @examples
  #' # Performing unpaired (two independent groups) analysis.
  #' # We will analyse the `wellbeing_ind` dataset that comes with dabestr.
  #' unpaired_mean_diff <- bootdiff(data = wellbeing_ind,
  #'                                control = "control", test = "test",
  #'                                paired = FALSE)
  #'
  #' # Display the results in a user-friendly format.
  #' unpaired_mean_diff
  #'
  #' # Plot the bootstrap resamples as a histogram, along with the CI.
  #' plot(unpaired_mean_diff)
  #'
  #'
  #' # Performing paired analysis.
  #' # We will demonstrate this with the paired dataset `wellbeing_ind`.
  #' paired_mean_diff <- bootdiff(data = wellbeing_paired,
  #'                              control = "before", test = "after",
  #'                              paired = TRUE)
  #'
  #'
  #' # Computing the median difference.
  #' unpaired_median_diff <- bootdiff(data = wellbeing_ind,
  #'                                 control = "control", test = "test",
  #'                                 paired = FALSE, func = median)
  #'
  #'
  #' # Producing a 90 percent CI instead of 95 percent.
  #' unpaired_mean_diff_90_ci <- bootdiff(data = wellbeing_ind,
  #'                                      control = "control", test = "test",
  #'                                      paired = FALSE, ci = 0.90)
  #'
  #'
  #' # Constructing the confidence intervals on 10000 bootstrap resamples.
  #' unpaired_mean_diff_n10000 <- bootdiff(data = wellbeing_ind,
  #'                                       control = "control", test = "test",
  #'                                       paired = FALSE, reps = 10000)
  #'
  #' @section References:
  #' DiCiccio, Thomas J., and Bradley Efron. Bootstrap Confidence Intervals.
  #'   Statistical Science: vol. 11, no. 3, 1996, pp. 189–228,
  #'   http://www.jstor.org/stable/2246110.
  #'
  #' Efron, Bradley, and R. J. Tibshirani. An Introduction to the Bootstrap.
  #'   CRC Press, 1994.
  #'   https://www.crcpress.com/An-Introduction-to-the-Bootstrap/Efron-Tibshirani/p/book/9780412042317

  c <- data[[control]]
  t <- data[[test]]

  if (identical(paired, FALSE)) {
    diff <- func(t) - func(c)
    # For two.boot, note that the first vector is the test vector.
    boot <- simpleboot::two.boot(t, c, FUN = func, R = reps)

  } else {
    if (length(c) != length(t)) {
      stop("The two groups are not the same size, but paired = TRUE.")
    }
    paired_diff <- t - c
    diff <- func(paired_diff)
    boot <- simpleboot::one.boot(paired_diff, FUN = func, R = reps)
  }

  bootci <- boot::boot.ci(boot, conf = ci, type = c("perc", "bca"))

  result = list()

  # convert the name of `func` to a string.
  result$func = as.character(substitute(func))
  result$paired = paired
  result$difference = diff
  result$ci = ci
  result$bca_ci_low = bootci$bca[4]
  result$bca_ci_high = bootci$bca[5]
  result$pct_ci_low = bootci$percent[4]
  result$pct_ci_high = bootci$percent[5]
  result$bootstraps = boot$t

  class(result) <- "boot.diff"
  result

}



print.boot.diff <- function(result, ...) {
  #  Print the intervals
  cat("Effect Size with Bootstrap Confidence Intervals\n")
  cat("-----------------------------------------------\n\n")

  cat(paste("Using", length(result$bootstraps), "bootstrap replicates"))
  cat(sprintf(" and the function %s().\n\n", result$func))

  if (identical(result$paired, TRUE)) p <- "Paired" else p <- "Unpaired"

  cat(sprintf("%s Difference with BCa Confidence Intervals:\n", p))
  cat(sprintf("%.3f [%iCI %.2f, %.2f]\n",
          result$difference, result$ci * 100,
          result$bca_ci_low, result$bca_ci_high))

  cat(sprintf("%s Difference with Percentile Confidence Intervals:\n", p))
  cat(sprintf("%.3f [%iCI %.2f, %.2f]\n",
          result$difference, result$ci * 100,
          result$pct_ci_low, result$pct_ci_high))
}



plot.boot.diff <- function(result, ...) {
  bootstrap_resamples = result$bootstraps
  hist(bootstrap_resamples, ...)
  # Draw the mean difference.
  abline(v = result$difference, col='red')

  # Draw the CIs.
  abline(v = result$bca_ci_low)
  abline(v = result$bca_ci_high)
}
