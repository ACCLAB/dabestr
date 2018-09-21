

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
  #' @return A list with the following 8 elements:
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
  #'
  #' # Performing paired analysis.
  #' # We will demonstrate this with the paired dataset `wellbeing_ind`.
  #' unpaired_mean_diff <- bootdiff(data = wellbeing_paired,
  #'                                control = "control", test = "test",
  #'                                paired = TRUE)
  #'
  #'
  #' # Computing the median difference.
  #' unpaired_median_diff <- bootdiff(data = wellbeing_paired,
  #'                                 control = "control", test = "test",
  #'                                 paired = FALSE, func = median)
  #'
  #'
  #' # Producing a 90 percent CI instead of 95 percent.
  #' unpaired_mean_diff_90_ci <- bootdiff(data = wellbeing_paired,
  #'                                      control = "control", test = "test",
  #'                                      paired = FALSE, ci = 0.90)
  #'
  #'
  #' # Constructing the confidence intervals on 10000 bootstrap resamples.
  #' unpaired_mean_diff_n10000 <- bootdiff(data = wellbeing_paired,
  #'                                       control = "control", test = "test",
  #'                                       paired = FALSE, reps = 10000)
  #' @section References:
  #' 1. What is bootstrap resampling?
  #' 2. What are BCa confidence intervals?
  #'
  #' @seealso
  #' \url{https://www.google.com}
  #'

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

  ci <- boot::boot.ci(boot, conf = ci, type = c("perc", "bca"))

  result = list()

  result$func = func
  result$difference = diff
  result$ci = ci
  result$bca_ci_low = ci$bca[4]
  result$bca_ci_high = ci$bca[5]
  result$pct_ci_low = ci$percent[4]
  result$pct_ci_high = ci$percent[5]
  result$bootstraps = boot$t

  return(result)
}

