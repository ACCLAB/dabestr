

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
  #' samples. The \code{control} group is treated as \code{pret.intervention}
  #' and the \code{control} group is considered \code{post.intervention}.
  #' @param ci float, default 0.95. The level of the confidence intervals
  #' produced. The default (0.95) produces 95% CIs.
  #' @param reps integer, default 5000. The number of bootstrap resamples that
  #' will be generated.
  #' @param func function, default mean. This function will be applied to
  #' \code{control} and \code{test} individually, and the difference will be
  #' saved as a single bootstrap resample.
  #'
  #' @return A list with the following 8 elements:
  #' @param func The \code{func} passed to \code{bootdiff}.
  #' @param difference The difference between the two groups; effectively
  #' \code{func(test) - func(control)}.
  #' @param ci The \code{ci} passed to the \code{bootdiff}.
  #' @param bca_ci_low The lower limit of the Bias Corrected and Accelerated
  #' bootstrap confidence interval.
  #' @param bca_ci_high The upper limit of the Bias Corrected and Accelerated
  #' bootstrap confidence interval.
  #' @param pct_ci_low The lower limit of the percentile bootstrap
  #' confidence interval.
  #' @param pct_ci_low The upper limit of the percentile bootstrap
  #' confidence interval.
  #' @param bootstraps An array of all the bootstraps resamples.
  #'
  #' @examples
  #' # Use the preloaded data.
  #'
  #' @section References:
  #' Various references will be added here.
  #'
  #' @seealso
  #'

  c <- data[[control]]
  t <- data[[test]]

  if (identical(paired, FALSE)) {
    diff <- func(t) - func(c)
    # For two.boot, note that the first vector is the test vector.
    boot <- simpleboot::two.boot(t, c, FUN = diff_func, R = reps)

  } else {
    if (length(c) != length(t)) {
      stop("The two groups are not the same size, but paired = TRUE.")
    }
    paired_diff <- t - c
    diff <- func(paired_diff)
    boot <- simpleboot::one.boot(paired_diff, FUN = diff_func, R = reps)
  }

  ci <- boot::boot.ci(boot, conf = ci, type = c("perc", "bca"))

  result = list()

  result$difference = diff
  result$bootstraps = boot$t
  result$bca_ci_low = ci$bca[4]
  result$bca_ci_high = ci$bca[5]
  result$pct_ci_low = ci$percent[4]
  result$pct_ci_high = ci$percent[5]

  return(result)
}


create_padded_data_frame <- function(x1, x2, name1, name2) {
  #' DOCSTRING
  #'
  max_len = max(length(x1), length(x2))
  c = c(x1, rep(NA, max_len - length(x1)))
  t = c(x2, rep(NA, max_len - length(x2)))

  df <- data.frame(list(c, t))
  colnames(df) <- c(name1, name2)
  return(df)
}
