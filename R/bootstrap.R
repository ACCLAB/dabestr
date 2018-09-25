
#' Difference between Two Groups with Bootstrap Confidence Intervals
#'
#' \code{bootdiff} applies a summary function (\code{func}, default
#' \code{mean}) to \code{control} and \code{test}. The difference between
#' \code{func(test)} and \code{func(control)} is computed, and a bootstrap
#' confidence interval is constructed for the difference.
#'
#'
#' @param data A data.frame or tibble.
#' @param x,y Columns in \code{data}.
#' @param control_group,test_group Factors or strings in the \code{x} columns.
#' These must be quoted (ie surrounded by quotation marks), and will be used to
#' identify the control group and the test group. Any NaNs will be removed with
#' \code{na.omit}.
#' @param paired boolean. If TRUE, the two groups are treated as paired
#' samples. The \code{control_group} group is treated as pre-intervention
#' and the \code{test_group} group is considered post-intervention.
#' @param ci float, default 0.95. The level of the confidence intervals
#' produced. The default \code{ci = 0.95} produces 95\% CIs.
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
#' unpaired_mean_diff <- bootdiff(iris, Species, Petal.Width,
#'                                "setosa", "versicolor", paired = FALSE)
#'
#' # Display the results in a user-friendly format.
#' unpaired_mean_diff
#'
#' # Plot the bootstrap resamples as a histogram, along with the CI.
#' plot(unpaired_mean_diff)
#'
#'
#' # Performing paired analysis.
#' paired_mean_diff <- bootdiff(iris, Species, Petal.Width,
#'                              "setosa", "versicolor", paired = FALSE)
#'
#'
#' # Computing the median difference.
#' unpaired_median_diff <- bootdiff(iris, Species, Petal.Width,
#'                                  "setosa", "versicolor", paired = FALSE,
#'                                  func = median)
#'
#'
#' # Producing a 90\% CI instead of 95\%
#' unpaired_mean_diff_90_ci <- bootdiff(iris, Species, Petal.Width,
#'                                      "setosa", "versicolor", paired = FALSE,
#'                                      ci = 0.90)
#'
#'
#' # Constructing the confidence intervals on 10000 bootstrap resamples.
#' unpaired_mean_diff_n10000 <- bootdiff(iris, Species, Petal.Width,
#'                                       "setosa", "versicolor", paired = FALSE,
#'                                       reps = 10000)
#'
#' @section References:
#' DiCiccio, Thomas J., and Bradley Efron. Bootstrap Confidence Intervals.
#'   Statistical Science: vol. 11, no. 3, 1996, pp. 189–228,
#'
#'   \url{http://www.jstor.org/stable/2246110.}
#'
#'
#' Efron, Bradley, and R. J. Tibshirani. An Introduction to the Bootstrap.
#'   CRC Press, 1994.
#'
#'   \url{https://www.crcpress.com/An-Introduction-to-the-Bootstrap/Efron-Tibshirani/p/book/9780412042317}
#'
#' @export
bootdiff <- function(data, x, y, control_group, test_group, paired,
                     ci = 0.95, reps = 5000, func = mean) {
  # Create quosures and quonames to pass variables along properly.
  x_enquo       <-  enquo(x)
  x_quoname     <-  quo_name(x_enquo)

  y_enquo       <-  enquo(y)
  y_quoname     <-  quo_name(y_enquo)

  func_enquo    <-  enquo(func)
  func_quoname  <-  quo_name(func_enquo)


  # Get only the columns we need.
  data_for_diff <-
    as_tibble(data) %>%
    select(!!x_enquo, !!y_enquo)


  # Get ctrl and exp groups data
  ctrl <- data_for_diff %>%
    filter(!!x_enquo == control_group)

  test <- data_for_diff %>%
    filter(!!x_enquo == test_group)

  ctrl = ctrl[[y_quoname]]
  test = test[[y_quoname]]


  # Remove NaNs.
  c <- na.omit(ctrl)
  t <- na.omit(test)


  # Compute bootstrap.
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


  # Compute confidence interval.
  bootci <- boot::boot.ci(boot, conf = ci, type = c("perc", "bca"))


  # Save result.
  result = list()
  # Convert the name of `func` to a string.
  result$func = func_quoname
  result$paired = paired
  result$difference = diff
  result$ci = ci
  result$bca_ci_low = bootci$bca[4]
  result$bca_ci_high = bootci$bca[5]
  result$pct_ci_low = bootci$percent[4]
  result$pct_ci_high = bootci$percent[5]
  result$bootstraps = as.vector(boot$t)
  # Create our own class.
  class(result) <- "boot.diff"


  # Return the result.
  return(result)
}


#' @export
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


#' @export
plot.boot.diff <- function(result, ...) {

  bootstrap_resamples = result$bootstraps

  # Capitalise the function for a prettier title.
  funct <- result$func
  substr(funct, 1, 1) <- toupper(substr(funct, 1, 1))

  # Is this paired or unpaired?
  if (identical(result$paired, TRUE)) {
    is.paired <- "Paired"
    } else {
      is.paired <- "Unpaired"
    }

  # Stitch the title.
  hist.title <- stringr::str_interp(
    "Bootstrap Distribution of ${is.paired} Two-group ${funct} Difference")

  # Plot the histogram.
  hist(bootstrap_resamples, main = hist.title, xlab = "", ...)
  # Plot the mean difference.
  abline(v = result$difference, col='red')
  # Plot the CIs.
  abline(v = result$bca_ci_low)
  abline(v = result$bca_ci_high)
}
