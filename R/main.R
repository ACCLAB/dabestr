
#' Differences between Groups with Bootstrap Confidence Intervals
#'
#' \code{dabest} applies a summary function (\code{func}, default
#' \code{\link{mean}}) to the groups listed in \code{idx}, which
#' are factors/strings in the \code{x} column of \code{.data}. The first element
#' of \code{idx} is the control group. The difference between
#' \code{func(group_n)} and \code{func(control)} is computed, for every
#' subsequent element of \code{idx}.\cr \cr For each comparison, a bootstrap
#' confidence interval is constructed for the difference, and bias correction and
#' acceleration is applied to correct for any skew. \code{dabest} uses bootstrap
#' resampling to compute non-parametric assumption-free confidence intervals,
#' and visualizes them using estimation plots with a specialized
#' \code{\link{plot.dabest}} function.
#'
#' Estimation statistics is a statistical framework that focuses on effect
#' sizes and confidence intervals around them, rather than \emph{P} values and
#' associated dichotomous hypothesis testing.
#'
#'
#'
#' @param .data A data.frame or tibble.
#'
#' @param x,y Columns in \code{.data}.
#'
#' @param idx A vector containing factors or strings in the \code{x} columns.
#'   These must be quoted (ie. surrounded by quotation marks). The first element
#'   will be the control group, so all differences will be computed for every
#'   other group and this first group.
#'
#' @param paired boolean, default FALSE. If TRUE, the two groups are treated as
#'   paired samples. The \code{control_group} group is treated as
#'   pre-intervention and the \code{test_group} group is considered
#'   post-intervention.
#'
#' @param id.column, default NULL. A column name indicating the identity of the
#'   datapoint if the data is paired. This must be supplied if paired is TRUE.
#'
#' @param ci float, default 95. The level of the confidence intervals produced.
#'   The default \code{ci = 95} produces 95\% CIs.
#'
#' @param reps integer, default 5000. The number of bootstrap resamples that
#'   will be generated.
#'
#' @param func function, default mean. This function will be applied to
#'   \code{control} and \code{test} individually, and the difference will be
#'   saved as a single bootstrap resample. Any NaNs will be removed
#'   automatically with \code{na.omit}.
#'
#' @param seed integer, default 12345. This specifies the seed used to set the
#' random number generator. Setting a seed ensures that the bootstrap confidence
#' intervals for the same data will remain stable over separate runs/calls of
#' this function. See \link{set.seed} for more details.
#'
#'
#'
#' @return A list with 7 elements: \code{data}, \code{x}, \code{y}, \code{idx},
#'  \code{id.column}, \code{result}, and \code{summary}.
#'
#'   \code{data}, \code{x}, \code{y}, \code{id.column}, and \code{idx} are the
#'   same keywords supplied to \code{dabest} as noted above. \cr \cr \code{x}
#'   and \code{y} are quoted variables for tidy evaluation by \code{plot}. \cr
#'   \cr \code{summary} is a \link{tibble} with \code{func} applied to every
#'   group specified in \code{idx}. These will be used by \code{plot()} to
#'   generate the estimation plot.
#'
#'   \code{result} is a \link{tibble} with the following 15 columns:
#'
#'   \item{control_group, test_group}{ The name of the control group
#'   and test group respectively. }
#'
#'   \item{control_size, test_size}{ The number
#'   of observations in the control group and test group respectively. }
#'
#'   \item{func}{ The \code{func} passed to \code{bootdiff}. }
#'
#'   \item{paired}{ Is
#'   the difference paired (\code{TRUE}) or not (\code{FALSE})? }
#'
#'   \item{difference}{ The difference between the two groups; effectively
#'   \code{func(test_group) - func(control_group)}. }
#'
#'   \item{variable}{ The
#'   variable whose difference is being computed, ie. the column supplied to
#'   \code{y}. }
#'
#'   \item{ci}{ The \code{ci} passed to the \code{bootdiff}. }
#'
#'   \item{bca_ci_low, bca_ci_high}{ The lower and upper limits of the Bias
#'   Corrected and Accelerated bootstrap confidence interval. }
#'
#'   \item{pct_ci_low, pct_ci_high}{ The lower and upper limits of the
#'   percentile bootstrap confidence interval. }
#'
#'   \item{bootstraps}{ The array of bootstrap resamples generated. }
#'
#'
#'
#' @seealso \code{\link{plot.dabest}}, which generates an estimation plot from
#'   the \code{dabest} object.
#'
#'
#'
#' @examples
#'
#' # Performing unpaired (two independent groups) analysis.
#' unpaired_mean_diff <- dabest(iris, Species, Petal.Width,
#'                              idx = c("setosa", "versicolor"),
#'                              paired = FALSE)
#'
#' # Display the results in a user-friendly format.
#' unpaired_mean_diff
#'
#' # Produce an estimation plot.
#' plot(unpaired_mean_diff)
#'
#'
#' # Performing paired analysis.
#' # First, we munge the `iris` dataset so we can perform a within-subject
#' # comparison of sepal length vs. sepal width.
#'
#' new.iris     <- iris
#' new.iris$ID  <- 1: length(new.iris)
#' setosa.only  <-
#'   new.iris %>%
#'   tidyr::gather(key = Metric, value = Value, -ID, -Species) %>%
#'   dplyr::filter(Species %in% c("setosa"))
#'
#' paired_mean_diff          <- dabest(
#'                               setosa.only, Metric, Value,
#'                               idx = c("Sepal.Length", "Sepal.Width"),
#'                               paired = TRUE, id.col = ID
#'                               )
#'
#'
#' # Computing the median difference.
#' unpaired_median_diff      <- dabest(
#'                               iris, Species, Petal.Width,
#'                               idx = c("setosa", "versicolor", "virginica"),
#'                               paired = FALSE,
#'                               func = median
#'                               )
#'
#'
#' # Producing a 90% CI instead of 95%.
#' unpaired_mean_diff_90_ci  <- dabest(
#'                               iris, Species, Petal.Width,
#'                               idx = c("setosa", "versicolor", "virginica"),
#'                               paired = FALSE,
#'                               ci = 0.90
#'                               )
#'
#'
#' # Constructing the confidence intervals on 10000 bootstrap resamples.
#' unpaired_mean_diff_n10000 <- dabest(
#'                                iris, Species, Petal.Width,
#'                                idx = c("setosa", "versicolor", "virginica"),
#'                                paired = FALSE,
#'                                reps = 10000
#'                                )
#'
#' # Using pipes to munge your data and then passing to `dabest`.
#' # First, we generate some synthetic data.
#' set.seed(12345)
#' N        <- 70
#' c         <- rnorm(N, mean = 50, sd = 20)
#' t1        <- rnorm(N, mean = 200, sd = 20)
#' t2        <- rnorm(N, mean = 100, sd = 70)
#' long.data <- tibble::tibble(Control = c, Test1 = t1, Test2 = t2)
#'
#' # Munge the data using `gather`, then pass it directly to `dabest`
#'
#' meandiff <- long.data %>%
#'               tidyr::gather(key = Group, value = Measurement) %>%
#'               dabest(x = Group, y = Measurement,
#'                      idx = c("Control", "Test1", "Test2"),
#'                      paired = FALSE)
#'
#'
#'
#' @section References:
#' \href{https://www.jstor.org/stable/2246110}{Bootstrap Confidence Intervals.}
#' DiCiccio, Thomas J., and Bradley Efron.
#' Statistical Science: vol. 11, no. 3, 1996. pp. 189–228.
#'
#'   \href{https://www.crcpress.com/An-Introduction-to-the-Bootstrap/Efron-Tibshirani/p/book/9780412042317}{An Introduction to the Bootstrap.} Efron, Bradley, and R. J. Tibshirani. 1994. CRC Press.
#'
#'
#' @importFrom magrittr %>%
#' @importFrom boot boot
#'
#' @export
dabest <- function(
            .data, x, y, idx, paired = FALSE, id.column = NULL,
            ci = 95, reps = 5000, func = mean, seed = 12345) {

  #### Create quosures and quonames. ####
  x_enquo        <-  rlang::enquo(x)
  x_quoname      <-  rlang::quo_name(x_enquo)

  y_enquo        <-  rlang::enquo(y)
  y_quoname      <-  rlang::quo_name(y_enquo)

  func_enquo     <-  rlang::enquo(func)
  func_quoname   <-  rlang::quo_name(func_enquo)

  id.col_enquo   <-  rlang::enquo(id.column)


  if (identical(paired, TRUE) & rlang::quo_is_null(id.col_enquo)) {
    stop("`paired` is TRUE but no `id.col` was supplied.")
  }



  #### Get only the columns we need. ####
  data_for_diff <-
    tibble::as_tibble(.data) %>%
    dplyr::select(!!x_enquo, !!y_enquo, !!id.col_enquo)



  #### Handled if paired. ####
  if (isTRUE(paired)) {
    id.col_quoname <-  rlang::quo_name(id.col_enquo)
    # sort the data by id.col so we can be sure all the observations match up.
    data_for_diff  <-
      data_for_diff %>% dplyr::arrange(!!x_enquo, !!id.col_enquo)
  }



  #### Decide if multiplot or not. ####
  if (class(idx) == "character") {
    # Not multiplot. Add it to an empty list.
    group_list  <-  list(idx)
    all_groups  <-  idx

  } else if (class(idx) == "list") {
    # This is a multiplot. Give it a new name.
    group_list  <-  idx
    all_groups  <-  unique(unlist(group_list)) # Flatten `group_list`.
  }



  #### Loop through each comparison group. ####
  result <- tibble::tibble() # To capture output.

  for (group in group_list) {

    # Check the control group (`group[1]`) is in the x-column.
    if (identical(group[1] %in% data_for_diff[[x_quoname]], FALSE)) {

      err1 <- stringr::str_interp("${group[1]} is not found")
      err2 <- stringr::str_interp("in the ${x_quoname} column.")

      stop(paste(err1, err2))
    }

    ctrl <-
      data_for_diff %>%
      dplyr::filter(!!x_enquo == group[1])

    ctrl <- ctrl[[y_quoname]]

    c <- na.omit(ctrl)

    # If ctrl is length 0, stop!
    if (length(c) == 0) {
      stop(
        stringr::str_interp(
          c("There are zero numeric observations in the group ${group[1]}.")
          )
        )
    }

    # Get test groups (everything else in group), loop through them and compute
    # the difference between group[1] and each group.
    # Test groups are the 2nd element of group onwards.
    test_groups <- group[2: length(group)]

    for (grp in test_groups) {

      # Check if the current group is in the x-column.
      if (identical(grp %in% data_for_diff[[x_quoname]], FALSE)) {
        stop(
          stringr::str_interp(
            "${grp} is not found in the ${x_quoname} column."
            )
          )
      }

      test <- data_for_diff %>% dplyr::filter(!!x_enquo == grp)
      test <- test[[y_quoname]]
      t <- na.omit(test)

      # If current test group is length 0, stop!
      if (length(t) == 0) {
        stop(
          stringr::str_interp(
            c("There are zero numeric observations in the group ${grp}.")
            )
          )
      }



      #### Compute bootstrap. ####
      set.seed(seed)

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



      #### Compute confidence interval. ####
      # check CI.
      if (ci < 0 | ci > 100) {
        err_string <- stringr::str_interp(
          "`ci` must be between 0 and 100, not ${ci}"
          )
        stop(err_string)
      }

      bootci <- boot::boot.ci(boot, conf = ci/100, type = c("perc", "bca"))



      #### Save pairwise result. ####
      row <- tibble::tibble(
        # Convert the name of `func` to a string.
        control_group = group[1],
        test_group = grp,
        control_size = length(c),
        test_size = length(t),
        func = func_quoname,
        paired = paired,
        variable = y_quoname,
        difference = diff,
        ci = ci,
        bca_ci_low = bootci$bca[4],
        bca_ci_high = bootci$bca[5],
        pct_ci_low = bootci$percent[4],
        pct_ci_high = bootci$percent[5],
        bootstraps = list(as.vector(boot$t)),
        nboots = length(boot$t)
      )
      result <- dplyr::bind_rows(result, row)

    }
  }

  # Reset seed.
  set.seed(NULL)



  #### Compute summaries. ####
  summaries <-
    .data %>%
    dplyr::filter(!!x_enquo %in% all_groups) %>%
    dplyr::group_by(!!x_enquo) %>%
    dplyr::summarize(func_quoname = func(!!y_enquo))

  colnames(summaries) <- c(x_quoname, func_quoname)



  #### Assemble only the data used to create the plot. ####
  data.out <- .data

  data.out[[x_quoname]] <- forcats::as_factor(data.out[[x_quoname]],
                                              all_groups)

  data.out <- dplyr::filter(data.out, !!x_enquo %in% all_groups)



  #### Collate output. ####
  out = list(
    data = data.out,
    x = x_enquo,
    y = y_enquo,
    idx = group_list,
    id.column = id.col_enquo,
    result = result,
    summary = summaries
  )



  #### Append the custom class `dabest`. ####
  class(out) <- c("dabest", "list")



  #### Return the output. ####
  return(out)
}


#' Print a `dabest` object
#'
#' @param x A \code{dabest} object, generated by the function of the same name.
#'
#' @param signif_digits integer, default 3. All numerical values in the printout
#' will be rounded to this many significant digits.
#'
#' @param ... Signature for S3 generic function.
#'
#' @return A summary of all the relevant effect sizes computed.
#'
#' @examples
#' # Performing unpaired (two independent groups) analysis.
#' unpaired_mean_diff <- dabest(iris, Species, Petal.Width,
#'                              idx = c("setosa", "versicolor"),
#'                              paired = FALSE)
#'
#' # Display the results in a user-friendly format.
#' print(unpaired_mean_diff)
#'
#' @export
print.dabest <- function(x, ..., signif_digits = 3) {

  #### Check object class ####
  if (class(x)[1] != "dabest") {
    stop(paste(
      "The object you are plotting is not a `dabest` class object. ",
      "Please check again! ")
    )
  } else {
    dabest.object <- x
  }

  #### Get results table and y var. ####
  tbl <- dabest.object$result
  var <- rlang::quo_name(dabest.object$y)

  #### Create header. ####
  dabest_ver <- utils::packageVersion("dabestr")
  header     <- stringr::str_interp(
    "DABEST (Data Analysis with Bootstrap Estimation) v${dabest_ver}\n")
  cat(header)

  cat(rep('=', nchar(header) - 1), sep='')
  cat("\n\n")

  cat(stringr::str_interp("Variable: ${var} \n\n"))

  #### Print each row. ####
  cat(apply(tbl, 1, printrow_dabest, sigdig = signif_digits),
      sep = "\n")

  #### Endnote about BCa. ####
  cat(stringr::str_interp("${tbl$nboots[1]} bootstrap resamples.\n"))
  cat("All confidence intervals are bias-corrected and accelerated.\n\n")

}




printrow_dabest <- function(my.row, sigdig = 3) {
  if (identical(my.row$paired, TRUE)) p <- "Paired" else p <- "Unpaired"
  ffunc <- my.row$func
  line1 <- stringr::str_interp(
    c(
      "${p} ${ffunc} difference of ",
      "${my.row$test_group} ",
      "(n=${my.row$control_size}) ",
      "minus ${my.row$control_group} ",
      "(n=${my.row$test_size})\n"
    )
  )


  line2 <- stringr::str_interp(
    c("${signif(my.row$difference, sigdig)} ",
      "[${signif(my.row$ci, sigdig)}CI  ",
      "${signif(my.row$bca_ci_low, sigdig)}; ",
      "${signif(my.row$bca_ci_high, sigdig)}]\n\n")
  )

  cat(line1, line2)
}

