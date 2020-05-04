#' Compute the Mean Difference
#'
#' @param .data A dabest_proto object
#'
#' @param ci float, default 95. The level of the confidence intervals produced.
#'   The default \code{ci = 95} produces 95\% CIs.
#'
#' @param reps integer, default 5000. The number of bootstrap resamples that
#'   will be generated.
#'
#' @param seed integer, default 12345. This specifies the seed used to set the
#' random number generator. Setting a seed ensures that the bootstrap confidence
#' intervals for the same data will remain stable over separate runs/calls of
#' this function. See \link{set.seed} for more details.
#'
#' @return Difference of means for treatment vs. control.
#'
#' @export
mean_diff    <- function(x, ..., ci = 95, reps = 5000 , seed = 12345) UseMethod("mean_diff", x)

#' @export
mean_diff.dabest_proto <- function(x, ..., ci = 95, reps = 5000 , seed = 12345) {
  effect_size(x, ci = ci, reps = reps, seed = seed, effect.size = "mean_diff")
}


#' Compute the Median Difference
#'
#' @param .data A dabest_proto object
#'
#' @param ci float, default 95. The level of the confidence intervals produced.
#'   The default \code{ci = 95} produces 95\% CIs.
#'
#' @param reps integer, default 5000. The number of bootstrap resamples that
#'   will be generated.
#'
#' @param seed integer, default 12345. This specifies the seed used to set the
#' random number generator. Setting a seed ensures that the bootstrap confidence
#' intervals for the same data will remain stable over separate runs/calls of
#' this function. See \link{set.seed} for more details.
#'
#' @return Difference of medians for treatment vs. control.
#'
#' @export
median_diff  <- function(x, ..., ci = 95, reps = 5000 , seed = 12345) UseMethod("median_diff", x)

#' @export
median_diff.dabest_proto <- function(x, ..., ci = 95, reps = 5000 , seed = 12345) {
  effect_size(x, ci = ci, reps = reps, seed = seed, effect.size = "median_diff")
}


#' Compute Cohen's d
#'
#' @param .data A dabest_proto object
#'
#' @param ci float, default 95. The level of the confidence intervals produced.
#'   The default \code{ci = 95} produces 95\% CIs.
#'
#' @param reps integer, default 5000. The number of bootstrap resamples that
#'   will be generated.
#'
#' @param seed integer, default 12345. This specifies the seed used to set the
#' random number generator. Setting a seed ensures that the bootstrap confidence
#' intervals for the same data will remain stable over separate runs/calls of
#' this function. See \link{set.seed} for more details.
#'
#' @return Cohen's d.
#' @export
cohens_d     <- function(x, ..., ci = 95, reps = 5000 , seed = 12345) UseMethod("cohens_d", x)

#' @export
cohens_d.dabest_proto <- function(x, ..., ci = 95, reps = 5000 , seed = 12345) {
  effect_size(x, ci = ci, reps = reps, seed = seed, effect.size = "cohens_d")
}


#' Compute Hedges' g
#'
#' @param .data A dabest_proto object
#'
#' @param ci float, default 95. The level of the confidence intervals produced.
#'   The default \code{ci = 95} produces 95\% CIs.
#'
#' @param reps integer, default 5000. The number of bootstrap resamples that
#'   will be generated.
#'
#' @param seed integer, default 12345. This specifies the seed used to set the
#' random number generator. Setting a seed ensures that the bootstrap confidence
#' intervals for the same data will remain stable over separate runs/calls of
#' this function. See \link{set.seed} for more details.
#'
#' @return Difference of means for treatment vs. control.
#' @export
hedges_g     <- function(x, ..., ci = 95, reps = 5000 , seed = 12345) UseMethod("hedges_g", x)

#' @export
hedges_g.dabest_proto <- function(x, ..., ci = 95, reps = 5000 , seed = 12345) {
  effect_size(x, ci = ci, reps = reps, seed = seed, effect.size = "hedges_g")
}


#' Compute Cliff's delta
#'
#' @param .data A dabest_proto object
#'
#' @param ci float, default 95. The level of the confidence intervals produced.
#'   The default \code{ci = 95} produces 95\% CIs.
#'
#' @param reps integer, default 5000. The number of bootstrap resamples that
#'   will be generated.
#'
#' @param seed integer, default 12345. This specifies the seed used to set the
#' random number generator. Setting a seed ensures that the bootstrap confidence
#' intervals for the same data will remain stable over separate runs/calls of
#' this function. See \link{set.seed} for more details.
#'
#' @return Difference of means for treatment vs. control.
#' @export
cliffs_delta <- function(x, ..., ci = 95, reps = 5000 , seed = 12345) UseMethod("cliffs_delta", x)

#' @export
cliffs_delta.dabest_proto <- function(x, ..., ci = 95, reps = 5000 , seed = 12345) {
  effect_size(x, ci = ci, reps = reps, seed = seed, effect.size = "cliffs_delta")
}



mean_diff_ <- function(control, treatment, paired) {
  if (identical(paired, FALSE)) return(mean(treatment) - mean(control))
  else return(mean(treatment - control))
}



median_diff_ <- function(control, treatment, paired) {
  if (identical(paired, FALSE)) return(median(treatment) - median(control))
  else return(median(treatment - control))
}



cohens_d_ <- function(control, treatment, paired) {
  return(effsize::cohen.d(treatment, control, paired=paired)$estimate)
}



hedges_g_ <- function(control, treatment, paired) {
  cd <- cohens_d_(treatment, control, paired=paired)
  corr.factor <- hedges_correction(treatment, control)
  return(cd * corr.factor)
}



cliffs_delta_ <- function(control, treatment, paired=NA) {
  return(effsize::cliff.delta(treatment, control)$estimate)
}



hedges_correction <- function(g1, g2) {
  # Returns the exact Hedges' correction factor for Cohen's d.

  deg.freedom = length(g1) + length(g2) - 2

  exact.f = gamma(deg.freedom/2) /
    (sqrt(deg.freedom/2) * gamma((deg.freedom - 1)/2))

  return(exact.f)
}



effsize_boot <- function(effsize, data, paired, indices) {
  control <- data$control[indices]
  test <- data$test[indices]

  cd <- effsize(control, test, paired)

  return(cd)
}



#' @importFrom magrittr %>%
effect_size <- function(.data, ..., effect.size, ci, reps, seed) {

  #### Check object class ####
  if (class(.data)[1] != "dabest_proto") {
    stop(paste(
      "The object you are plotting is not a `dabest_proto` class object. ",
      "Please check again! ")
    )
  }

  # Create handles for easy access to the items in `.data`.
  raw.data            <-  .data$data
  idx                 <-  .data$idx
  all.groups          <-  .data$.all.groups
  paired              <-  .data$is.paired

  plot.groups.sizes   <-  unlist(lapply(idx, length))

  # The variables below should are quosures!
  x_enquo             <-  .data$x
  y_enquo             <-  .data$y
  effect.size_enquo   <-  rlang::quo_name(effect.size)

  x_quoname           <-  rlang::quo_name(x_enquo)
  y_quoname           <-  rlang::quo_name(y_enquo)

  # effect.size_enquo   <-  rlang::enquo(effect.size)
  # effect.size_quoname <-  rlang::quo_name(effect.size_enquo)

  id.col_enquo        <-  .data$id.column

  # Parse the effect.size.
  if (effect.size == "mean_diff") {
    es = mean_diff_
    es_summ = mean
    es_name = "mean"
  } else if (effect.size == "median_diff") {
    es = median_diff_
    es_summ = median
    es_name = "median"
  } else if (effect.size == "cohens_d") {
    es = cohens_d_
    es_summ = mean
    es_name = "mean"
  } else if (effect.size == "hedges_g") {
    es = hedges_g_
    es_summ = mean
    es_name = "mean"
  } else if (effect.size == "cliffs_delta") {
    es = cliffs_delta_
    es_summ = median
    es_name = "median"
  }



  #### Loop through each comparison group. ####
  result <- tibble::tibble() # To capture output.

  for (group in idx) {

    # Check the control group (`group[1]`) is in the x-column.
    if (identical(group[1] %in% raw.data[[x_quoname]], FALSE)) {

      err1 <- stringr::str_interp("${group[1]} is not found")
      err2 <- stringr::str_interp("in the ${x_quoname} column.")

      stop(paste(err1, err2))
    }

    # Patch in v0.2.2.
    # Note how we have to unquote both the x_enquo, and the group name!
    ctrl <- raw.data %>% dplyr::filter(!!x_enquo == !!group[1])

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
      if (identical(grp %in% raw.data[[x_quoname]], FALSE)) {
        stop(
          stringr::str_interp(
            "${grp} is not found in the ${x_quoname} column."
          )
        )
      }

      # Patch in v0.2.2.
      # Note how we have to unquote both x_enquo, and grp!
      test <- raw.data %>% dplyr::filter(!!x_enquo == !!grp)
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
      input_list <- data.frame(control=ctrl, test=test)

      set.seed(seed)

      boot_result <- boot::boot(statistic = effsize_boot,
                               R = reps,
                               effsize = es,
                               data = input_list,
                               paired = paired)
      set.seed(NULL)

      # if (identical(paired, FALSE)) {
      #   diff <- func(t) - func(c)
      #   # For two.boot, note that the first vector is the test vector.
      #   boot <- simpleboot::two.boot(t, c, FUN = func, R = reps)
      #
      # } else {
      #   if (length(c) != length(t)) {
      #     stop("The two groups are not the same size, but paired = TRUE.")
      #   }
      #   paired_diff <- t - c
      #   diff <- func(paired_diff)
      #   boot <- simpleboot::one.boot(paired_diff, FUN = func, R = reps)
      # }



      #### Compute confidence interval. ####
      # check CI.
      if (ci < 0 | ci > 100) {
        err_string <- stringr::str_interp(
          "`ci` must be between 0 and 100, not ${ci}"
        )
        stop(err_string)
      }

      bootci <- boot::boot.ci(boot_result, conf = ci/100, type = c("perc", "bca"))


      #### Save pairwise result. ####
      row <- tibble::tibble(
        # Convert the name of `func` to a string.
        control_group = group[1],
        test_group = grp,
        control_size = length(c),
        test_size = length(t),
        func = effect.size,
        paired = paired,
        variable = y_quoname,
        difference = boot_result$t0,
        ci = ci,
        bca_ci_low = bootci$bca[4],
        bca_ci_high = bootci$bca[5],
        pct_ci_low = bootci$percent[4],
        pct_ci_high = bootci$percent[5],
        bootstraps = list(as.vector(boot_result$t)),
        nboots = length(boot_result$t)
      )
      result <- dplyr::bind_rows(result, row)
    }
  }

  # Reset seed.
  set.seed(NULL)



  #### Compute summaries. ####
  summaries <-
    raw.data %>%
    dplyr::filter(!!x_enquo %in% all.groups) %>%
    dplyr::group_by(!!x_enquo) %>%
    dplyr::summarize(func_quoname = es_summ(!!y_enquo))

  colnames(summaries) <- c(x_quoname, es_name)

  # Order the summaries by the idx.
  summaries[[x_quoname]] <-
    summaries[[x_quoname]] %>%
    factor(all.groups, ordered = TRUE)

  summaries <- summaries %>% dplyr::arrange(!!x_enquo)



  #### Assemble only the data used to create the plot. ####
  data.out <- raw.data

  # New in v0.2.1 patch.
  # Basically, the `ellipsis` package has been updated,
  # and now forcats::as_factor() should only take the object to coerce.
  data.out[[x_quoname]] <- forcats::as_factor(data.out[[x_quoname]])

  data.out <- dplyr::filter(data.out, !!x_enquo %in% all.groups)



  #### Collate output. ####
  out = list(
    data = data.out,
    x = x_enquo,
    y = y_enquo,
    idx = all.groups,
    id.column = id.col_enquo,
    result = result,
    summary = summaries
  )



  #### Append the custom class `dabest_effsize_proto`. ####
  class(out) <- c("dabest_effsize_proto", "list")



  #### Return the output. ####
  return(out)
}



#' Print a `dabest_effsize_proto` object
#'
#' @param dabest.effsize A \code{dabest_effsize_proto} object, generated by \code{compute.effect.size}.
#'
#' @param ... Signature for S3 generic function.
#'
#'
#' @return A summary of the experimental designs.
#'
#' @examples
#' # Performing unpaired (two independent groups) analysis.
#' unpaired_mean_diff <- dabest(iris, Species, Petal.Width,
#'                              idx = c("setosa", "versicolor"),
#'                              paired = FALSE) %>%
#'                      compute.effect.siz
#'
#' # Display the results in a user-friendly format.
#' print(unpaired_mean_diff)
#'
#' @export
print.dabest_effsize_proto <- function(x, ..., signif_digits = 3) {

  #### Check object class ####
  if (class(x)[1] != "dabest_effsize_proto") {
    stop(paste(
      "The object you are plotting is not a `dabest_effsize_proto` class object. ",
      "Please check again! ")
    )
  } else {
    dabest.effsize <- x
  }

  #### Get results table and y var. ####
  tbl <- dabest.effsize$result
  var <- rlang::quo_name(dabest.effsize$y)

  #### Create header. ####
  dabest_ver <- utils::packageVersion("dabestr")
  header     <- stringr::str_interp(
    "DABEST (Data Analysis with Bootstrap Estimation) v${dabest_ver}\n")
  cat(header)

  cat(rep('=', nchar(header) - 1), sep='')
  cat("\n\n")

  cat(stringr::str_interp("Variable: ${var} \n\n"))

  #### Print each row. ####
  cat(apply(tbl, 1, printrow_, sigdig = signif_digits),
      sep = "\n")

  #### Endnote about BCa. ####
  cat(stringr::str_interp("${tbl$nboots[1]} bootstrap resamples.\n"))
  cat("All confidence intervals are bias-corrected and accelerated.\n\n")

}




printrow_ <- function(my.row, sigdig = 3) {
  if (identical(my.row$paired, TRUE)) p <- "Paired" else p <- "Unpaired"
  ffunc <- my.row$func
  line1 <- stringr::str_interp(
    c(
      "${p} ${ffunc} difference of ",
      "${my.row$test_group} ",
      "(n = ${my.row$test_size}) ",
      "minus ${my.row$control_group} ",
      "(n = ${my.row$control_size})\n"
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



