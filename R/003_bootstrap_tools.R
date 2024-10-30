# Obtains bootstraps as well as bca intervals
#
# Contains functions `effsize_boot`, `bootstrap`, `bca` and `boot_weighted_row`.

effsize_boot <- function(
    data,
    effect_size_func,
    reps = 5000,
    paired = FALSE) {
  s <- c(
    rep(1, length(data$control)),
    rep(2, length(data$test))
  )

  bootboot <- function(d, indices, paired) {
    c <- d[indices[s == 1]]
    t <- d[indices[s == 2]]

    return(effect_size_func(c, t, paired))
  }

  b <- boot::boot(
    c(data$control, data$test),
    statistic = bootboot,
    R = reps,
    strata = s,
    paired = paired
  )

  return(b)
}

check_params <- function(is_paired, boot_labs, proportional, delta2, ci) {
  if (is_paired && (boot_labs == "Cliffs' delta")) {
    cli::cli_abort(c("{.var Cliffs' delta} cannot be used when {.field paired} is not NULL.",
      "x" = "Please change {.var effect_size_func}."
    ))
  }
  if (proportional && !(boot_labs %in% c("Mean difference", "Cohen's h", "Paired\nmean difference"))) {
    cli::cli_abort(c("Other effect sizes besides {.var Cohens h} and {.var Mean difference} cannot be used when {.field proportional} is TRUE.",
      "x" = "Please change {.var effect_size_func}."
    ))
  }

  ## Check that if delta2 is true, only mean difference can be computed
  if (delta2 && !(boot_labs %in% c("Paired\nmean difference", "Mean difference"))) {
    cli::cli_abort(c("Other effect sizes besides {.var Mean difference} cannot be used when {.field delta2} is TRUE.",
      "x" = "Please change {.var effect_size_func}."
    ))
  }

  if (ci < 0 | ci > 100) {
    cli::cli_abort(c("{.field ci} is not between 0 and 100.",
      "x" = "{.field ci} must be between 0 and 100, not {ci}."
    ))
  }
}

get_boot_row <- function(ctrl_measurement, test_measurement, effect_size_func, seed, reps, is_paired, control_group, test_group, ci) {
  control_test_measurement <- list(
    control = ctrl_measurement,
    test = test_measurement
  )

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

  weight <- 1 / grp_var

  set.seed(seed)

  boots <- effsize_boot(
    data = control_test_measurement,
    effect_size_func = effect_size_func,
    reps = reps,
    paired = is_paired
  )

  bootci <- boot::boot.ci(boots, conf = ci / 100, type = c("perc", "bca"))

  boot_row <- list(
    control_group = control_group,
    test_group = test_group,
    bootstraps = list(as.vector(boots$t)),
    nboots = length(boots$t),
    bca_ci_low = bootci$bca[4],
    bca_ci_high = bootci$bca[5],
    pct_ci_low = bootci$percent[4],
    pct_ci_high = bootci$percent[5],
    ci = ci,
    difference = boots$t0,
    weight = weight
  )
}

# Main bootstrap function
bootstrap <- function(
    dabest_obj,
    effect_size_func,
    seed = 12345,
    reps = 5000,
    boot_labs) {
  boot_result <- tibble::tibble()
  baseline_ec_boot_result <- tibble::tibble()

  raw_data <- dabest_obj$raw_data
  idx <- dabest_obj$idx
  resamples <- dabest_obj$resamples

  if (!(is.list(idx))) {
    idx <- list(idx)
  }
  enquo_x <- dabest_obj$enquo_x
  enquo_y <- dabest_obj$enquo_y
  ci <- dabest_obj$ci
  paired <- dabest_obj$paired
  is_paired <- dabest_obj$is_paired
  is_colour <- dabest_obj$is_colour

  proportional <- dabest_obj$proportional

  quoname_x <- rlang::as_name(enquo_x)
  quoname_y <- rlang::as_name(enquo_y)
  delta_x_labels <- list()
  delta_y_labels <- boot_labs

  minimeta <- dabest_obj$minimeta
  delta2 <- dabest_obj$delta2

  ## Validity Checks
  check_params(is_paired, boot_labs, proportional, delta2, ci)

  ## Getting boot_results
  for (group in idx) {
    group_length <- length(group)
    for (i in 1:(group_length - 1)) {
      if (!(is_paired) || (paired == "baseline")) {
        control_group <- group[1]
      } else {
        control_group <- group[i]
      }

      test_group <- group[i + 1]

      ctrl_tibble <- raw_data %>%
        dplyr::filter(!!enquo_x == !!control_group)
      ctrl_measurement <- ctrl_tibble[[quoname_y]]

      test_tibble <- raw_data %>%
        dplyr::filter(!!enquo_x == !!test_group)
      test_measurement <- test_tibble[[quoname_y]]

      xlabels <- paste(test_group, control_group, sep = "\nminus\n")
      delta_x_labels <- append(delta_x_labels, xlabels)

      # add weights column
      boot_row <- get_boot_row(ctrl_measurement, test_measurement, effect_size_func, seed, reps, is_paired, control_group, test_group, ci)

      boot_result <- dplyr::bind_rows(boot_result, boot_row)
    }
  }

  if (minimeta) {
    boot_last_row <- boot_weighted_row(boot_result = boot_result, ci)
    boot_result <- dplyr::bind_rows(boot_result, boot_last_row)
  }
  if (delta2) {
    boot_last_row <- boot_delta_delta(boot_result = boot_result, ci)
    boot_result <- dplyr::bind_rows(boot_result, boot_last_row)
  }

  ## Getting boot_results for baseline_error_curve
  for (group in idx) {
    control_group <- group[1]
    test_group <- control_group

    ctrl_tibble <- raw_data %>%
      dplyr::filter(!!enquo_x == !!control_group)
    ctrl_measurement <- ctrl_tibble[[quoname_y]]
    test_measurement <- ctrl_measurement

    xlabels <- paste(test_group, control_group, sep = "\nminus\n")

    # add weights column
    boot_row <- get_boot_row(ctrl_measurement, test_measurement, effect_size_func, seed, reps, is_paired, control_group, test_group, ci)

    baseline_ec_boot_result <- dplyr::bind_rows(baseline_ec_boot_result, boot_row)
  }

  raw_y_labels <- ifelse(proportional, "proportion of success", "value")

  out <- list(
    raw_data = raw_data,
    idx = idx,
    delta_x_labels = delta_x_labels,
    delta_y_labels = delta_y_labels,
    raw_y_labels = raw_y_labels,
    is_paired = is_paired,
    is_colour = is_colour,
    paired = paired,
    resamples = resamples,
    Ns = dabest_obj$Ns,
    control_summary = dabest_obj$control_summary,
    test_summary = dabest_obj$test_summary,
    ylim = dabest_obj$ylim,
    enquo_x = dabest_obj$enquo_x,
    enquo_y = dabest_obj$enquo_y,
    enquo_id_col = dabest_obj$enquo_id_col,
    enquo_colour = dabest_obj$enquo_colour,
    proportional = proportional,
    minimeta = minimeta,
    delta2 = dabest_obj$delta2,
    proportional_data = dabest_obj$proportional_data,
    boot_result = boot_result,
    baseline_ec_boot_result = baseline_ec_boot_result
  )

  class(out) <- c("dabest_effectsize")

  return(out)
}

# BCA function
# TODO Add documentation
bca <- function(bootstraps, conf.level = .95) {
  # Inverse Variance Method
  if (stats::var(bootstraps) == 0) {
    lower <- mean(bootstraps)
    upper <- mean(bootstraps)
    return(c(lower, upper))
  }

  if (max(bootstraps) == Inf | min(bootstraps) == -Inf) {
    stop("bca() function does not work when some values are infinite")
  }

  low <- (1 - conf.level) / 2
  high <- 1 - low
  sims <- length(bootstraps)
  z.inv <- length(bootstraps[bootstraps < mean(bootstraps)]) / sims
  z <- stats::qnorm(z.inv)
  U <- (sims - 1) * (mean(bootstraps, na.rm = TRUE) - bootstraps)
  top <- sum(U^3)
  under <- 6 * (sum(U^2))^{
    3 / 2
  }
  a <- top / under
  lower.inv <- stats::pnorm(z + (z + stats::qnorm(low)) / (1 - a * (z + stats::qnorm(low))))
  lower <- stats::quantile(bootstraps, lower.inv, names = FALSE)
  upper.inv <- stats::pnorm(z + (z + stats::qnorm(high)) / (1 - a * (z + stats::qnorm(high))))
  upper <- stats::quantile(bootstraps, upper.inv, names = FALSE)
  return(c(lower, upper))
}


# TODO Add documentation
#' Creates df of values (bca ci, weighted bootstraps) for `minimeta`
#'
#' @param boot_result boot_result
#' @param ci ci
#'
#' @return A list with the minimeta parameters.
#' @noRd
#'
boot_weighted_row <- function(boot_result, ci) {
  bootstraps <- boot_result$bootstraps
  weights <- boot_result$weight

  weighted_result <- Map(
    function(x, w) x * w,
    boot_result$bootstraps, boot_result$weight
  )
  weighted_bootstrap <- Reduce("+", weighted_result)
  weighted_bootstrap <- weighted_bootstrap / sum(weights)


  weighted_difference <- calculate_weighted_delta(
    weight = boot_result$weight,
    differences = boot_result$difference
  )
  bca_weighted <- bca(bootstraps = weighted_bootstrap)
  pct_interval <- confinterval(weighted_bootstrap, ci / 100)
  boot_last_row <- list(
    control_group = "Minimeta Overall Test",
    test_group = "Minimeta Overall Test",
    bootstraps = list(as.vector(weighted_bootstrap)),
    nboots = length(weighted_bootstrap),
    bca_ci_low = bca_weighted[1],
    bca_ci_high = bca_weighted[2],
    pct_ci_low = pct_interval[1],
    pct_ci_high = pct_interval[2],
    ci = ci,
    difference = weighted_difference,
    weight = 1
  )
  return(boot_last_row)
}

#
# TODO Add documentation
#' Creates df of values (bca ci, weighted bootstraps) for `deltadelta`
#'
#' @param boot_result boot_result
#' @param ci ci
#'
#' @return A list with the deltadelta parameters.
#' @noRd
#' 
boot_delta_delta <- function(boot_result, ci) {
  bootstrap_delta_delta_neg <- Reduce("-", boot_result$bootstraps)
  bootstrap_delta_delta <- bootstrap_delta_delta_neg * -1
  difference_delta_delta <- calculate_delta_difference(boot_result$difference)
  bca_delta_delta <- bca(bootstrap_delta_delta)
  pct_interval <- confinterval(bootstrap_delta_delta, ci / 100)
  boot_last_row <- list(
    control_group = "Delta2 Overall Test",
    test_group = "Delta2 Overall Test",
    bootstraps = list(as.vector(bootstrap_delta_delta)),
    nboots = length(bootstrap_delta_delta),
    bca_ci_low = bca_delta_delta[1],
    bca_ci_high = bca_delta_delta[2],
    pct_ci_low = pct_interval[1],
    pct_ci_high = pct_interval[2],
    ci = ci,
    difference = difference_delta_delta,
    weight = 1
  )
  return(boot_last_row)
}
