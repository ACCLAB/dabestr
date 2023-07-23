#' Main effectsize api
#' 
#' @description
#' Contains functions `mean_diff`, `median_diff`, `cohens_d`, `hedges_g`,
#' `cliffs_delta`, `cohens_h` and `hedges_correction`.
#' 
#' To be used after loading in data with `load()` function.

mean_diff <- function(dabest_obj) {
  
  if (class(dabest_obj)!="dabest") {
    cli::cli_abort(c("{.field dabest_obj} must be a {.cls dabest} object."),
                   "x" = "Please supply a {.cls dabest} object.")
  }
  
  effect_size_func <- function(control, test, paired) {
    if (identical(paired, FALSE)) {
      return(mean(test) - mean(control))
    }
    return(mean(test - control))
  }
  
  is_paired <- dabest_obj$is_paired
  
  if(is_paired){
    return(bootstrap(dabest_obj, effect_size_func, boot_labs = "Paired\nmean difference"))
  }
  return(bootstrap(dabest_obj, effect_size_func, boot_labs = "Mean difference"))
}

median_diff <- function(dabest_obj) {
  
  if (class(dabest_obj)!="dabest") {
    cli::cli_abort(c("{.field dabest_obj} must be a {.cls dabest} object."),
                   "x" = "Please supply a {.cls dabest} object.")
  }
  
  effect_size_func <- function(control, test, paired) {
    if (identical(paired, FALSE)) {
      return(median(test) - median(control))
    }
    return(median(test - control))
  }
  
  is_paired <- dabest_obj$is_paired
  
  if(is_paired){
    return(bootstrap(dabest_obj, effect_size_func, boot_labs = "Paired\nmedian difference"))
  }
  return(bootstrap(dabest_obj, effect_size_func, boot_labs = "Median difference"))
}

cohens_d <- function(dabest_obj) {
  
  if (class(dabest_obj)!="dabest") {
    cli::cli_abort(c("{.field dabest_obj} must be a {.cls dabest} object."),
                   "x" = "Please supply a {.cls dabest} object.")
  }
  
  effect_size_func <- function(control, test, paired) {
    return(effsize::cohen.d(test, control, paired=paired)$estimate)
  }
  
  return(bootstrap(dabest_obj, effect_size_func, boot_labs = "Cohen's d"))
}

hedges_g <- function(dabest_obj) {
  
  if (class(dabest_obj)!="dabest") {
    cli::cli_abort(c("{.field dabest_obj} must be a {.cls dabest} object."),
                   "x" = "Please supply a {.cls dabest} object.")
  }
  
  cohens_d_ <- function(control, test, paired) {
    return(effsize::cohen.d(test, control, paired=paired)$estimate)
  }
  
  effect_size_func <- function(control, test, paired) {
    cd <- cohens_d_(test, control, paired=paired)
    corr.factor <- -hedges_correction(test, control)
    return(cd * corr.factor)
  }
  
  return(bootstrap(dabest_obj, effect_size_func, boot_labs = "Hedges' g"))
}

cliffs_delta <- function(dabest_obj) {
  
  if (class(dabest_obj)!="dabest") {
    cli::cli_abort(c("{.field dabest_obj} must be a {.cls dabest} object."),
                   "x" = "Please supply a {.cls dabest} object.")
  }
  
  effect_size_func <- function(control, test, paired=NA) {
    return(effsize::cliff.delta(test, control)$estimate)
  }
  
  return(bootstrap(dabest_obj, effect_size_func, boot_labs = "Cliffs' delta"))
}

cohens_h <- function(dabest_obj){
  
  if (class(dabest_obj)!="dabest") {
    cli::cli_abort(c("{.field dabest_obj} must be a {.cls dabest} object."),
                   "x" = "Please supply a {.cls dabest} object.")
  }
  
  effect_size_func <- function(control, test, paired) {
    #remove nas and nulls later on
    prop_control <- mean(control)
    prop_test <- mean(test)
    
    # Arcsine transformation
    phi_control <- 2 * asin(sqrt(prop_control))
    phi_test <- 2 * asin(sqrt(prop_test))
    result <- phi_test - phi_control
    return(result)
  }
  
  return(bootstrap(dabest_obj, effect_size_func, boot_labs = "Cohen's h"))
}

hedges_correction <- function(x1, x2) {
  
  n1 <- length(x1)
  n2 <- length(x2)
  
  deg.freedom <- n1 + n2 - 2
  numer       <- gamma(deg.freedom/2)
  denom0      <- gamma((deg.freedom - 1) / 2)
  denom       <- sqrt((deg.freedom / 2)) * denom0
  
  if (is.infinite(numer) | is.infinite(denom)) {
    # Occurs when df is too large.
    # Applies Hedges and Olkin's approximation.
    df.sum <- n1 + n2
    denom <- (4 * df.sum) - 9
    out <- 1 - (3 / denom)
  } else out <- numer / denom
  
  return(out)
}