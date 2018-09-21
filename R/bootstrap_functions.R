

bootdiff <- function(data, control, test, paired, reps = 5000,
                     diff_func = mean) {
  #' DOCTSTRING
  #' data must be a dataframe or matrix
  #'
  c <- data[[control]]
  t <- data[[test]]
  
  if (identical(paired, FALSE)) {
    diff <- diff_func(t) - diff_func(c)
    # For two.boot, note that the first vector is the test vector.
    boot <- simpleboot::two.boot(t, c, FUN = diff_func, R = reps)
    
  } else {
    if (length(c) != length(t)) {
      stop("The two groups are not the same size, but paired = TRUE.")
    }
    paired_diff <- t - c
    diff <- difference_function(paired_diff)
    boot <- simpleboot::one.boot(paired_diff, FUN = diff_func, R = reps)
  }
  
  ci <- boot::boot.ci(boot, type = c("perc", "bca"))
  
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