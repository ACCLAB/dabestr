#' Formulas used
#' 
#' @description
#' Contains functions `sigmoid`, `flipped_sig`, `calculate_group_variance`, `calculate_weighted_delta`, `confinterval` and `var_w_df`.

sigmoid <- function(x_from, factor, y_from, y_to, smooth = 5.5, n = 300) {
  x <- seq(-smooth, smooth, length = n)
  y <- exp(x) / (exp(x) + 1)
  out <- data.frame(x = (x + smooth) / (smooth * 2) * factor + x_from,
                    y = y * (y_to - y_from) + y_from)
}

flipped_sig <- function(x_from, factor, y_from, y_to, smooth = 5.5, n = 300) {
  x <- seq(-smooth, smooth, length = n)
  y <- -exp(-x) / (exp(-x) + 1)
  out <- data.frame(x = (x + smooth) / (smooth * 2) * factor + x_from,
                    y = y * (y_to - y_from) + y_to)
}

calculate_group_variance <- function(ctrl_var, 
                                     ctrl_N,
                                     test_var, 
                                     test_N) {
  num <- (test_N-1)*test_var + (ctrl_N-1)*ctrl_var
  denom <- test_N + ctrl_N -2
  return(num/denom)
  # return(ctrl_var/ctrl_N + test_var/test_N)
}

calculate_weighted_delta <- function(weight, differences) {
  denom <- sum(weight)
  num <- sum(weight*differences)
  return(num / denom)
}

calculate_delta_difference <- function(differences) {
  delta_difference <- differences[2]-differences[1]
  return(delta_difference)
}

confinterval <- function(vector, ci_decimal){
  sample_mean <- mean(vector)
  standard_error <- sd(vector) / sqrt(length(vector))
  z <- qnorm(1 - (1 - ci_decimal) / 2)
  
  margin_of_error <- z * standard_error
  confidence_interval <- c(sample_mean - margin_of_error, 
                           sample_mean + margin_of_error)
  
  return(confidence_interval)
}

var_w_df <- function(measurement, size){
  df <- size - 1
  # Calculate the variance with specified degrees of freedom
  var <- (sum((measurement - mean(measurement))^2) / df)
  
  return(var)
}