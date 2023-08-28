#' Non-proportional data for Estimation plots.
#'
#' Contains 3 Control Samples and 6 Test Samples.
#'
#' @keywords internal
#' @format A data frame with 180 rows and 4 variables:
#'  \describe{
#'       \item{Gender}{Gender of each observation}
#'       \item{ID}{Identity of each observation}
#'       \item{Group}{Which control group or test it is}
#'       \item{Measurement}{Measurement value}
#'       }
#'
#' @examples
#' data(non_proportional_data) # Lazy loading. Data becomes visible as soon as it is loaded
"non_proportional_data"




#' Numerical Binary data for Proportion Plots
#'
#' Contains 3 Control Samples and 7 Test Samples.
#'
#' @keywords internal
#' @format A data frame with 400 rows and 4 variables:
#'  \describe{
#'       \item{Gender}{Gender of each observation}
#'       \item{ID}{Identity of each observation}
#'       \item{Group}{Which control group or test it is}
#'       \item{Success}{1 (Success) or 0 (Failure)}
#'       }
#'
#' @examples
#' data(proportional_data) # Lazy loading. Data becomes visible as soon as it is loaded
"proportional_data"




#' Data to produce a mini-meta Dabest plot
#'
#' Contains 3 Control Samples and 3 Test Samples.
#'
#' @keywords internal
#' @format A data frame with 120 rows and 5 variables:
#'  \describe{
#'       \item{Gender}{Gender of each observation}
#'       \item{ID}{Identity of each observation}
#'       \item{Group}{Which control group or test it is}
#'       \item{Measurement}{Measurement value}
#'       }
#'
#' @examples
#' data(minimeta_data) # Lazy loading. Data becomes visible as soon as it is loaded
"minimeta_data"




#' Data to produce a delta2 Dabest plot
#'
#' Contains 2 Genotype groups and 2 Treatment groups.
#'
#' @keywords internal
#' @format A data frame with 40 rows and 5 variables:
#'  \describe{
#'       \item{Genotype}{Genotype of each observation}
#'       \item{ID}{Identity of each observation}
#'       \item{Rep}{Rep of each observation}
#'       \item{Treatment}{Which treatment method was used}
#'       \item{Measurement}{Measurement value}
#'       }
#'
#' @examples
#' data(deltadelta_data) # Lazy loading. Data becomes visible as soon as it is loaded
"deltadelta_data"
