#' dabestr: A package for producing estimation plots.
#'
#' The dabestr package provides a function to construct bootstrap confidence
#' intervals for differences between groups \link{dabest} and a function to
#' generate estimation plots \link{plot.dabest}.
#'
#' @docType package
#' @name dabestr
NULL

## quiets concerns of R CMD check due to non-standard evaluation.
if(getRversion() >= "2.15.1") {
  utils::globalVariables(c("bca_ci_high","bca_ci_low","difference",
                           "low.quartile", "low.sd", "test_group",
                           "upper.quartile","upper.sd")
                         )
}
