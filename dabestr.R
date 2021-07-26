#' dabestr: A package for producing estimation plots.
#'
#' The dabestr package provides functiond to construct bootstrap confidence
#' intervals for differences between groups for a range of
#' \link[=mean_diff]{effect sizes} and a function to
#' generate \link[=plot.dabest_effsize]{estimation plots}.
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
