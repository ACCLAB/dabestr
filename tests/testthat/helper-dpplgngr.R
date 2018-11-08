# default is equal to whether NOT_CRAN is true or not
enable_vdiffr <- isTRUE(Sys.getenv("NOT_CRAN"))

# disable or enable vdiffr based on the state of USE_VDIFFR, if set
if ( isTRUE(Sys.getenv("USE_VDIFFR")) ) {
  enable_vdiffr <- TRUE
} else if ( isFALSE(Sys.getenv("USE_VDIFFR")) ) {
  enable_vdiffr <- FALSE
}

# disable vdiffr if version is old
if (!requireNamespace("vdiffr", quietly = TRUE) ||
    utils::packageVersion("vdiffr") < "0.2.3.9001") {
  enable_vdiffr <- FALSE
}

expect_dpplgngr <- function(title, fig, path = NULL, ...,
                            user_fonts = NULL,
                            verbose = FALSE) {

  if (!enable_vdiffr) {
    expect_error(regexp = NA, ggplot2::ggplot_build(fig))
    return(invisible(NULL))
  }

  vdiffr::expect_doppelganger(
    title,
    fig,
    path = path,
    ...,
    user_fonts = user_fonts,
    verbose = verbose
  )
}
