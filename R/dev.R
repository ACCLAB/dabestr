
#' Estimation Plot (DEV VERSION)
#'
#' An estimation plot has two key features.
#' \enumerate{
#'   \item{It presents all datapoints as a
#'   \href{https://github.com/eclarke/ggbeeswarm#introduction}{swarmplot} or
#'   \href{https://cran.r-project.org/web/packages/sinaplot/vignettes/SinaPlot.html}{sinaplot},
#'   which orders each point to display the underlying distribution.}
#'   \item{It presents the effect size as a bootstrap 95 percent confidence
#'         interval on a separate but aligned axes.}
#' }
#' Estimation plots emerge from estimation statistics, an inutitive framework
#' that avoids the pitfalls of significance testing. It uses familiar
#' statistical concepts: means, mean differences, and error bars.
#' More importantly, it focuses on the effect size of one's
#' experiment/intervention, as opposed to a false dichotomy engendered
#' by \emph{P} values.
#'
#'
#' @param .data A tidy data.frame or tibble.
#' @param x,y Columns in the data.frame.
#' @param idx Categories in the column \code{x}.
#' @param paired boolean. If TRUE, the two groups are treated as paired
#' samples. The group first passed to \code{idx} is treated as pre-intervention
#' while the second group is considered post-intervention.
#' @param func function, default mean. This function will be applied to
#' the groups in \code{idx} individually, and the difference will be
#' saved as a single bootstrap resample.
#' @param ci float, default 0.95. The level of the confidence intervals
#' produced. The default \code{ci = 0.95} produces 95\% CIs.
#' @param reps integer, default 5000. The number of bootstrap resamples that
#' will be generated.
#' @param slopegraph boolean, default TRUE.
#' @param float.contrast default NULL
#' @param color.column default NULL
#' @param palette default "Set1"
#' @param theme default theme_classic()
#' @param tick.fontsize default 12
#' @param axes.title.fontsize default 15
#' @param swarm.label default NULL
#' @param es.label default NULL
#' @param esmarker.size default 3
#' @param rawplot.type default "beeswarm. Accepts any value found in
#' c("beeswarm", "sinaplot")
#' @param beeswarm.params default NULL. Supply list of \code{keyword = value}
#' pairs to \code{ggbeeswarm::geom_beeswarm()}.
#' @param sinaplot.params default NULL. Supply list of \code{keyword = value}
#' pairs to \code{ggforce::geom_sina()}.
#' @param slopegraph.params default NULL. Supply list of \code{keyword = value}
#' pairs to \code{ggplot2::geom_line()}. This controls the appearance of the
#' lines plotted for a paired slopegraph.
#'
#' @return a \code{ggplot} object.
#'
#' @section References:
#' Moving beyond P values: Everyday data analysis with estimation plots.
#' Joses Ho, Tayfun Tumkaya, Sameer Aryal, Hyungwon Choi, Adam Claridge-Chang (2018)
#'
#' \url{https://doi.org/10.1101/377978}
#'
#'
#' @examples
#' dabest.plot.dev(iris, x = Species, y = Petal.Width,
#'                 idx = c("setosa", "versicolor"))
#'
#' @export
dabest.plot.dev <- function(
  .data, x, y, idx, color.col = NULL, paired = FALSE, func = mean,
  ci = 0.95, reps = 5000, slopegraph = TRUE,
  float.contrast = NULL, color.column = NULL, palette = "Set1",
  theme = theme_classic(), tick.fontsize = 12, axes.title.fontsize = 15,
  swarm.label = NULL, es.label = NULL, esmarker.size = 3,
  rawplot.type = c("beeswarm", "sinaplot"),
  beeswarm.params = NULL,
  sinaplot.params = NULL,
  slopegraph.params = NULL) {


  # Create quosures and quonames to pass variables along properly.
  x_enquo         <-  enquo(x)
  x_quoname       <-  quo_name(x_enquo)

  y_enquo         <-  enquo(y)
  y_quoname       <-  quo_name(y_enquo)

  func_enquo      <-  enquo(func)
  func_quoname    <-  quo_name(func_enquo)

  color.col_enquo <-  enquo(color.col)


  # Get only the columns we need.
  data_for_plot <-
    as_tibble(data) %>%
    select(!!x_enquo, !!y_enquo, !!color.col_enquo) %>%
    filter(!!x_enquo %in% idx) %>%
    mutate(!!x_quoname := factor(!!x_enquo, levels = idx, ordered = TRUE))


  # Compute the Ns.
  Ns <- data_for_plot %>% group_by(!!x_enquo) %>% count()

  # Compute the summaries.
  summ_func <- quo(func(!!y_enquo))
  summaries <- data_for_plot %>% group_by(!!x_enquo) %>% summarize(!!summ_func)
  colnames(summaries) <- c(x_quoname, "summary")

  # Parse keywords of interest.
  if (length(idx) > 2) float.contrast <- FALSE

  if (identical(paired, FALSE)) slopegraph <- FALSE

  if (quo_is_null(color.col_enquo)) {
    color.aes          <-  aes(col = !!x_enquo)
  } else {
    color.col_quoname  <-  quo_name(color.col_enquo)
    color.aes          <-  aes(col = !!color.col_enquo)
  }

  # If rawplot is not specified, defaults to 'swarmplot'.
  if (length(rawplot.type) > 1) {
    rawplot.type <- rawplot.type[1]
  }

  if (identical(slopegraph, FALSE)) {

    if (rawplot.type == 'beeswarm') {
      if (is.null(beeswarm.params)) {
        beeswarm.params <- list(size = 1, alpha = 0.95, cex = 1,
                                mapping = color.aes)
      } else if (class(beeswarm.params) != "list") {
        stop("`beeswarm_params` is not a list.")
      } else beeswarm.params[['mapping']] = color.aes


    } else if (rawplot.type == 'sinaplot') {
      if (is.null(sinaplot.params)) {
        sinaplot.params <- list(mapping = color.aes)
      } else if (class(sinaplot.params) != "list") {
        stop("`sinaplot.params` is not a list.")
      } else sinaplot.params[['mapping']] = color.aes

    } else stop(paste(rawplot.type, "is not a recognized plot type. ",
                      "Accepted plot types: 'beeswarm' and 'sinaplot'. "))
  }


  # Compute the bootstrap difference.
  # TODO This needs to be done for each 'pair' of idx.
  control_groupname <- idx[1]
  test_groupname <- idx[2]
  control_summ <- filter(summaries, !!x_enquo == control_groupname)$summary
  test_summ <- filter(summaries, !!x_enquo == test_groupname)$summary


  # Compute bootstrapped results.
  boot.result <- bootdiff(data, !!x_enquo, !!y_enquo,
                          control_groupname, test_groupname,
                          paired = paired, ci = ci, func = func, reps = reps)
  # reassign func for output?
  boot.result$func <- func_quoname
  func_diff        <- boot.result$difference
  ci_low           <- boot.result$bca_ci_low
  ci_high          <- boot.result$bca_ci_high
  es_shift         <- test_summ - func_diff

  # munge bootstraps for plotting.
  boots            <- boot.result$bootstraps + es_shift
  labs             <- "delta"
  boots_data       <- tibble(labs, boots)


  # Plot!
  if (identical(paired, FALSE) | identical(slopegraph, FALSE)) {
    p <- ggplot(data = data_for_plot, aes(!!x_enquo, !!y_enquo)) +
      coord_cartesian(xlim = c(1, 2.3))


    if (rawplot.type == 'beeswarm') {
      p <- p + do.call(ggbeeswarm::geom_beeswarm, beeswarm.params)
    } else if (rawplot.type == 'sinaplot') {
      p <- p + do.call(ggforce::geom_sina, sinaplot.params)
    }


  } else if (identical(slopegraph, TRUE)) {
    # Munge the data.
    # ggplot plots in lexicographical (alhpabetical) order.
    # Perhaps in the future, we will set "Group" as an ordered factor.
    # But for now, this works internally.

    # First, pull out the control and test data as vectors.
    # Seems a bit messy...
    control <- filter(data_for_plot, !!x_enquo == idx[1])[[y_quoname]]
    test    <- filter(data_for_plot, !!x_enquo == idx[2])[[y_quoname]]

    if (quo_is_null(color.col_enquo)) {
      wide.paired.data <- tibble(a = control, z = test)
      for.slope.plot   <- tidyr::gather(wide.paired.data,
                                        key = !!x_enquo, value = !!y_enquo)
    } else {
      colcol <- filter(data_for_plot, !!x_enquo == control_groupname)
      colcol <- colcol[[color.col_quoname]]

      wide.paired.data <- tibble(a = control, z = test,
                                 !!color.col_quoname := colcol)
      for.slope.plot   <- tidyr::gather(wide.paired.data, key = !!x_enquo,
                                        value = !!y_enquo, -color.col_quoname)
    }


    for.slope.plot['Observation'] = rep(1: nrow(wide.paired.data), 2)


    if (quo_is_null(color.col_enquo)) {
      slope.mapping = aes(x = !!x_enquo, y = !!y_enquo,
                          group = Observation)
    } else {
      slope.mapping = aes(x = !!x_enquo, y = !!y_enquo,
                          group = Observation, col = !!color.col_enquo)
    }


    if (is.null(slopegraph.params)) {
      slopegraph.params <- list(size = 0.25, mapping = slope.mapping)
    } else {
      if (class(slopegraph.params) != "list") {
        stop("`slopegraph.params` is not a list.")
      }
      slopegraph.params[['mapping']] = slope.mapping
    }


    p <- ggplot(data = for.slope.plot) +
      coord_cartesian(xlim = c(1, 2.3)) +
      do.call(geom_line, slopegraph.params)
  }


  p <- p +
    scale_colour_brewer(palette = palette) +
    ## Plot the half-violin.
    geom_flat_violin(data = boots_data, aes(x = 2.5, y = boots),
                     adjust = 5, size = 0, width = 0.25) +
    ## Plot the effect size and CIs.
    geom_point(colour = "black", size = esmarker.size,
               aes(x = 2.5, y = test_summ) ) +
    geom_errorbar(colour = "black", width = 0, size = 0.3,
                  aes(x = 2.5,
                      ymin = ci_low + es_shift,
                      ymax = ci_high + es_shift) ) +
    ## Plot the summary lines for each group.
    geom_segment(color = "black", size = 0.2,
                 aes(x = 1, y = control_summ,
                     xend = 4, yend = control_summ) ) +
    geom_segment(color = "black", size = 0.2,
                 aes(x = 2, y = test_summ,
                     xend = 4, yend = test_summ) )

  # Label raw data axes.
  if (is.null(swarm.label)) {
    swarmylab <- y_enquo
    swarm.label <- stringr::str_interp("${y_quoname}\n")
  } else {
    swarm.label <- stringr::str_interp("${swarm.label}\n")
  }

  # Count and label Ns.
  # TODO create a loop for this for hub-and-spoke plots.
  control_length <- filter(Ns, !!x_enquo == control_groupname)[["n"]]
  test_length    <- filter(Ns, !!x_enquo == test_groupname)[["n"]]

  p <- p +
    ylab(swarm.label) +
    scale_x_discrete(
      labels=c(paste(control_groupname,"\nn =", control_length),
               paste(test_groupname,"\nn =", test_length),
               " ")
    )

  if (quo_is_null(color.col_enquo)) {
    p <- p + guides(colour = "none", fill = "none")
  }

  # Include paired status in effect size label.
  if (identical(boot.result$paired, TRUE)) is.p <- "Paired" else is.p <- "Unpaired"
  # Include the capitalised func for a prettier title.
  ff <- boot.result$func
  substr(ff, 1, 1) <- toupper(substr(ff, 1, 1))
  if (is.null(es.label)) {
    es.label <- stringr::str_interp("${is.p} ${ff}\nDifference\n")
  } else {
    es.label <- stringr::str_interp("${es.label}\n")
  }

  # Add secondary y-axis for floating side-by-side plots.
  if (control_summ > 0) {
    floataxes = sec_axis(trans = ~.-control_summ, name = es.label)
  } else {
    floataxes = sec_axis(trans = ~.+control_summ, name = es.label)
  }
  p <- p + scale_y_continuous(sec.axis = floataxes)


  # Add theme.
  p <- p + theme + theme(axis.line.x.bottom  = element_blank(),
                         axis.title.x.bottom = element_blank(),
                         axis.ticks.x.bottom = element_blank(),
                         axis.text = element_text(size = tick.fontsize),
                         axis.title = element_text(size = axes.title.fontsize),
                         axis.ticks.length = unit(7, "points"))


  # Return ggplot object!
  return(p)
}
