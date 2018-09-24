
#' Estimation Plot
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
#' @param data A tidy data.frame.
#' @param x,y Columns in the data.frame.
#' @param idx Categories in the column \code{x}.
#' @param paired boolean. If TRUE, the two groups are treated as paired
#' samples. The group first passed to \code{idx} is treated as pre-intervention
#' while the second group is considered post-intervention.
#' @param ci float, default 0.95. The level of the confidence intervals
#' produced. The default \code{ci = 0.95} produces 95 percent CIs.
#' @param reps integer, default 5000. The number of bootstrap resamples that
#' will be generated.
#' @param fun function, default mean. This function will be applied to
#' the groups in \code{idx} individually, and the difference will be
#' saved as a single bootstrap resample.
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
#' # Coming soon!
#'
#' @export
dabest.plot <- function(data, x, y, idx, fun = mean, reps = 5000) {
  df_plot <- df[which(df[[x]] %in% idx), c(x, y)]

  # if num. of groups compared is two
  if (length(idx) == 2){
    control_groupname <- idx[1]
    test_groupname <- idx[2]

    # Get ctrl and exp groups data
    ctrl <- df_plot[which(df_plot[[x]] == control_groupname), ][[y]]
    test  <- df_plot[which(df_plot[[x]] == test_groupname), ][[y]]
    df_summaries <- data.frame(l = test, t = ctrl)

    # Compute bootstrapped results.
    boot.result <- bootdiff(control = ctrl, test = test, paired = FALSE,
                            func = fun, reps = reps)
    func_diff <- boot.result$difference

    bootstrap_name <- "bootstraps"
    bootstraps <- data.frame(bootstrap_name, boot.result$bootstraps)

    TwoGroups <- ggplot(data = df_plot, mapping = aes(x, y)) +
      ggforce::geom_sina(aes(col = x)) +
      geom_flat_violin(data = bootstraps,
                       aes(x = bootstrap_name, y = func_diff,
                           fill = bootstrap_name),
                       adjust = 1, size = 0) +
      guides(col=FALSE, fill=FALSE)+
      geom_point(aes(x = 'delta', y = fun(test)), colour = "black") +
      cowplot::theme_cowplot() +
      geom_errorbar(aes(x = 'delta', ymin = fun(test)-1, ymax = fun(test)+1),
                    colour = "black", width = 0, size = 0.2) +
      geom_segment(aes(x = 1, y = fun(ctrl), xend = 4, yend = fun(ctrl)),
                   color = "black", size = 0.2) +
      geom_segment(aes(x = 2, y = fun(test), xend = 4, yend = fun(test)),
                   color = "black", size = 0.2) +
      theme(axis.line.x.bottom  = element_blank(),
            axis.title.x.bottom = element_blank(),
            axis.ticks.x.bottom = element_blank()) +
      scale_x_discrete(labels=c(control_groupname,
                                test_groupname," "))

    ## Shifting the secondary y-axis up or down,
    ## depending on the mean_diff is positive or neg.
    if (fun(ctrl) > 0) {
      p1 <- TwoGroups +
        scale_y_continuous(sec.axis = sec_axis(~.-mean(ctrl), name = "Delta ES")
                           )
    } else {
      p1 <- TwoGroups +
        scale_y_continuous(sec.axis = sec_axis(~.+mean(ctrl), name = "Delta ES")
                           )
    }
    p2 <- NULL
    # print("OK")
  }


  else if(length(idx) > 2) {
    ## Above-below plots
    MultipleGroupsScatter <- ggplot(data = df,
                                    mapping=aes(labs, dat, col=labs)) +
                            geom_sina() + cowplot::theme_cowplot() +
                            guides(col=FALSE)

    MultipleGroupsBelow <- ggplot(data = df,
                                  mapping = aes(labs, dat, fill=labs)) +
                            geom_flat_violin(adjust = 1,size=0) +
                            geom_point(data = df_summaries,
                                       mapping= aes(x=l,y=t),
                                       inherit.aes = FALSE) +
                            geom_errorbar(data = df_summaries,
                                          mapping = aes(x = l,
                                                        ymin = t-0.5,
                                                        ymax = t+0.5),
                                          colour = "black", width = 0,
                                          size = 0.4, inherit.aes = FALSE) +
                            cowplot::theme_cowplot() +
                            guides(fill = FALSE)

    p2 <- MultipleGroupsScatter + MultipleGroupsBelow +
      patchwork::plot_layout(nrow=2)
    p1 <- NULL
  }
  return(list(p1, p2))
}
