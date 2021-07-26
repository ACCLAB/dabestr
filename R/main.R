
#'Prepare Data for Analysis with dabestr
#'
#'\code{dabest} prepares a
#'\href{https://vita.had.co.nz/papers/tidy-data.pdf}{tidy dataset} for analysis
#'using estimation statistics.
#'
#'Estimation statistics is a statistical framework that focuses on effect sizes
#'and confidence intervals around them, rather than \emph{P} values and
#'associated dichotomous hypothesis testing.
#'
#'\code{dabest}() collates the data in preparation for the computation of
#'\link[=mean_diff]{effect sizes}. Bootstrap resampling is used to compute
#'non-parametric assumption-free confidence intervals. Visualization of the
#'effect sizes and their confidence intervals using estimation plots is then
#'performed with a specialized \link[=plot.dabest_effsize]{plotting} function.
#'
#'
#'@param .data A data.frame or tibble.
#'
#'@param x,y Columns in \code{.data}.
#'
#'@param idx A vector containing factors or strings in the \code{x} columns.
#'  These must be quoted (ie. surrounded by quotation marks). The first element
#'  will be the control group, so all differences will be computed for every
#'  other group and this first group.
#'
#'@param paired Boolean, default FALSE. If TRUE, the two groups are treated as
#'  paired samples. The first group is treated as pre-intervention and the
#'  second group is considered post-intervention.
#'
#'@param id.column Default NULL. A column name indicating the identity of the
#'  datapoint if the data is paired. \emph{This must be supplied if paired is
#'  \code{TRUE}.}
#'
#'
#'@return A \code{dabest} object with 8 elements.
#'
#'  \describe{
#'
#'  \item{\code{data}}{ The dataset passed to \code{\link{dabest}}, stored here
#'  as a \code{\link[tibble]{tibble}}. }
#'
#'  \item{\code{x} and \code{y}}{ The columns in \code{data} used to plot the x
#'  and y axes, respectively, as supplied to \code{\link{dabest}}. These are
#'  \href{https://adv-r.hadley.nz/quasiquotation.html}{quoted variables} for
#'  \href{https://tidyeval.tidyverse.org/}{tidy evaluation} during the
#'  computation of effect sizes. }
#'
#'  \item{\code{idx}}{ The vector of control-test groupings. For each pair in
#'  \code{idx}, an effect size will be computed by downstream \code{dabestr}
#'  functions used to compute \link[=mean_diff]{effect sizes} (such as
#'  \code{mean_diff()}.  }
#'
#'  \item{\code{is.paired}}{ Whether or not the experiment consists of paired
#'  (aka repeated) observations. }
#'
#'  \item{\code{id.column}}{ If \code{is.paired} is \code{TRUE}, the column in
#'  \code{data} that indicates the pairing of observations. }
#'
#'  \item{\code{.data.name}}{ The variable name of the dataset passed to
#'  \code{\link{dabest}}. }
#'
#'  \item{\code{.all.groups}}{ All groups as indicated in the \code{idx}
#'  argument. }
#'
#'  }
#'
#'
#'
#' @seealso \itemize{
#'
#'  \item \link[=mean_diff]{Effect size computation} from the loaded data.
#'
#'  \item \link[=plot.dabest_effsize]{Generating estimation plots} after effect size computation.
#'
#'  }
#'
#'
#'
#' @examples
#' # Performing unpaired (two independent groups) analysis.
#' unpaired_mean_diff <- dabest(iris, Species, Petal.Width,
#'                              idx = c("setosa", "versicolor"),
#'                              paired = FALSE)
#'
#' # Display the results in a user-friendly format.
#' unpaired_mean_diff
#'
#' # Compute the mean difference.
#' mean_diff(unpaired_mean_diff)
#'
#' # Plotting the mean differences.
#' mean_diff(unpaired_mean_diff) %>% plot()
#'
#' # Performing paired analysis.
#' # First, we munge the `iris` dataset so we can perform a within-subject
#' # comparison of sepal length vs. sepal width.
#'
#' new.iris     <- iris
#' new.iris$ID  <- 1: length(new.iris)
#' setosa.only  <-
#'   new.iris %>%
#'   tidyr::gather(key = Metric, value = Value, -ID, -Species) %>%
#'   dplyr::filter(Species %in% c("setosa"))
#'
#' paired_mean_diff <- dabest(setosa.only, Metric, Value,
#'                            idx = c("Sepal.Length", "Sepal.Width"),
#'                            paired = TRUE, id.col = ID) %>%
#'                     mean_diff()
#'
#'
#'
#' # Using pipes to munge your data and then passing to `dabest`.
#' # First, we generate some synthetic data.
#' set.seed(12345)
#' N        <- 70
#' c         <- rnorm(N, mean = 50, sd = 20)
#' t1        <- rnorm(N, mean = 200, sd = 20)
#' t2        <- rnorm(N, mean = 100, sd = 70)
#' long.data <- tibble::tibble(Control = c, Test1 = t1, Test2 = t2)
#'
#' # Munge the data using `gather`, then pass it directly to `dabest`
#'
#' meandiff <- long.data %>%
#'               tidyr::gather(key = Group, value = Measurement) %>%
#'               dabest(x = Group, y = Measurement,
#'                      idx = c("Control", "Test1", "Test2"),
#'                      paired = FALSE) %>%
#'               mean_diff()
#'
#'
#'
#'@importFrom magrittr %>%
#'@importFrom stringr str_interp
#'@importFrom rlang as_name enquo quo_is_null
#'@importFrom tibble as_tibble
#'@importFrom dplyr select arrange filter
#'@export
dabest <- function(
  .data, x, y, idx, paired = FALSE, id.column = NULL) {

  #### Create quosures and quonames. ####
  data_enquo     <- enquo(.data)
  data_quoname   <- as_name(data_enquo)

  x_enquo        <-  enquo(x)
  x_quoname      <-  as_name(x_enquo)

  y_enquo        <-  enquo(y)
  y_quoname      <-  as_name(y_enquo)

  id.col_enquo   <-  enquo(id.column)


  if (identical(paired, TRUE) & quo_is_null(id.col_enquo)) {
    stop("`paired` is TRUE but no `id.col` was supplied.")
  }



  #### Get only the columns we need. ####
  data_for_diff <-
    as_tibble(.data) %>%
    select(!!x_enquo, !!y_enquo, !!id.col_enquo)



  #### Handled if paired. ####
  if (isTRUE(paired)) {
    id.col_quoname <-  as_name(id.col_enquo)
    # sort the data by id.col so we can be sure all the observations match up.
    data_for_diff  <-
      data_for_diff %>% arrange(!!x_enquo, !!id.col_enquo)
  }



  #### Decide if multiplot or not. ####
  if (class(idx) == "character") {
    # Not multiplot. Add it to an empty list.
    group_list  <-  list(idx)
    all.groups  <-  idx

  } else if (class(idx) == "list") {
    # This is a multiplot. Give it a new name.
    group_list  <-  idx
    all.groups  <-  unique(unlist(group_list)) # Flatten `group_list`.
  }


  #### Assemble only the data used to create the plot. ####
  data.out <- .data

  # New in v0.2.1 patch.
  # Basically, the `ellipsis` package has been updated,
  # and now forcats::as_factor() should only take the object to coerce.
  data.out[[x_quoname]] <- forcats::as_factor(data.out[[x_quoname]])

  data.out <- filter(data.out, !!x_enquo %in% all.groups)



  #### Collate output. ####
  out = list(
    data        = data.out,
    x           = x_enquo,
    y           = y_enquo,
    idx         = group_list,
    is.paired   = paired,
    id.column   = id.col_enquo,
    .data.name  = data_quoname,
    .all.groups = all.groups
  )



  #### Append the custom class `dabest`. ####
  class(out) <- c("dabest", "list")


  #### Return the output. ####
  return(out)
}




#' Print a `dabest` object
#'
#' @param x A \code{\link{dabest}} object, generated by the function of the same name.
#'
#' @param ... S3 signature for generic plot function.
#'
#' @return A summary of the experimental designs.
#'
#' @examples
#' # Performing unpaired (two independent groups) analysis.
#' unpaired_mean_diff <- dabest(iris, Species, Petal.Width,
#'                              idx = c("setosa", "versicolor"),
#'                              paired = FALSE)
#'
#' # Display the results in a user-friendly format.
#' print(unpaired_mean_diff)
#'
#' @export
print.dabest <- function(x, ...) {

  #### Check object class ####
  if (class(x)[1] != "dabest") {
    stop(paste(
      "The object you are plotting is not a `dabest` class object. ",
      "Please check again! "))
  } else {
    dabest.object <- x
  }

  #### Print greeting header. ####
  print_greeting_header()

  #### Print head of data? ####
  cat(str_interp("Dataset    :  ${dabest.object$.data.name}\n"))
  cat("The first five rows are:\n")
  print(head(dabest.object$data, n = 5L))
  cat("\n")

  #### Print xvar and yvar. ####
  xvar = as_name(dabest.object$x)
  yvar = as_name(dabest.object$y)

  cat(str_interp("X Variable :  ${xvar}\n"))
  cat(str_interp("Y Variable :  ${yvar}\n\n"))

  #### Loop thru the groups and print out the comparisons to be done. ####
  if (dabest.object$is.paired) {
    es = "Paired effect size(s)"
  } else {
    es = "Effect sizes(s)"
  }
  cat(es, "will be computed for:\n")

  print_each_comparison(dabest.object)

  cat("\n")
}



print_greeting_header <- function() {
  now = Sys.time()

  now.hour <- as.integer(strftime(now, "%H"))

  if (0 < now.hour & now.hour < 12) {
    greeting = "Good morning!\n"
  } else if (12 < now.hour & now.hour < 18) {
    greeting = "Good afternoon!\n"
  } else {
    greeting = "Good evening!\n"
  }


  dabest_ver <- utils::packageVersion("dabestr")
  header     <- str_interp(
    "dabestr (Data Analysis with Bootstrap Estimation in R) v${dabest_ver}\n")

  cat(header)

  cat(rep('=', nchar(header) - 1), sep='')
  cat("\n\n")

  cat(greeting)
  cat("The current time is", strftime(now, "%R %p on %A %B %d, %Y."))
  cat("\n\n")
}



print_each_comparison <- function(dabest.object, ...) {

  i <- 1

  for (group in dabest.object$idx) {
    # Get test groups (everything else in group), loop through them and compute
    # the difference between group[1] and each group.
    # Test groups are the 2nd element of group onwards.

    control_group = group[1]
    test_groups <- group[2: length(group)]

    for (current_test_group in test_groups) {
      cat(str_interp("  ${i}. ${current_test_group} minus ${control_group}\n"))
      i <- i + 1
    }
  }

}
