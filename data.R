#' LSAT Scores.
#'
#' This dataset is taken from Thomason et al. (2014) who studied a group of
#' 12 students who underwent a course in critical thinking. Their scores on
#' the Logical Reasoning section of the Law School Aptitude Test (LSAT) were
#' assesed before and after training.
#'
#' It is also found in Chapter 8 "The Paired Design"
#' (pp 195 - 199) of Introduction to the New Statistics
#' (Routledge, 2017), by Geoff Cumming and Robert Calin-Jageman.
#'
#' @format A list with two elements: \code{pretrain} and \code{posttrain}.
#'
#'
#' @source Thomason, N. R., Adajian, T., Barnett, A. E., Boucher, S.,
#' van der Brugge, E., Campbell, J., Knorpp, W., Lempert, R.,
#' Lengbeyer, L., Mandel, D. R., Rider, Y., van Gelder, T., & Wilkins, J. (2014).
#' \emph{Critical thinking final report}. The University of Melbourne,
#' N66001-12-C-2004.
"lsat_scores"



#' Transcription Scores.
#'
#' This dataset is taken from Mueller and Oppenheimer (2014), comparing the
#' percentage of notes that was verbatim transcribed during a lecture by two
#' independent groups of students: one using pen and paper, and one using
#' laptops.
#'
#' It is also found in Chapter 7 "The Independent Groups Design"
#' (pp 160 - 166) of Introduction to the New Statistics
#' (Routledge, 2017), by Geoff Cumming and Robert Calin-Jageman.
#'
#' @format A list with two elements: \code{pen} and \code{laptop}.
#'
#' @source Mueller, Pam A., and Daniel M. Oppenheimer. “The Pen Is Mightier
#' than the Keyboard: Advantages of Longhand over Laptop Note Taking.”
#' Psychological Science, vol. 25, no. 6, June 2014, pp. 1159–68,
#' doi:10.1177/0956797614524581.
"transcription_scores"



#' Wellbeing Scores (2 independent groups).
#'
#' This is a synthetic dataset from Geoff Cumming. 20 students were randomly
#' assigned to spend the afternoon reading in the library—the Control condition
#' —or reading in the local botanical gardens—the Test condition. At the end of
#' the session, each student completed a measure of his or her perceived
#' well-being.
#'
#' It is found in Chapter 11 "The Paired Design" (page 286) of
#' Understanding the New Statistics (Routledge, 2012) by Geoff Cumming.
#'
#' @format A list with two elements: \code{control} and \code{test}.
#'
#' @source Cumming, G. Understanding the New Statistics: Effect Sizes,
#' Confidence Intervals, and Meta-Analysis. Routledge 2012.
#' https://books.google.com/books?id=AVBDYgEACAAJ
"wellbeing_ind"



#' Wellbeing Scores (Before and after design).
#'
#' This is a synthetic dataset from Geoff Cumming. A single group of 10 students
#' first completed a well-being questionnaire (\code{before}), spent the
#' afternoon reading in the botanical gardens, then gave well-being scores once
#' again (\code{after}).
#'
#' It is found in Chapter 11 "The Paired Design" (page 291) of
#' Understanding the New Statistics (Routledge, 2012) by Geoff Cumming.
#'
#' @format A list with two elements: \code{before} and \code{after}.
#'
#' @source Cumming, G. Understanding the New Statistics: Effect Sizes,
#' Confidence Intervals, and Meta-Analysis. Routledge 2012.
#' https://books.google.com/books?id=AVBDYgEACAAJ
"wellbeing_paired"
