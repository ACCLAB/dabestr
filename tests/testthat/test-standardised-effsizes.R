library(dabestr)
context("Test Standardized Effect Sizes")


test_that("Transcription scores Hedges' g", {
  attach(transcription_scores)

  dat <- tibble::tibble(method = c(rep('pen',  length(transcription_scores$pen)),
                                   rep('laptop', length(transcription_scores$laptop))
                               ),
                    scores = c(transcription_scores$pen,
                               transcription_scores$laptop)
                    )

  tscores <- dabest(dat, method, scores,
                    idx = c("pen", "laptop"))

  tscores_g <- tscores %>% hedges_g()
  result <- tscores_g$result

  expect_equal(result$difference, 0.9259595, tolerance=1e-07)
})
