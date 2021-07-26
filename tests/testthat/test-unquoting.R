library(dplyr)
library(dabestr)
context("Test Unquoting")


test_that("Renaming `Species` in iris to `Groups` is kosher", {
  # Motivated by https://github.com/ACCLAB/dabestr/issues/35

  canonical <- iris %>%
    dabest(Species, Sepal.Length,
           c("virginica","setosa")) %>%
    mean_diff()

  renamed   <- iris %>%
    rename(group=Species) %>%
    dabest(group, Sepal.Length,
           c("virginica","setosa")) %>%
    mean_diff()

  # Check results.
  canonical.result <- canonical$result
  expect_equal(canonical.result$difference, -1.582)
  expect_equal(canonical.result$bca_ci_low, -1.788)
  expect_equal(canonical.result$bca_ci_high, -1.388295, tolerance = 1e-06)

  renamed.result <- renamed$result
  expect_equal(renamed.result$difference, -1.582)
  expect_equal(renamed.result$bca_ci_low, -1.788)
  expect_equal(renamed.result$bca_ci_high, -1.388295, tolerance = 1e-06)

})



test_that("Using `grp` as x column is kosher", {
  # Derived from https://github.com/ACCLAB/dabestr/issues/29

  # Run code here.
  set.seed(12345)
  df <- cbind(data.frame(value=sample(1:9, 100, replace = TRUE)/10),
              data.frame(grp=sample(c("A", "B"), 100, replace = TRUE))
              )
  df_analysed <- dabest(df, x = grp, y = value,
                        idx = c("A", "B")) %>%
                 mean_diff()

  # Check results.
  df_analysed.result <- df_analysed$result

  expect_match(as.character(df_analysed.result$control_group), "A")
  expect_match(as.character(df_analysed.result$test_group), "B")

  expect_equal(df_analysed.result$control_size, 54)
  expect_equal(df_analysed.result$test_size, 46)

  expect_equal(df_analysed.result$difference, -0.02769726)
  expect_equal(df_analysed.result$bca_ci_low, -0.1283135, tolerance = 1e-06)
  expect_equal(df_analysed.result$bca_ci_high, 0.0698794)

  gardner.altman.renamed <- plot(df_analysed)

  vdiffr::expect_doppelganger("Gardner Altman Renamed", gardner.altman.renamed)

})
