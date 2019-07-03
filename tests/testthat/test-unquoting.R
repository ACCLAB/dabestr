library(dplyr)
library(dabestr)
context("Test Unquoting")


test_that("Renaming `Species` in iris to `Groups` is kosher", {
  # Motivated by https://github.com/ACCLAB/dabestr/issues/35

  # Run code here.
  canonical <- iris %>%
    dabest(Species, Sepal.Length,
           c("virginica","setosa"))

  renamed   <- iris %>%
    rename(group=Species) %>%
    dabest(group, Sepal.Length,
           c("virginica","setosa"))

  # Check results.
  canonical.result <- canonical$result
  expect_equal(canonical.result$difference, -1.582)
  expect_equal(canonical.result$bca_ci_low, -1.778)
  expect_equal(canonical.result$bca_ci_high, -1.378)

  renamed.result <- renamed$result
  expect_equal(renamed.result$difference, -1.582)
  expect_equal(renamed.result$bca_ci_low, -1.778)
  expect_equal(renamed.result$bca_ci_high, -1.378)

})



test_that("Using `grp` as x column is kosher", {
  # Derived from https://github.com/ACCLAB/dabestr/issues/29

  # Run code here.
  set.seed(12345)
  df <- cbind(data.frame(value=sample(1:9, 100, replace = TRUE)/10),
              data.frame(grp=sample(c("A","B"), 100, replace = T))
              )
  df_analysed <- dabest(df, x = grp, y = value, idx = c("A", "B"))

  # Check results.
  df_analysed.result <- df_analysed$result

  expect_match(df_analysed.result$control_group, "A")
  expect_match(df_analysed.result$test_group, "B")

  expect_equal(df_analysed.result$control_size, 54)
  expect_equal(df_analysed.result$test_size, 46)

  expect_equal(df_analysed.result$difference, -0.02769726)
  expect_equal(round(df_analysed.result$bca_ci_low, digits=4), -0.1253)
  expect_equal(df_analysed.result$bca_ci_high, 0.07162217)

  gardner.altman.renamed <- plot(df_analysed)

  vdiffr::expect_doppelganger("Gardner Altman Renamed", gardner.altman.renamed)

})
