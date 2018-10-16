library(dabestr)
context("Test bootstrap")

test_that("Two-group unpaired mean diff reports accurately", {
  sample.size <- sample(50:100, 1) # random sample size.
  diff        <- sample(50:100, 1) # random difference in means.

  dummy.data  <- generate.two.groups(sampleN = sample.size,
                                     difference = diff)

  boot.result <- dabest(dummy.data, Group, Value, paired = FALSE,
                        idx = c("Control", "Test"))

  report      <- boot.result$result

  expect_equal(report$control_group, "Control")
  expect_equal(report$control_size, sample.size)

  expect_equal(report$test_group, "Test")
  expect_equal(report$test_size, sample.size)

  expect_equal(report$variable, "Value")

  expect_false(report$paired)
})



test_that("Two-group unpaired mean diff falls within 95 CI", {
  diff        <- sample(50:100, 1)
  dummy.data  <- generate.two.groups(difference = diff)

  ci.failures <- 0

  for (i in seq(1: 100)) {
    boot.result <- dabest(dummy.data, Group, Value, paired = FALSE,
                          idx = c("Control", "Test"), seed = i * i)

    report      <- boot.result$result

    if (diff < report$bca_ci_low || diff > report$bca_ci_high) {
      ci.failures <- ci.failures + 1
    }
  }

  expect_lte(ci.failures, 5)
})
