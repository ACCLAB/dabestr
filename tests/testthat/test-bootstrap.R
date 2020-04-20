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

  expect_match(as.character(report$control_group), "Control")
  expect_equal(report$control_size, sample.size)

  expect_match(as.character(report$test_group), "Test")
  expect_equal(report$test_size, sample.size)

  expect_match(as.character(report$variable), "Value")

  expect_false(report$paired)
})



test_that("Two-group unpaired mean diff falls within 95 CI", {
  diff        <- sample(50: 100, 1)
  dummy.data  <- generate.two.groups(difference = diff)

  ci.for.test <- 95
  ci.failures <- 0
  seeds       <- seq(51: 100)

  for (s in seeds) {
    boot.result <- dabest(dummy.data, Group, Value, ci = ci.for.test,
                          paired = FALSE, idx = c("Control", "Test"),
                          seed = s * s)

    report      <- boot.result$result

    if (diff < report$bca_ci_low || diff > report$bca_ci_high) {
      ci.failures <- ci.failures + 1
    }
  }

  max.errors <- ceiling((1 - (ci.for.test/100)) * length(seeds))

  expect_lte(ci.failures, max.errors)
})



test_that("Two-group paired mean diff reports accurately", {
  sample.size <- sample(50:100, 1) # random sample size.
  diff        <- sample(50:100, 1) # random difference in means.

  dummy.data  <- generate.two.groups(sampleN = sample.size,
                                     difference = diff)

  boot.result <- dabest(dummy.data, Group, Value, paired = TRUE, id.col = ID,
                        idx = c("Control", "Test"))

  report      <- boot.result$result

  expect_true(report$paired)
})




test_that("Two-group paired mean diff requires id.col", {
  expect_error(dabest(dummy.data, Group, Value, paired = TRUE,
                      # Missing `id.col` parameter.
                      idx = c("Control", "Test")),
               "`paired` is TRUE but no `id.col` was supplied.")

})
