library(dabestr)
context("Test Loading and Bootstrap")



test_that("Two-group unpaired loads properly", {
  sample.size <- sample(50:100, 1) # random sample size.
  diff        <- sample(50:100, 1) # random difference in means.

  dummy.data  <- generate.two.groups(sampleN = sample.size,
                                     difference = diff)

  my.data <- dabest(dummy.data, Group, Value, paired = FALSE,
                        idx = c("Control", "Test"))


  # Check attributes are ok.
  my.data.class <- attr(my.data, "class")
  expect_match(my.data.class[1], "dabest")
  expect_match(my.data.class[2], "list")
})



test_that("Two-group unpaired mean diff parses properly", {
  sample.size <- sample(50:100, 1) # random sample size.
  diff        <- sample(50:100, 1) # random difference in means.

  dummy.data  <- generate.two.groups(sampleN = sample.size,
                                     difference = diff)

  my.data <- dabest(dummy.data, Group, Value, paired = FALSE,
                    idx = c("Control", "Test"))



  md.my.data <- my.data %>% mean_diff()

  report      <- md.my.data$result



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

  my.data <- dabest(dummy.data, Group, Value, paired = FALSE,
                    idx = c("Control", "Test"))

  for (s in seeds) {
    boot.result <- my.data %>% mean_diff(seed = s * s)

    report      <- boot.result$result

    if (diff < report$bca_ci_low || diff > report$bca_ci_high) {
      ci.failures <- ci.failures + 1
    }
  }

  max.errors <- ceiling((1 - (ci.for.test/100)) * length(seeds))

  expect_lte(ci.failures, max.errors)
})



test_that("Two-group paired reports accurately", {
  sample.size <- sample(50:100, 1) # random sample size.
  diff        <- sample(50:100, 1) # random difference in means.
  dummy.data  <- generate.two.groups(sampleN = sample.size,
                                     difference = diff)

  loaded <- dabest(dummy.data, Group, Value,
                        paired = TRUE, id.col = ID,
                        idx = c("Control", "Test"))

  expect_true(loaded$is.paired)
})




test_that("Two-group paired mean diff requires id.col", {
  expect_error(dabest(dummy.data, Group, Value, paired = TRUE,
                      # Missing `id.col` parameter.
                      idx = c("Control", "Test")),
               "`paired` is TRUE but no `id.col` was supplied.")

})
