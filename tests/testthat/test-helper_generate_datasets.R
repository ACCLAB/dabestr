testthat::test_that("Able to generate dataset for testing", {
  expect_no_error(generate_non_proportional_dataset())
  expect_no_error(generate_proportional_dataset())
  expect_no_error(generate_deltadelta_dataset())
})