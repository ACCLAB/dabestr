testthat::test_that("Able to generate dataset for testing", {
  expect_no_error(generate_non_proportional_dataset())
  expect_no_error(generate_proportional_dataset())
  expect_no_error(generate_deltadelta_dataset())
})

testthat::test_that("Able to generate dataset for testing with parameters", {
  expect_no_error(generate_non_proportional_dataset(N = 100))
  expect_no_error(generate_proportional_dataset(seed = 42))
  expect_no_error(generate_deltadelta_dataset(N = 38, seed = 300))
})
