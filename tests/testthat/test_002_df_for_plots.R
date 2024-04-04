
testthat::test_that("Testing create_dfs_for_proportion_bar function", {
  proportion_success <- c(0.5)
  result <- create_dfs_for_proportion_bar(proportion_success)
  expect_type(result, "list")
  expect_equal(nrow(result), 4)
  expect_equal(ncol(result), 5)
  expect_equal(sum(is.na(result)), 0)
  
  # Test case 2: proportion_success contains 0 and 1
  proportion_success <- c(0, 1)
  result <- create_dfs_for_proportion_bar(proportion_success)
  expect_type(result, "list")
  expect_equal(nrow(result), 8)
  expect_equal(ncol(result), 5)
  expect_equal(sum(is.na(result)), 0)
})

test_that("Throws an error when proportion_success contains values outside [0, 1]", {
  # Test case: proportion_success contains a value outside [0, 1]
  proportion_success <- c(0.5, 1.5, 0.75)
  expect_error(create_dfs_for_proportion_bar(proportion_success), 
               message = "proportion_success values must be between 0 and 1")
})

test_that("Returns a data frame with correct dimensions and values for valid inputs", {
  # Test case: Valid inputs
  boots <- list(replicate(5, rnorm(100)), replicate(5, rnorm(100)), replicate(5, rnorm(100)))
  x_idx_position <- c(1, 2, 3)
  float_contrast <- TRUE
  result <- create_dfs_for_baseline_ec_violin(boots, x_idx_position, float_contrast)
  expect_is(result, "data.frame")
  expect_equal(nrow(result), 1537) # 512 * 3 (number of x_idx_position) +1
  expect_equal(ncol(result), 3)
})

test_that("Throws an error for invalid inputs", {
  # Test case: Invalid inputs (e.g., boots not provided)
  expect_error(create_dfs_for_baseline_ec_violin(), 
               message = "argument 'boots' is missing, with no default")
})
