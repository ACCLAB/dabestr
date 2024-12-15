describe("Testing apply_effectsize function", {
  # Generate dataset
  dataset <- generate_non_proportional_dataset()

  # Create dabest_effectsize_obj
  dabest_obj <- load(
    data = dataset, x = Group, y = Measurement,
    idx = list(c("Control 1", "Test 1"), c("Control 2", "Test 2"), c("Control 3", "Test 3")),
    paired = "baseline", id_col = ID
  )
  contrast_objects <- list(dabest_obj, dabest_obj)
  # Test with mean_diff effect size
  test_that("mean_diff effect size works", {
    result <- apply_effectsize(contrast_objects, effect_size = "mean_diff")
    expect_true(inherits(result[[1]], "dabest_effectsize"))
    expect_true(inherits(result[[2]], "dabest_effectsize"))
    expect_true(!is.null(result[[1]]$is_paired))
  })

  # Test with median_diff effect size
  test_that("median_diff effect size works", {
    result <- apply_effectsize(contrast_objects, effect_size = "median_diff")
    expect_true(inherits(result[[1]], "dabest_effectsize"))
    expect_true(inherits(result[[2]], "dabest_effectsize"))
    expect_true(!is.null(result[[1]]$is_paired))
  })

  # Test with invalid effect size
  test_that("invalid effect size throws an error", {
    expect_error(apply_effectsize(contrast_objects, effect_size = "invalid_value"), "Invalid effect_size: invalid_value")
  })
})
