# Test cases
describe("Testing print_each_comparism function", {
  test_that("Prints each comparison correctly for valid inputs", {
    # Test case: Valid inputs with list dabest_obj
    dd_dataset <- generate_deltadelta_dataset()
    dabest_obj <- dabestr::load(dd_dataset,
      x = Genotype, y = Measurement,
      delta2 = TRUE, experiment = Treatment,
      idx = list(c("W Placebo", "M Placebo"), c("W Drug", "M Drug")),
      colour = Genotype
    )
    cat_output <- capture_output(print_each_comparism(dabest_obj))
    expect_true(grepl("1. M Placebo minus W Placebo", cat_output, fixed = TRUE))
    expect_true(grepl("2. M Drug minus W Drug", cat_output, fixed = TRUE))
    expect_true(grepl("3. Drug minus Placebo (only for mean difference)", cat_output, fixed = TRUE))
  })

  test_that("Handles invalid effectsize gracefully", {
    # Test case: Invalid effectsize
    expect_error(print_each_comparism(),
      message = "argument 'dabest_obj' is missing, with no default"
    )
  })
})

# Test cases
describe("Testing print_each_comparism_effectsize function", {
  test_that("Prints each comparison correctly for valid inputs", {
    # Test case: Valid inputs with list dabest_effectsize_obj
    np_dataset <- generate_non_proportional_dataset()
    dabest_effectsize_obj <- dabestr::load(
      data = np_dataset, x = Group, y = Measurement, idx = list(
        c("Control 1", "Test 1"),
        c("Control 2", "Test 2")
      ),
      minimeta = TRUE
    ) %>% mean_diff()
    effectsize <- "mean_diff"
    expect_no_error(print_each_comparism_effectsize(dabest_effectsize_obj, effectsize))
  })

  test_that("Handles invalid effectsize gracefully", {
    # Test case: Invalid effectsize
    dabest_effectsize_obj <- list(
      idx = list("Group A", "Group B", "Group C"),
      paired = NULL
    )
    effectsize <- "invalid_effectsize"
    expect_error(print_each_comparism_effectsize(dabest_effectsize_obj, effectsize),
      message = "dabest_effectsize_obj must be a <dabest_effectsize> object"
    )
  })
})
