testthat::test_that("Able to apply all correct effect sizes to dabest_obj", {
  np_dataset <- generate_non_proportional_dataset()
  dabest_obj <- dabestr::load(data = np_dataset, x = Group, y = Measurement, idx = c("Control 1", "Test 1"))
  expect_no_error(dabestr::mean_diff(dabest_obj))
  expect_no_error(dabestr::median_diff(dabest_obj))
  expect_no_error(dabestr::cohens_d(dabest_obj))
  expect_no_error(dabestr::hedges_g(dabest_obj))
  expect_no_error(dabestr::cliffs_delta(dabest_obj))
  
  p_dataset <- generate_proportional_dataset()
  dabest_prop_obj <- dabestr::load(data = p_dataset, x = Group, y = Success, idx = c("Control 1", "Test 1"),
                              proportional = TRUE)
  expect_no_error(dabestr::cohens_h(dabest_prop_obj))
  expect_no_error(dabestr::mean_diff(dabest_prop_obj))
  
  dabest_mm_obj <- dabestr::load(data = np_dataset, x = Group, y = Measurement, idx = c("Control 1", "Test 1"),
                                   minimeta = TRUE)
  expect_no_error(dabestr::mean_diff(dabest_mm_obj))
})

testthat::test_that("Able to detect non-dabest_obj", {
  expect_error(dabestr::mean_diff("a"), "dabest_obj must be a <dabest> obj")
})

testthat::test_that("Able to detect non-valid effect sizes", {
  np_dataset <- generate_non_proportional_dataset()
  dabest_obj <- dabestr::load(data = np_dataset, x = Group, y = Measurement, idx = c("Control 1", "Test 1"),
                              paired = "sequential", id_col = ID)
  expect_error(dabestr::cliffs_delta(dabest_obj), "`Cliffs' delta` cannot be used when paired is not NULL.")
  
  p_dataset <- generate_proportional_dataset()
  dabest_prop_obj <- dabestr::load(data = p_dataset, x = Group, y = Success, idx = c("Control 1", "Test 1"),
                                   proportional = TRUE)
  expect_error(dabestr::median_diff(dabest_prop_obj), 
               "Other effect sizes besides `Cohens h` and `Mean difference` cannot be used when proportional is TRUE.")
  expect_error(dabestr::cohens_d(dabest_prop_obj), 
               "Other effect sizes besides `Cohens h` and `Mean difference` cannot be used when proportional is TRUE.")
  expect_error(dabestr::hedges_g(dabest_prop_obj),
               "Other effect sizes besides `Cohens h` and `Mean difference` cannot be used when proportional is TRUE.")
  expect_error(dabestr::cliffs_delta(dabest_prop_obj),
               "Other effect sizes besides `Cohens h` and `Mean difference` cannot be used when proportional is TRUE.")
})