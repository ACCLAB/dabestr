#### Apply valid effect sizes to dabest_obj
testthat::test_that("Able to apply all correct effect sizes to dabest_obj", {
  np_dataset <- generate_non_proportional_dataset()
  dabest_obj <- dabestr::load(data = np_dataset, x = Group, y = Measurement, idx = c("Control 1", "Test 1"))
  expect_no_error(dabestr::mean_diff(dabest_obj))
  expect_no_error(dabestr::median_diff(dabest_obj))
  expect_no_error(dabestr::cohens_d(dabest_obj))
  expect_no_error(dabestr::hedges_g(dabest_obj))
  expect_no_error(dabestr::cliffs_delta(dabest_obj))

  p_dataset <- generate_proportional_dataset()
  dabest_prop_obj <- dabestr::load(
    data = p_dataset, x = Group, y = Success, idx = c("Control 1", "Test 1"),
    proportional = TRUE
  )
  expect_no_error(dabestr::cohens_h(dabest_prop_obj))
  expect_no_error(dabestr::mean_diff(dabest_prop_obj))

  dabest_mm_obj <- dabestr::load(
    data = np_dataset, x = Group, y = Measurement, idx = c("Control 1", "Test 1"),
    minimeta = TRUE
  )
  expect_no_error(dabestr::mean_diff(dabest_mm_obj))
})

#### Detecting non-valid effect sizes for specific dabest_objs
testthat::test_that("Able to detect non-dabest_obj", {
  expect_error(dabestr::mean_diff("a"), "dabest_obj must be a <dabest> obj")
})

testthat::test_that("Able to detect non-valid effect sizes", {
  np_dataset <- generate_non_proportional_dataset()
  dabest_obj <- dabestr::load(
    data = np_dataset, x = Group, y = Measurement, idx = c("Control 1", "Test 1"),
    paired = "sequential", id_col = ID
  )
  expect_error(dabestr::cliffs_delta(dabest_obj), "`Cliffs' delta` cannot be used when paired is not NULL.")

  p_dataset <- generate_proportional_dataset()
  dabest_prop_obj <- dabestr::load(
    data = p_dataset, x = Group, y = Success, idx = c("Control 1", "Test 1"),
    proportional = TRUE
  )
  expect_error(
    dabestr::median_diff(dabest_prop_obj),
    "Other effect sizes besides `Cohens h` and `Mean difference` cannot be used when proportional is TRUE."
  )
  expect_error(
    dabestr::cohens_d(dabest_prop_obj),
    "Other effect sizes besides `Cohens h` and `Mean difference` cannot be used when proportional is TRUE."
  )
  expect_error(
    dabestr::hedges_g(dabest_prop_obj),
    "Other effect sizes besides `Cohens h` and `Mean difference` cannot be used when proportional is TRUE."
  )
  expect_error(
    dabestr::cliffs_delta(dabest_prop_obj),
    "Other effect sizes besides `Cohens h` and `Mean difference` cannot be used when proportional is TRUE."
  )
})

#### Printing functions
testthat::test_that("Prints correct output for dabest_effectsize_obj object", {
  #### 2GROUP ####
  np_dataset <- generate_non_proportional_dataset()
  dabest_effectsize_obj <- dabestr::load(
    data = np_dataset, x = Group, y = Measurement, idx = c("Control 1", "Test 1")
  ) %>% mean_diff()
  expect_output(print(dabest_effectsize_obj), regexp = "Test 1 and Control 1")
  expect_output(print(dabest_effectsize_obj), regexp = "5000 bootstrap samples")

  #### MULTIGROUP BASELINE ####
  dabest_effectsize_obj <- dabestr::load(np_dataset,
    x = Group, y = Measurement,
    idx = list(c("Control 1", "Test 1"), c("Control 2", "Test 2", "Test 3")),
    paired = "baseline", id_col = ID
  ) %>% mean_diff()
  expect_output(print(dabest_effectsize_obj), regexp = "Test 1 and Control 1")
  expect_output(print(dabest_effectsize_obj), regexp = "Test 2 and Control 2")
  expect_output(print(dabest_effectsize_obj), regexp = "Test 3 and Control 2")

  #### MULTIGROUP SEQUENTIAL ####
  dabest_effectsize_obj <- dabestr::load(np_dataset,
    x = Group, y = Measurement,
    idx = list(c("Control 1", "Test 1"), c("Control 2", "Test 2", "Test 3")),
    paired = "sequential", id_col = ID
  ) %>% mean_diff()
  expect_output(print(dabest_effectsize_obj), regexp = "Test 1 and Control 1")
  expect_output(print(dabest_effectsize_obj), regexp = "Test 2 and Control 2")
  expect_output(print(dabest_effectsize_obj), regexp = "Test 3 and Test 2")

  #### 2GROUP PROPORTION ####
  p_dataset <- generate_proportional_dataset()
  dabest_effectsize_obj <- dabestr::load(
    data = p_dataset, x = Group, y = Success, idx = c("Control 2", "Test 2"),
    proportional = TRUE
  ) %>% mean_diff()
  expect_output(print(dabest_effectsize_obj), regexp = "Test 2 and Control 2")

  #### MINIMETA ####
  np_dataset <- generate_non_proportional_dataset()
  dabest_effectsize_obj <- dabestr::load(
    data = np_dataset, x = Group, y = Measurement, idx = list(
      c("Control 1", "Test 1"),
      c("Control 2", "Test 2")
    ),
    minimeta = TRUE
  ) %>% mean_diff()
  expect_output(print(dabest_effectsize_obj), regexp = "Test 1 and Control 1")
  expect_output(print(dabest_effectsize_obj), regexp = "Test 2 and Control 2")

  #### DELTADELTA ####
  dd_dataset <- generate_deltadelta_dataset()
  dabest_effectsize_obj <- dabestr::load(dd_dataset,
    x = Genotype, y = Measurement, delta2 = TRUE, experiment = Treatment,
    idx = list(c("W Placebo", "M Placebo"), c("W Drug", "M Drug")), colour = Genotype
  ) %>% mean_diff()
  expect_output(print(dabest_effectsize_obj), regexp = "M Placebo and W Placebo")
  expect_output(print(dabest_effectsize_obj), regexp = "M Drug and W Drug")

  #### ADJUSTING CI ####
  dabest_effectsize_obj <- dabestr::load(
    data = np_dataset, x = Group, y = Measurement, idx = c("Control 1", "Test 1"), ci = 85
  ) %>% mean_diff()
  expect_output(print(dabest_effectsize_obj), regexp = "85%CI")

  #### ADJUSTING RESAMPLES ####
  dabest_effectsize_obj <- dabestr::load(
    data = np_dataset, x = Group, y = Measurement, idx = c("Control 1", "Test 1"), resamples = 3000
  ) %>% mean_diff()
  expect_output(print(dabest_effectsize_obj), regexp = "3000 bootstrap samples")

  #### CALCULATION OF PVALUES #####
})
