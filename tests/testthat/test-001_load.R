#### Loading of datasets
testthat::test_that("Able to load dataset", {
  np_dataset <- generate_non_proportional_dataset()
  expect_no_error(dabestr::load(np_dataset,
    x = Group, y = Measurement,
    idx = c("Control 1", "Test 1")
  ))

  p_dataset <- generate_proportional_dataset()
  expect_no_error(dabestr::load(p_dataset,
    x = Group, y = Success,
    idx = c("Control 1", "Test 1"), proportional = TRUE
  ))

  dd_dataset <- generate_deltadelta_dataset()
  expect_no_error(dabestr::load(dd_dataset,
    x = Genotype, y = Measurement,
    delta2 = TRUE, experiment = Treatment,
    idx = list(c("W Placebo", "M Placebo"), c("W Drug", "M Drug")),
    colour = Genotype
  ))
})

#### Detecting non-valid params
testthat::test_that("Able to detect non-valid params", {
  np_dataset <- generate_non_proportional_dataset()
  expect_error(
    dabestr::load(np_dataset,
      x = Grou, y = Measurement,
      idx = c("Control 1", "Test 1")
    ),
    regexp = "Column x is not in data"
  )
  expect_error(
    dabestr::load(np_dataset,
      x = Group, y = Measuremen,
      idx = c("Control 1", "Test 1")
    ),
    regexp = "Column y is not in data"
  )
  expect_error(
    dabestr::load(np_dataset,
      x = Group, y = Measurement,
      idx = c("Control 1", "Test 1"), id_col = I
    ),
    regexp = "Column id_col is not in data"
  )
  expect_error(
    dabestr::load(np_dataset,
      x = Group, y = Measurement,
      idx = c("Control 1", "Test 1"), colour = Grou
    ),
    regexp = "Column colour is not in data"
  )
  expect_error(
    dabestr::load(np_dataset,
      x = Group, y = Measurement,
      idx = c("Control 1")
    ),
    regexp = "does not consist of at least 2 groups"
  )
  expect_error(
    dabestr::load(np_dataset,
      x = Group, y = Measurement,
      idx = list(c("Control 1", "Test 1"), c("Control 2"))
    ),
    regexp = "does not consist of at least 2 groups"
  )
})

testthat::test_that("Able to detect non-valid params for proportional = TRUE", {
  np_dataset <- generate_non_proportional_dataset()
  expect_error(
    dabestr::load(np_dataset,
      x = Group, y = Measurement,
      idx = c("Control 1", "Test 1"), proportional = TRUE
    ),
    regexp = "data is not proportional"
  )
})

testthat::test_that("Able to detect non-valid params for is_paired = TRUE", {
  np_dataset <- generate_non_proportional_dataset()
  expect_error(
    dabestr::load(np_dataset,
      x = Group, y = Measurement,
      idx = c("Control 1", "Test 1", "Test 2"), paired = "baseline"
    ),
    regexp = "is TRUE but no id_col was supplied"
  )
  expect_error(
    dabestr::load(np_dataset,
      x = Group, y = Measurement,
      idx = c("Control 1", "Test 1", "Test 2"), paired = "some",
      id_col = ID
    ),
    regexp = "is not 'baseline' or 'sequential'."
  )
})

testthat::test_that("Able to detect non-valid params for minimeta = TRUE", {
  p_dataset <- generate_proportional_dataset()
  expect_error(
    dabestr::load(p_dataset,
      x = Group, y = Success,
      idx = c("Control 1", "Test 1"), id_col = ID,
      proportional = TRUE, minimeta = TRUE
    ),
    regexp = "proportional is TRUE but minimeta is also TRUE."
  )

  np_dataset <- generate_non_proportional_dataset()
  expect_error(
    dabestr::load(np_dataset,
      x = Group, y = Measurement,
      idx = c("Control 1", "Test 1"),
      delta2 = TRUE, minimeta = TRUE
    ),
    regexp = "delta2 is TRUE but minimeta is also TRUE."
  )
  expect_error(
    dabestr::load(np_dataset,
      x = Group, y = Measurement,
      idx = c("Control 1", "Test 1", "Test 2"),
      minimeta = TRUE
    ),
    regexp = "minimeta is TRUE, but some idx does not consist of exactly 2 groups"
  )
})

testthat::test_that("Able to detect non-valid params for delta2 = TRUE", {
  p_dataset <- generate_proportional_dataset()
  expect_error(
    dabestr::load(p_dataset,
      x = Group, y = Success,
      idx = c("Control 1", "Test 1"), id_col = ID,
      proportional = TRUE, delta2 = TRUE
    ),
    regexp = "delta2 is TRUE but proportional is also TRUE."
  )
})

#### Printing functions
testthat::test_that("Prints correct output for dabestr object", {
  #### 2GROUP ####
  np_dataset <- generate_non_proportional_dataset()
  dabest_obj <- dabestr::load(
    data = np_dataset, x = Group, y = Measurement, idx = c("Control 1", "Test 1")
  )
  expect_output(print(dabest_obj), regexp = "95% confidence intervals")
  expect_output(print(dabest_obj), regexp = "Test 1 minus Control 1")
  expect_output(print(dabest_obj), regexp = "5000 resamples")
  
  #### MULTIGROUP BASELINE ####
  dabest_obj <- dabestr::load(np_dataset,
                                  x = Group, y = Measurement,
                                  idx = list(c("Control 1", "Test 1"), c("Control 2", "Test 2", "Test 3")),
                                  paired = "baseline", id_col = ID
  ) 
  expect_output(print(dabest_obj), regexp = "Test 1 minus Control 1")
  expect_output(print(dabest_obj), regexp = "Test 2 minus Control 2")
  expect_output(print(dabest_obj), regexp = "Test 3 minus Control 2")
  
  #### MULTIGROUP SEQUENTIAL ####
  dabest_obj <- dabestr::load(np_dataset,
                              x = Group, y = Measurement,
                              idx = list(c("Control 1", "Test 1"), c("Control 2", "Test 2", "Test 3")),
                              paired = "sequential", id_col = ID
  ) 
  expect_output(print(dabest_obj), regexp = "Test 1 minus Control 1")
  expect_output(print(dabest_obj), regexp = "Test 2 minus Control 2")
  # expect_output(print(dabest_obj), regexp = "Test 3 minus Test 2")

  #### 2GROUP PROPORTION ####
  p_dataset <- generate_proportional_dataset()
  dabest_obj <- dabestr::load(
    data = p_dataset, x = Group, y = Success, idx = c("Control 2", "Test 2")
  )
  expect_output(print(dabest_obj), regexp = "95% confidence intervals")
  expect_output(print(dabest_obj), regexp = "Test 2 minus Control 2")
  expect_output(print(dabest_obj), regexp = "5000 resamples")

  #### DELTADELTA ####
  dd_dataset <- generate_deltadelta_dataset()
  dabest_obj <- dabestr::load(dd_dataset,
    x = Genotype, y = Measurement, delta2 = TRUE, experiment = Treatment,
    idx = list(c("W Placebo", "M Placebo"), c("W Drug", "M Drug")), colour = Genotype
  )
  expect_output(print(dabest_obj), regexp = "M Placebo minus W Placebo")
  expect_output(print(dabest_obj), regexp = "M Drug minus W Drug")
  expect_output(print(dabest_obj), regexp = "Drug minus Placebo")
  
  #### ADJUSTING CI ####
  dabest_obj <- dabestr::load(
    data = np_dataset, x = Group, y = Measurement, idx = c("Control 1", "Test 1"), ci = 85
  )
  expect_output(print(dabest_obj), regexp = "85% confidence intervals")
  expect_output(print(dabest_obj), regexp = "Test 1 minus Control 1")
  expect_output(print(dabest_obj), regexp = "5000 resamples")
  
  #### ADJUSTING RESAMPLES ####
  dabest_obj <- dabestr::load(
    data = np_dataset, x = Group, y = Measurement, idx = c("Control 1", "Test 1"), resamples = 3000
  )
  expect_output(print(dabest_obj), regexp = "95% confidence intervals")
  expect_output(print(dabest_obj), regexp = "Test 1 minus Control 1")
  expect_output(print(dabest_obj), regexp = "3000 resamples")
})
