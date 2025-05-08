describe("Testing effect_size functions", {
  # pre-loading all dataset types
  np_dataset <- generate_non_proportional_dataset()
  p_dataset <- generate_proportional_dataset()

  describe("Given valid params", {
    test_that("it should create dabest_effsize_obj with no issue", {
      dabest_obj <- dabestr::load(data = np_dataset, x = Group, y = Measurement, idx = c("Control 1", "Test 1"))
      expect_no_error(dabestr::mean_diff(dabest_obj))
      expect_no_error(dabestr::median_diff(dabest_obj))
      expect_no_error(dabestr::cohens_d(dabest_obj))
      expect_no_error(dabestr::hedges_g(dabest_obj))
      expect_no_error(dabestr::cliffs_delta(dabest_obj))
    })

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

  describe("Given non-valid params", {
    test_that("it should detect invalid dabest_obj", {
      expect_error(dabestr::mean_diff("a"), "dabest_obj must be a <dabest> obj")
    })

    describe("Given paired NOT NULL in dabest", {
      dabest_obj <- dabestr::load(
        data = np_dataset, x = Group, y = Measurement, idx = c("Control 1", "Test 1"),
        paired = "sequential", id_col = ID
      )

      test_that("it should raise error for cliffs_delta", {
        expect_error(dabestr::cliffs_delta(dabest_obj), "`Cliffs' delta` cannot be used when paired is not NULL.")
      })
    })

    describe("Given proportional = TRUE in dabest_obj", {
      p_dataset <- generate_proportional_dataset()
      dabest_prop_obj <- dabestr::load(
        data = p_dataset, x = Group, y = Success, idx = c("Control 1", "Test 1"),
        proportional = TRUE
      )

      test_that("it should raise error when effect sizes other than cohens_h and mean_diff are used", {
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
    })
  })
})

describe("Testing print.dabest_effectsize function", {
  # pre-loading all dataset types
  np_dataset <- generate_non_proportional_dataset()
  p_dataset <- generate_proportional_dataset()
  dd_dataset <- generate_deltadelta_dataset()

  describe("Given it is a two group plot", {
    #### 2GROUP ####
    dabest_effectsize_obj <- dabestr::load(
      data = np_dataset, x = Group, y = Measurement, idx = c("Control 1", "Test 1")
    ) %>% mean_diff()

    test_that("it should print the correct values", {
      expect_output(print(dabest_effectsize_obj), regexp = "Test 1 and Control 1")
      expect_output(print(dabest_effectsize_obj), regexp = "5000 bootstrap samples")
    })
  })

  describe("Given it is a multigroup plot", {
    describe("Given that the data is paired with baseline", {
      #### MULTIGROUP BASELINE ####
      dabest_effectsize_obj <- dabestr::load(np_dataset,
        x = Group, y = Measurement,
        idx = list(c("Control 1", "Test 1"), c("Control 2", "Test 2", "Test 3")),
        paired = "baseline", id_col = ID
      ) %>% mean_diff()

      test_that("it should print the correct values", {
        expect_output(print(dabest_effectsize_obj), regexp = "Test 1 and Control 1")
        expect_output(print(dabest_effectsize_obj), regexp = "Test 2 and Control 2")
        expect_output(print(dabest_effectsize_obj), regexp = "Test 3 and Control 2")
      })
    })

    describe("Given that the data is paired sequentially", {
      #### MULTIGROUP SEQUENTIAL ####
      dabest_effectsize_obj <- dabestr::load(np_dataset,
        x = Group, y = Measurement,
        idx = list(c("Control 1", "Test 1"), c("Control 2", "Test 2", "Test 3")),
        paired = "sequential", id_col = ID
      ) %>% mean_diff()

      test_that("it should print the correct values", {
        expect_output(print(dabest_effectsize_obj), regexp = "Test 1 and Control 1")
        expect_output(print(dabest_effectsize_obj), regexp = "Test 2 and Control 2")
        expect_output(print(dabest_effectsize_obj), regexp = "Test 3 and Test 2")
      })
    })
  })

  describe("Given it is a proportional plot", {
    #### 2GROUP PROPORTION ####
    dabest_effectsize_obj <- dabestr::load(
      data = p_dataset, x = Group, y = Success, idx = c("Control 2", "Test 2"),
      proportional = TRUE
    ) %>% mean_diff()

    test_that("it should print the correct values", {
      expect_output(print(dabest_effectsize_obj), regexp = "Test 2 and Control 2")
    })
  })

  describe("Given it is a minimeta plot", {
    #### MINIMETA ####
    dabest_effectsize_obj <- dabestr::load(
      data = np_dataset, x = Group, y = Measurement, idx = list(
        c("Control 1", "Test 1"),
        c("Control 2", "Test 2")
      ),
      minimeta = TRUE
    ) %>% mean_diff()

    test_that("it should print the correct values", {
      expect_output(print(dabest_effectsize_obj), regexp = "Test 1 and Control 1")
      expect_output(print(dabest_effectsize_obj), regexp = "Test 2 and Control 2")
    })
  })

  describe("Given it is a deltadelta plot", {
    #### DELTADELTA ####
    dabest_effectsize_obj <- dabestr::load(dd_dataset,
      x = Genotype, y = Measurement, delta2 = TRUE, experiment = Treatment,
      idx = list(c("W Placebo", "M Placebo"), c("W Drug", "M Drug")), colour = Genotype
    ) %>% mean_diff()

    test_that("it should print the correct values", {
      expect_output(print(dabest_effectsize_obj), regexp = "M Placebo and W Placebo")
      expect_output(print(dabest_effectsize_obj), regexp = "M Drug and W Drug")
    })
  })

  describe("Given ci = 85 in dabest object", {
    #### ADJUSTING CI ####
    dabest_effectsize_obj <- dabestr::load(
      data = np_dataset, x = Group, y = Measurement, idx = c("Control 1", "Test 1"), ci = 85
    ) %>% mean_diff()

    test_that("it should print the correct values", {
      expect_output(print(dabest_effectsize_obj), regexp = "85%CI")
    })
  })

  describe("Given resamples = 3000 in dabest object", {
    #### ADJUSTING RESAMPLES ####
    dabest_effectsize_obj <- dabestr::load(
      data = np_dataset, x = Group, y = Measurement, idx = c("Control 1", "Test 1"), resamples = 3000
    ) %>% mean_diff()

    test_that("it should print the correct values", {
      expect_output(print(dabest_effectsize_obj), regexp = "3000 bootstrap samples")
    })
  })
})
