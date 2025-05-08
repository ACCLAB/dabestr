describe("Testing load function", {
  describe("Given valid params", {
    describe("Given non proportional dataset", {
      np_dataset <- generate_non_proportional_dataset()

      test_that("it should load dataset", {
        expect_no_error(dabestr::load(np_dataset,
          x = Group, y = Measurement,
          idx = c("Control 1", "Test 1")
        ))
      })
    })

    describe("Given proportional dataset", {
      p_dataset <- generate_proportional_dataset()

      test_that("it should load dataset", {
        expect_no_error(dabestr::load(p_dataset,
          x = Group, y = Success,
          idx = c("Control 1", "Test 1"), proportional = TRUE
        ))
      })
    })

    describe("Given deltadelta-like dataset", {
      dd_dataset <- generate_deltadelta_dataset()

      test_that("it should load dataset", {
        expect_no_error(dabestr::load(dd_dataset,
          x = Genotype, y = Measurement,
          delta2 = TRUE, experiment = Treatment,
          idx = list(c("W Placebo", "M Placebo"), c("W Drug", "M Drug")),
          colour = Genotype
        ))
      })
    })
  })

  describe("Given non-valid params", {
    # since this is more of an integration test, this unit test logic is repeated in test_001_utils.R
    np_dataset <- generate_non_proportional_dataset()
    p_dataset <- generate_proportional_dataset()

    test_that("it should detect invalid x value", {
      expect_error(
        dabestr::load(np_dataset,
          x = Grou, y = Measurement,
          idx = c("Control 1", "Test 1")
        ),
        regexp = "Column x is not in data"
      )
    })

    test_that("it should detect invalid y value", {
      expect_error(
        dabestr::load(np_dataset,
          x = Group, y = Measuremen,
          idx = c("Control 1", "Test 1")
        ),
        regexp = "Column y is not in data"
      )
    })

    test_that("it should detect invalid id_col value", {
      expect_error(
        dabestr::load(np_dataset,
          x = Group, y = Measurement,
          idx = c("Control 1", "Test 1"), id_col = I
        ),
        regexp = "Column id_col is not in data"
      )
    })

    test_that("it should detect invalid colour value", {
      expect_error(
        dabestr::load(np_dataset,
          x = Group, y = Measurement,
          idx = c("Control 1", "Test 1"), colour = Grou
        ),
        regexp = "Column colour is not in data"
      )
    })

    test_that("it should detect invalid idx value", {
      expect_error(
        dabestr::load(np_dataset,
          x = Group, y = Measurement,
          idx = c("Control 1", "Test1")
        ),
        regexp = "Test1 not present in x"
      )
    })

    describe("Given idx contains subarrays with length < 2", {
      test_that("it should raise an error", {
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
    })

    describe("Given proportional = TRUE", {
      test_that("it should detect invalid dataset", {
        expect_error(
          dabestr::load(np_dataset,
            x = Group, y = Measurement,
            idx = c("Control 1", "Test 1"), proportional = TRUE
          ),
          regexp = "data is not proportional"
        )
      })
    })

    describe("Given is_paired = TRUE", {
      test_that("it should detect when no id_col is given", {
        expect_error(
          dabestr::load(np_dataset,
            x = Group, y = Measurement,
            idx = c("Control 1", "Test 1", "Test 2"), paired = "baseline"
          ),
          regexp = "is TRUE but no id_col was supplied"
        )
      })

      test_that("it should detect invalid paired value", {
        expect_error(
          dabestr::load(np_dataset,
            x = Group, y = Measurement,
            idx = c("Control 1", "Test 1", "Test 2"), paired = "some",
            id_col = ID
          ),
          regexp = "is not 'baseline' or 'sequential'."
        )
      })

      test_that("it should detect invalid dataset size", {
        expect_error(
          dabestr::load(np_dataset[-2, ],
            x = Group, y = Measurement,
            idx = c("Control 1", "Test 1", "Test 2"), paired = "sequential",
            id_col = ID
          ),
          regexp = "data is paired, as indicated by paired but size of control and treatment groups are not equal."
        )
      })
    })

    describe("Given minimeta = TRUE", {
      test_that("it should raise error when both minimeta and proportional is TRUE", {
        expect_error(
          dabestr::load(p_dataset,
            x = Group, y = Success,
            idx = c("Control 1", "Test 1"), id_col = ID,
            proportional = TRUE, minimeta = TRUE
          ),
          regexp = "proportional is TRUE but minimeta is also TRUE."
        )
      })

      test_that("it should raise error when both minimeta and delta2 is TRUE", {
        expect_error(
          dabestr::load(np_dataset,
            x = Group, y = Measurement,
            idx = c("Control 1", "Test 1"),
            delta2 = TRUE, minimeta = TRUE
          ),
          regexp = "delta2 is TRUE but minimeta is also TRUE."
        )
      })

      test_that("it should detect invalid idx value", {
        expect_error(
          dabestr::load(np_dataset,
            x = Group, y = Measurement,
            idx = c("Control 1", "Test 1", "Test 2"),
            minimeta = TRUE
          ),
          regexp = "minimeta is TRUE, but some idx does not consist of exactly 2 groups"
        )
      })
    })

    describe("Given delta2 = TRUE", {
      test_that("it should raise error when both delta2 and proportional is TRUE", {
        expect_error(
          dabestr::load(p_dataset,
            x = Group, y = Success,
            idx = c("Control 1", "Test 1"), id_col = ID,
            proportional = TRUE, delta2 = TRUE
          ),
          regexp = "delta2 is TRUE but proportional is also TRUE."
        )
      })
    })
  })
})

describe("Testing printing.dabest function", {
  # pre-loading all dataset types
  np_dataset <- generate_non_proportional_dataset()
  p_dataset <- generate_proportional_dataset()
  dd_dataset <- generate_deltadelta_dataset()

  describe("Given it is a two group plot", {
    #### 2GROUP ####
    dabest_obj <- dabestr::load(
      data = np_dataset, x = Group, y = Measurement, idx = c("Control 1", "Test 1")
    )

    test_that("it should print the correct values", {
      expect_output(print(dabest_obj), regexp = "95% confidence intervals")
      expect_output(print(dabest_obj), regexp = "Test 1 minus Control 1")
      expect_output(print(dabest_obj), regexp = "5000 resamples")
    })
  })

  describe("Given it is a multigroup plot", {
    describe("Given that the data is paired with baseline", {
      #### MULTIGROUP BASELINE ####
      dabest_obj <- dabestr::load(np_dataset,
        x = Group, y = Measurement,
        idx = list(c("Control 1", "Test 1"), c("Control 2", "Test 2", "Test 3")),
        paired = "baseline", id_col = ID
      )

      test_that("it should print the correct values", {
        expect_output(print(dabest_obj), regexp = "Test 1 minus Control 1")
        expect_output(print(dabest_obj), regexp = "Test 2 minus Control 2")
        expect_output(print(dabest_obj), regexp = "Test 3 minus Control 2")
      })
    })

    describe("Given that the data is paired sequentially", {
      #### MULTIGROUP SEQUENTIAL ####
      dabest_obj <- dabestr::load(np_dataset,
        x = Group, y = Measurement,
        idx = list(c("Control 1", "Test 1"), c("Control 2", "Test 2", "Test 3")),
        paired = "sequential", id_col = ID
      )

      test_that("it should print the correct values", {
        expect_output(print(dabest_obj), regexp = "Test 1 minus Control 1")
        expect_output(print(dabest_obj), regexp = "Test 2 minus Control 2")
        expect_output(print(dabest_obj), regexp = "Test 3 minus Test 2")
      })
    })
  })

  describe("Given it is a proportional plot", {
    #### 2GROUP PROPORTION ####
    dabest_obj <- dabestr::load(
      data = p_dataset, x = Group, y = Success, idx = c("Control 2", "Test 2"),
      proportional = TRUE
    )

    test_that("it should print the correct values", {
      expect_output(print(dabest_obj), regexp = "95% confidence intervals")
      expect_output(print(dabest_obj), regexp = "Test 2 minus Control 2")
      expect_output(print(dabest_obj), regexp = "5000 resamples")
    })
  })

  describe("Given it is a minimeta plot", {
    #### MINIMETA ####
    dabest_obj <- dabestr::load(
      data = np_dataset, x = Group, y = Measurement, idx = list(
        c("Control 1", "Test 1"),
        c("Control 2", "Test 2")
      ),
      minimeta = TRUE
    )

    test_that("it should print the correct values", {
      expect_output(print(dabest_obj), regexp = "95% confidence intervals")
      expect_output(print(dabest_obj), regexp = "Test 1 minus Control 1")
      expect_output(print(dabest_obj), regexp = "Test 2 minus Control 2")
      expect_output(print(dabest_obj), regexp = "weighted delta")
      expect_output(print(dabest_obj), regexp = "5000 resamples")
    })
  })

  describe("Given it is a deltadelta plot", {
    #### DELTADELTA ####
    dabest_obj <- dabestr::load(dd_dataset,
      x = Genotype, y = Measurement, delta2 = TRUE, experiment = Treatment,
      idx = list(c("W Placebo", "M Placebo"), c("W Drug", "M Drug")), colour = Genotype
    )

    test_that("it should print the correct values", {
      expect_output(print(dabest_obj), regexp = "M Placebo minus W Placebo")
      expect_output(print(dabest_obj), regexp = "M Drug minus W Drug")
      expect_output(print(dabest_obj), regexp = "Drug minus Placebo")
    })
  })

  describe("Given ci = 85 in dabest object", {
    #### ADJUSTING CI ####
    dabest_obj <- dabestr::load(
      data = np_dataset, x = Group, y = Measurement, idx = c("Control 1", "Test 1"), ci = 85
    )

    test_that("it should print the correct values", {
      expect_output(print(dabest_obj), regexp = "85% confidence intervals")
      expect_output(print(dabest_obj), regexp = "Test 1 minus Control 1")
      expect_output(print(dabest_obj), regexp = "5000 resamples")
    })
  })

  describe("Given resamples = 3000 in dabest object", {
    #### ADJUSTING RESAMPLES ####
    dabest_obj <- dabestr::load(
      data = np_dataset, x = Group, y = Measurement, idx = c("Control 1", "Test 1"), resamples = 3000
    )
    test_that("it should print the correct values", {
      expect_output(print(dabest_obj), regexp = "95% confidence intervals")
      expect_output(print(dabest_obj), regexp = "Test 1 minus Control 1")
      expect_output(print(dabest_obj), regexp = "3000 resamples")
    })
  })
})
