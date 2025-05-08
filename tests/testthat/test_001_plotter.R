describe("Testing dabest_plot function", {
  skip_on_cran() # skips all tests below if env var NOT_CRAN=false
  skip_on_ci() # skips all tests below if env var CI=true

  np_dataset <- generate_non_proportional_dataset()
  p_dataset <- generate_proportional_dataset()
  d_dataset <- generate_deltadelta_dataset()

  describe("Given it is a two group plot", {
    #### 2GROUP ####
    unpaired_mean_diff <- dabestr::load(np_dataset, x = Group, y = Measurement, idx = c("Control 1", "Test 1")) %>%
      dabestr::mean_diff()

    #### FLOAT TRUE ####
    describe("Given that float_contrast is TRUE", {
      test_that("it should generate the correct plot", {
        unpaired_mean_diff_float_true <- dabestr::dabest_plot(unpaired_mean_diff, float_contrast = TRUE)
        vdiffr::expect_doppelganger("two-groups unpaired mean diff float true", unpaired_mean_diff_float_true)
      })
    })

    #### FLOAT FALSE ####
    describe("Given that float_contrast is FALSE", {
      test_that("it should generate the correct plot", {
        unpaired_mean_diff_float_false <- dabestr::dabest_plot(unpaired_mean_diff, float_contrast = FALSE)
        vdiffr::expect_doppelganger("two-groups unpaired mean diff float false", unpaired_mean_diff_float_false)
      })
    })

    describe("Given that there is a colour column", {
      #### 2GROUP COLOUR ####
      unpaired_mean_diff_colour <- dabestr::load(np_dataset,
        x = Group, y = Measurement,
        idx = c("Control 1", "Test 1"),
        colour = Gender
      ) %>% dabestr::mean_diff()

      describe("Given that float_contrast is TRUE", {
        test_that("it should generate the correct plot", {
          unpaired_mean_diff_colour_float_true <- dabestr::dabest_plot(unpaired_mean_diff_colour, float_contrast = TRUE)
          vdiffr::expect_doppelganger("two-groups unpaired mean diff colour float true", unpaired_mean_diff_colour_float_true)
        })
      })

      describe("Given that float_contrast is FALSE", {
        test_that("it should generate the correct plot", {
          unpaired_mean_diff_colour_float_false <- dabestr::dabest_plot(unpaired_mean_diff_colour, float_contrast = FALSE)
          vdiffr::expect_doppelganger("two-groups unpaired mean diff colour float false", unpaired_mean_diff_colour_float_false)
        })
      })
    })
  })

  describe("Given it is a multi-group plot", {
    #### MULTIGROUP UNPAIRED ####
    describe("Given that it is unpaired", {
      multi_unpaired <- dabestr::load(np_dataset,
        x = Group, y = Measurement,
        idx = list(c("Control 1", "Test 1"), c("Control 2", "Test 2", "Test 3"))
      ) %>% dabestr::mean_diff()

      test_that("it should generate the correct plot", {
        multi_unpaired_plot <- dabestr::dabest_plot(multi_unpaired, float_contrast = TRUE)
        vdiffr::expect_doppelganger("multigroup unpaired mean diff", multi_unpaired_plot)
      })

      #### MULTIGROUP COLOUR UNPAIRED ####
      describe("Given that it has a colour column", {
        multi_unpaired_colour <- dabestr::load(np_dataset,
          x = Group, y = Measurement,
          idx = list(c("Control 1", "Test 1"), c("Control 2", "Test 2", "Test 3")),
          colour = Gender
        ) %>% dabestr::mean_diff()

        test_that("it should generate the correct plot with colour", {
          multi_unpaired_colour_plot <- dabestr::dabest_plot(multi_unpaired_colour, float_contrast = TRUE)
          vdiffr::expect_doppelganger("multigroup unpaired mean diff colour", multi_unpaired_colour_plot)
        })
      })
    })

    #### MULTIGROUP SEQUENTIAL ####
    describe("Given that the data is paired sequentially", {
      multi_sequential <- dabestr::load(np_dataset,
        x = Group, y = Measurement,
        idx = list(c("Control 1", "Test 1"), c("Control 2", "Test 2", "Test 3")),
        paired = "sequential", id_col = ID
      ) %>% dabestr::mean_diff()

      test_that("it should generate the correct plot", {
        multi_sequential_plot <- dabestr::dabest_plot(multi_sequential, float_contrast = TRUE)
        vdiffr::expect_doppelganger("multigroup sequential mean diff", multi_sequential_plot)
      })
    })

    #### MULTIGROUP BASELINE ####
    describe("Given that the data is paired with baseline", {
      multi_baseline <- dabestr::load(np_dataset,
        x = Group, y = Measurement,
        idx = list(c("Control 1", "Test 1"), c("Control 2", "Test 2", "Test 3")),
        paired = "baseline", id_col = ID
      ) %>% dabestr::mean_diff()

      test_that("it should generate the correct baseline plot", {
        multi_baseline_plot <- dabestr::dabest_plot(multi_baseline, float_contrast = TRUE)
        vdiffr::expect_doppelganger("multigroup baseline mean diff", multi_baseline_plot)
      })

      #### MULTIGROUP BASELINE COLOUR ####
      describe("Given that there is a colour column", {
        multi_baseline_colour <- dabestr::load(np_dataset,
          x = Group, y = Measurement,
          idx = list(c("Control 1", "Test 1"), c("Control 2", "Test 2", "Test 3")),
          paired = "baseline", id_col = ID, colour = Gender
        ) %>% dabestr::mean_diff()
        test_that("it should generate the correct baseline plot with colour", {
          multi_baseline_colour_plot <- dabestr::dabest_plot(multi_baseline_colour, float_contrast = TRUE)
          vdiffr::expect_doppelganger("multigroup baseline colour mean diff", multi_baseline_colour_plot)
        })
      })
    })
  })

  describe("Given a proportional dataset", {
    describe("Given that it is an unpaired proportions plot", {
      #### 2GROUP PROPORTION UNPAIRED ####
      describe("Given that it is two groups", {
        unpaired_mean_diff <- dabestr::load(p_dataset,
          x = Group, y = Success,
          idx = c("Control 1", "Test 1"),
          proportional = TRUE
        ) %>%
          dabestr::mean_diff()

        #### FLOAT TRUE ####
        describe("Given that float_contrast is TRUE", {
          test_that("it should generate the correct plot", {
            unpaired_mean_diff_float_true <- dabestr::dabest_plot(unpaired_mean_diff, float_contrast = TRUE)
            vdiffr::expect_doppelganger("proportion unpaired mean diff float true", unpaired_mean_diff_float_true)
          })
        })

        #### FLOAT FALSE ####
        describe("Given that float_contrast is FALSE", {
          test_that("it should generate the correct plot", {
            unpaired_mean_diff_float_false <- dabestr::dabest_plot(unpaired_mean_diff, float_contrast = FALSE)
            vdiffr::expect_doppelganger("proportion unpaired mean diff float false", unpaired_mean_diff_float_false)
          })
        })
      })

      #### MULTIGROUP PROPORTION UNPAIRED ####
      describe("Given that there are multi-groups", {
        multi_unpaired <- dabestr::load(p_dataset,
          x = Group, y = Success,
          idx = c("Control 1", "Test 1", "Test 2"),
          proportional = TRUE
        ) %>%
          dabestr::mean_diff()

        test_that("it should generate the correct plot", {
          multi_unpaired <- dabestr::dabest_plot(multi_unpaired, float_contrast = FALSE)
          vdiffr::expect_doppelganger("proportion unpaired multigroup mean diff", multi_unpaired)
        })
      })
    })

    describe("Given that it is an paired proportions plot", {
      #### 2GROUP PROPORTION BASELINE ####
      describe("Given that it is 2 group and the data is paired baseline", {
        paired_mean_diff <- dabestr::load(p_dataset,
          x = Group, y = Success,
          idx = c("Control 1", "Test 1"),
          proportional = TRUE, paired = "baseline",
          id_col = ID
        ) %>%
          dabestr::mean_diff()

        #### FLOAT TRUE ####
        describe("Given that float_contrast is TRUE", {
          test_that("it should generate the correct plot", {
            paired_mean_diff_float_true <- dabestr::dabest_plot(paired_mean_diff, float_contrast = TRUE)
            vdiffr::expect_doppelganger("proportion paired mean diff float true", paired_mean_diff_float_true)
          })
        })

        #### FLOAT FALSE ####
        describe("Given that float_contrast is FALSE", {
          test_that("it should generate the correct plot", {
            paired_mean_diff_float_false <- dabestr::dabest_plot(paired_mean_diff, float_contrast = FALSE)
            vdiffr::expect_doppelganger("proportion paired mean diff float false", paired_mean_diff_float_false)
          })
        })
      })

      describe("Given that it is multigroup", {
        #### MULTIGROUP PROPORTION SEQUENTIAL ####
        describe("Given that the data is paired sequentially", {
          multi_sequential <- dabestr::load(p_dataset,
            x = Group, y = Success,
            idx = list(c("Control 1", "Test 1", "Test 2", "Test 3"), c("Control 2", "Test 4")),
            proportional = TRUE, paired = "sequential",
            id_col = ID
          ) %>%
            dabestr::mean_diff()

          test_that("it should generate the correct plot", {
            multi_sequential <- dabestr::dabest_plot(multi_sequential, float_contrast = FALSE)
            vdiffr::expect_doppelganger("proportion sequential mean diff", multi_sequential)
          })
        })

        #### MULTIGROUP PROPORTION BASELINE ####
        describe("Given that the data is paired with baseline", {
          multi_baseline <- dabestr::load(p_dataset,
            x = Group, y = Success,
            idx = list(c("Control 1", "Test 1", "Test 2", "Test 3"), c("Control 2", "Test 4")),
            proportional = TRUE, paired = "baseline",
            id_col = ID
          ) %>%
            dabestr::mean_diff()

          test_that("it should generate the correct plot", {
            multi_baseline_flow_true <- dabestr::dabest_plot(multi_baseline, float_contrast = FALSE)
            vdiffr::expect_doppelganger("proportion baseline mean diff", multi_baseline_flow_true)
          })

          #### FLOW FALSE ####
          describe("Given that flow=FALSE", {
            test_that("it should generate the correct plot", {
              multi_baseline_flow_false <- dabestr::dabest_plot(multi_baseline, float_contrast = FALSE, flow = FALSE)
              vdiffr::expect_doppelganger("proportion baseline flow false mean diff", multi_baseline_flow_false)
            })
          })
        })
      })
    })
  })

  describe("Given it is a minimeta plot", {
    #### MINIMETA ####
    minimeta <- dabestr::load(np_dataset,
      x = Group, y = Measurement,
      idx = list(c("Control 1", "Test 1"), c("Control 2", "Test 2"), c("Control 3", "Test 3")),
      minimeta = TRUE
    ) %>%
      dabestr::mean_diff()

    test_that("it should generate the correct plot", {
      minimeta <- dabestr::dabest_plot(minimeta, float_contrast = TRUE)
      vdiffr::expect_doppelganger("minimeta mean diff", minimeta)
    })
  })

  describe("Given a deltadelta dataset", {
    describe("Given it is a deltadelta plot", {
      #### DELTADELTA ####
      deltadelta <- dabestr::load(d_dataset,
        x = Genotype, y = Measurement, delta2 = TRUE, experiment = Treatment,
        idx = list(c("W Placebo", "M Placebo"), c("W Drug", "M Drug")), colour = Genotype
      ) %>%
        dabestr::mean_diff()

      test_that("it should generate the correct plot", {
        deltadelta <- dabestr::dabest_plot(deltadelta, float_contrast = TRUE)
        vdiffr::expect_doppelganger("deltadelta mean diff", deltadelta)
      })
    })
  })
})
