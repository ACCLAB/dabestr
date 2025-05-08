describe("Testing forest_plot function", {
  skip_on_cran() # skips all tests below if env var NOT_CRAN=false
  skip_on_ci() # skips all tests below if env var CI=true

  np_dataset_1 <- generate_non_proportional_dataset(seed = 1)
  np_dataset_2 <- generate_non_proportional_dataset(seed = 2)
  np_dataset_3 <- generate_non_proportional_dataset(seed = 3)
  d_dataset_1 <- generate_deltadelta_dataset(seed = 1)
  d_dataset_2 <- generate_deltadelta_dataset(seed = 2)
  d_dataset_3 <- generate_deltadelta_dataset(seed = 3)

  describe("Given that data is minimeta contrasts", {
    #### MINIMETA ####
    minimeta_dabest_obj_1 <- dabestr::load(np_dataset_1,
      x = Group, y = Measurement,
      idx = list(c("Control 1", "Test 1"), c("Control 2", "Test 2"), c("Control 3", "Test 3")),
      minimeta = TRUE
    )
    minimeta_dabest_obj_2 <- dabestr::load(np_dataset_2,
      x = Group, y = Measurement,
      idx = list(c("Control 1", "Test 1"), c("Control 2", "Test 2"), c("Control 3", "Test 3")),
      minimeta = TRUE
    )
    minimeta_dabest_obj_3 <- dabestr::load(np_dataset_3,
      x = Group, y = Measurement,
      idx = list(c("Control 1", "Test 1"), c("Control 2", "Test 2"), c("Control 3", "Test 3")),
      minimeta = TRUE
    )

    contrasts_minimeta <- list(minimeta_dabest_obj_1, minimeta_dabest_obj_2, minimeta_dabest_obj_3)

    describe("Given default params", {
      #### MEAN DIFF ####
      it("should generate the correct plot", {
        minimeta_contrast_forest_plot <- dabestr::forest_plot(
          contrasts = contrasts_minimeta,
          contrast_labels = c("Drug 1", "Drug 2", "Drug 3"),
          contrast_type = "minimeta"
        )
        vdiffr::expect_doppelganger("minimeta forest plot mean diff", minimeta_contrast_forest_plot)
      })
    })

    describe("Given median_diff effect size", {
      #### MEDIAN DIFF ####
      it("should generate the correct plot", {
        minimeta_contrast_forest_plot_median_diff <- dabestr::forest_plot(
          contrasts = contrasts_minimeta,
          contrast_labels = c("Drug 1", "Drug 2", "Drug 3"),
          contrast_type = "minimeta",
          effect_size = "median_diff"
        )
        vdiffr::expect_doppelganger("minimeta forest plot median diff", minimeta_contrast_forest_plot_median_diff)
      })
    })

    describe("Given plot kwargs", {
      it("should generate the correct plot", {
        minimeta_contrast_forest_plot_with_plot_kwargs <- dabestr::forest_plot(
          contrasts = contrasts_minimeta,
          contrast_labels = c("Drug X", "Drug Y", "Drug Z"),
          contrast_type = "minimeta",
          custom_palette = c("orange", "gray", "purple"),
          alpha_violin_plot = 0.4,
          fontsize = 14,
          rotation_for_xlabels = 30,
          marker_size = 0.8,
          ci_line_width = 1.0
        )
        vdiffr::expect_doppelganger("minimeta forest plot with plot kwargs", minimeta_contrast_forest_plot_with_plot_kwargs)
      })
    })
  })
  describe("Given that data is deltadelta contrasts", {
    #### DELTADELTA ####
    deltadelta_dabest_obj_1 <- dabestr::load(d_dataset_1,
      x = Genotype, y = Measurement, delta2 = TRUE, experiment = Treatment,
      idx = list(c("W Placebo", "M Placebo"), c("W Drug", "M Drug")), colour = Genotype
    )

    deltadelta_dabest_obj_2 <- dabestr::load(d_dataset_2,
      x = Genotype, y = Measurement, delta2 = TRUE, experiment = Treatment,
      idx = list(c("W Placebo", "M Placebo"), c("W Drug", "M Drug")), colour = Genotype
    )
    deltadelta_dabest_obj_3 <- dabestr::load(d_dataset_1,
      x = Genotype, y = Measurement, delta2 = TRUE, experiment = Treatment,
      idx = list(c("W Placebo", "M Placebo"), c("W Drug", "M Drug")), colour = Genotype
    )

    contrasts_deltadelta <- list(deltadelta_dabest_obj_1, deltadelta_dabest_obj_2, deltadelta_dabest_obj_3)

    describe("Given default params", {
      #### MEAN DIFF ####
      it("should generate the correct plot", {
        deltadelta_contrast_forest_plot <- dabestr::forest_plot(
          contrasts = contrasts_deltadelta,
          contrast_labels = c("Drug 1", "Drug 2", "Drug 3"),
        )
        vdiffr::expect_doppelganger("deltadelta forest plot mean diff", deltadelta_contrast_forest_plot)
      })
    })

    describe("Given plot kwargs", {
      it("should generate the correct plot", {
        deltadelta_contrast_forest_plot_with_plot_kwargs <- dabestr::forest_plot(
          contrasts = contrasts_deltadelta,
          contrast_labels = c("Drug X", "Drug Y", "Drug Z"),
          custom_palette = c("orange", "gray", "purple"),
          alpha_violin_plot = 0.4,
          fontsize = 14,
          rotation_for_xlabels = 30,
          marker_size = 0.8,
          ci_line_width = 1.0
        )
        vdiffr::expect_doppelganger("deltadelta forest plot with plot kwargs", deltadelta_contrast_forest_plot_with_plot_kwargs)
      })
    })
  })
})


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
