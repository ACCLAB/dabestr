testthat::test_that("Plot two groups correctly", {
  np_dataset <- generate_non_proportional_dataset()
  
  #### 2GROUP ####
  unpaired_mean_diff <- dabestr::load(np_dataset, x = Group, y = Measurement, idx = c("Control 1", "Test 1")) %>%
    dabestr::mean_diff()
  
  #### FLOAT TRUE ####
  unpaired_mean_diff_float_true <- dabestr::dabest_plot(unpaired_mean_diff, float_contrast = TRUE)
  vdiffr::expect_doppelganger("two-groups unpaired mean diff float true", unpaired_mean_diff_float_true)
  
  #### FLOAT FALSE ####
  unpaired_mean_diff_float_false <- dabestr::dabest_plot(unpaired_mean_diff, float_contrast = FALSE)
  vdiffr::expect_doppelganger("two-groups unpaired mean diff float false", unpaired_mean_diff_float_false)
})

testthat::test_that("Plot two groups colour correctly", {
  np_dataset <- generate_non_proportional_dataset()
  
  #### 2GROUP COLOUR ####
  unpaired_mean_diff_colour <- dabestr::load(np_dataset, x = Group, y = Measurement, 
                                             idx = c("Control 1", "Test 1"),
                                             colour = Gender) %>%
    dabestr::mean_diff()
  
  #### FLOAT TRUE ####
  unpaired_mean_diff_colour_float_true <- dabestr::dabest_plot(unpaired_mean_diff_colour, float_contrast = TRUE)
  vdiffr::expect_doppelganger("two-groups unpaired mean diff colour float true", unpaired_mean_diff_colour_float_true)
  
  #### FLOAT FALSE ####
  unpaired_mean_diff_colour_float_false <- dabestr::dabest_plot(unpaired_mean_diff_colour, float_contrast = FALSE)
  vdiffr::expect_doppelganger("two-groups unpaired mean diff colour float false", unpaired_mean_diff_colour_float_false)
})

testthat::test_that("Plot multi groups correctly", {
  np_dataset <- generate_non_proportional_dataset()

  #### MULTIGROUP UNPAIRED ####
  multi_unpaired <- dabestr::load(np_dataset, x = Group, y = Measurement, 
                                  idx = list(c("Control 1", "Test 1"), c("Control 2", "Test 2", "Test 3"))) %>%
    dabestr::mean_diff()
  
  multi_unpaired <- dabestr::dabest_plot(multi_unpaired, float_contrast = TRUE)
  vdiffr::expect_doppelganger("multigroup unpaired mean diff", multi_unpaired)
  
  #### MULTIGROUP COLOUR UNPAIRED ####
  multi_unpaired_colour <- dabestr::load(np_dataset, x = Group, y = Measurement, 
                                         idx = list(c("Control 1", "Test 1"), c("Control 2", "Test 2", "Test 3")),
                                         colour = Gender) %>%
    dabestr::mean_diff()
  
  multi_unpaired_colour <- dabestr::dabest_plot(multi_unpaired_colour, float_contrast = TRUE)
  vdiffr::expect_doppelganger("multigroup unpaired mean diff colour", multi_unpaired_colour)
  
  #### MULTIGROUP SEQUENTIAL ####
  multi_sequential <- dabestr::load(np_dataset, x = Group, y = Measurement, 
                                    idx = list(c("Control 1", "Test 1"), c("Control 2", "Test 2", "Test 3")),
                                    paired = "sequential", id_col = ID) %>%
    dabestr::mean_diff()
  
  multi_sequential <- dabestr::dabest_plot(multi_sequential, float_contrast = TRUE)
  vdiffr::expect_doppelganger("multigroup sequential mean diff", multi_sequential)
  
  #### MULTIGROUP BASELINE ####
  multi_baseline <- dabestr::load(np_dataset, x = Group, y = Measurement, 
                                  idx = list(c("Control 1", "Test 1"), c("Control 2", "Test 2", "Test 3")),
                                  paired = "baseline", id_col = ID) %>%
    dabestr::mean_diff()
  
  multi_baseline <- dabestr::dabest_plot(multi_baseline, float_contrast = TRUE)
  vdiffr::expect_doppelganger("multigroup baseline mean diff", multi_baseline)
  
  #### MULTIGROUP BASELINE COLOUR ####
  multi_baseline_colour <- dabestr::load(np_dataset, x = Group, y = Measurement, 
                                    idx = list(c("Control 1", "Test 1"), c("Control 2", "Test 2", "Test 3")),
                                    paired = "baseline", id_col = ID, colour = Gender) %>%
    dabestr::mean_diff()
  
  multi_baseline_colour <- dabestr::dabest_plot(multi_baseline_colour, float_contrast = TRUE)
  vdiffr::expect_doppelganger("multigroup baseline colour mean diff", multi_baseline_colour)
})

testthat::test_that("Plot unpaired proportions correctly", {
  p_dataset <- generate_proportional_dataset()
  
  #### 2GROUP PROPORTION UNPAIRED ####
  unpaired_mean_diff <- dabestr::load(p_dataset, x = Group, y = Success, 
                                  idx = c("Control 1", "Test 1"),
                                  proportional = TRUE) %>%
    dabestr::mean_diff()
  
  #### FLOAT TRUE ####
  unpaired_mean_diff_float_true <- dabestr::dabest_plot(unpaired_mean_diff, float_contrast = TRUE)
  vdiffr::expect_doppelganger("proportion unpaired mean diff float true", unpaired_mean_diff_float_true)
  
  #### FLOAT FALSE ####
  unpaired_mean_diff_float_false <- dabestr::dabest_plot(unpaired_mean_diff, float_contrast = FALSE)
  vdiffr::expect_doppelganger("proportion unpaired mean diff float false", unpaired_mean_diff_float_false)
  
  #### MULTIGROUP PROPORTION UNPAIRED ####
  multi_unpaired <- dabestr::load(p_dataset, x = Group, y = Success, 
                                  idx = c("Control 1", "Test 1", "Test 2"),
                                  proportional = TRUE) %>%
    dabestr::mean_diff()
  
  multi_unpaired <- dabestr::dabest_plot(multi_unpaired, float_contrast = FALSE)
  vdiffr::expect_doppelganger("proportion unpaired multigroup mean diff", multi_unpaired)
})

testthat::test_that("Plot paired proportions correctly", {
  p_dataset <- generate_proportional_dataset()
  
  #### 2GROUP PROPORTION BASELINE ####
  paired_mean_diff <- dabestr::load(p_dataset, x = Group, y = Success, 
                                    idx = c("Control 1", "Test 1"),
                                    proportional = TRUE, paired = "baseline",
                                    id_col = ID) %>%
    dabestr::mean_diff()
  
  #### FLOAT TRUE ####
  paired_mean_diff_float_true <- dabestr::dabest_plot(paired_mean_diff, float_contrast = TRUE)
  vdiffr::expect_doppelganger("proportion paired mean diff float true", paired_mean_diff_float_true)
  
  #### FLOAT FALSE ####
  paired_mean_diff_float_false <- dabestr::dabest_plot(paired_mean_diff, float_contrast = FALSE)
  vdiffr::expect_doppelganger("proportion paired mean diff float false", paired_mean_diff_float_false)
  
  #### MULTIGROUP PROPORTION SEQUENTIAL ####
  multi_sequential <- dabestr::load(p_dataset, x = Group, y = Success, 
                                    idx = list(c("Control 1", "Test 1", "Test 2", "Test 3"), c("Control 2", "Test 4")),
                                    proportional = TRUE, paired = "sequential",
                                    id_col = ID) %>%
    dabestr::mean_diff()
  
  multi_sequential <- dabestr::dabest_plot(multi_sequential, float_contrast = FALSE)
  vdiffr::expect_doppelganger("proportion sequential mean diff", multi_sequential)
  
  #### MULTIGROUP PROPORTION BASELINE ####
  multi_baseline <- dabestr::load(p_dataset, x = Group, y = Success, 
                                    idx = list(c("Control 1", "Test 1", "Test 2", "Test 3"), c("Control 2", "Test 4")),
                                    proportional = TRUE, paired = "baseline",
                                    id_col = ID) %>%
    dabestr::mean_diff()
  
  multi_baseline_flow_true <- dabestr::dabest_plot(multi_baseline, float_contrast = FALSE)
  vdiffr::expect_doppelganger("proportion baseline mean diff", multi_baseline_flow_true)
  
  #### FLOW FALSE ####
  multi_baseline_flow_false <- dabestr::dabest_plot(multi_baseline, float_contrast = FALSE, flow = FALSE)
  vdiffr::expect_doppelganger("proportion baseline flow false mean diff", multi_baseline_flow_false)
})

testthat::test_that("Plot minimeta correctly", {
  np_dataset <- generate_non_proportional_dataset()

  #### MINIMETA ####
  minimeta <- dabestr::load(np_dataset, x = Group, y = Measurement, 
                            idx = list(c("Control 1", "Test 1"), c("Control 2", "Test 2"), c("Control 3", "Test 3")),
                            minimeta = TRUE) %>%
    dabestr::mean_diff()
  
  minimeta <- dabestr::dabest_plot(minimeta, float_contrast = TRUE)
  vdiffr::expect_doppelganger("minimeta mean diff", minimeta)
})

testthat::test_that("Plot deltadelta correctly", {
  d_dataset <- generate_deltadelta_dataset()
  
  #### DELTADELTA ####
  deltadelta <- dabestr::load(d_dataset, x = Genotype, y = Measurement, delta2 = TRUE, experiment = Treatment, 
                              idx = list(c("W Placebo","M Placebo"),c("W Drug","M Drug")), colour = Genotype) %>%
    dabestr::mean_diff()
  
  deltadelta <- dabestr::dabest_plot(deltadelta, float_contrast = TRUE)
  vdiffr::expect_doppelganger("deltadelta mean diff", deltadelta)
})


