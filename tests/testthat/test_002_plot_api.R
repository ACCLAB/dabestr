# Test cases
describe("Testing plot_delta function", {
  test_that("Returns a list with correct components for valid inputs", {
    # Test case: Valid inputs
    np_dataset <- generate_non_proportional_dataset()
    dabest_effectsize_obj <- dabestr::load(
      data = np_dataset, x = Group, y = Measurement, idx = list(
        c("Control 1", "Test 1"),
        c("Control 2", "Test 2")
      ),
      minimeta = TRUE
    ) %>% mean_diff()
    float_contrast <- TRUE
    plot_kwargs <- list(
      contrast_label = "Contrast Label",
      show_mini_meta = FALSE,
      show_delta2 = FALSE,
      raw_marker_size = 2,
      raw_marker_alpha = 0.5,
      raw_bar_width = 0.2,
      tufte_size = 0.5,
      es_marker_size = 2,
      es_line_size = 0.5,
      flow = TRUE,
      contrast_x_text = 12,
      contrast_y_text = 12,
      show_zero_dot = TRUE,
      show_baseline_ec = FALSE,
      swarm_ylim = NULL
    )

    result <- plot_delta(dabest_effectsize_obj, float_contrast, plot_kwargs)
    expect_type(result, "list")
    expect_named(result, c("delta_plot", "delta_range"))
    # TODO Add specific expectations to check if the components are created correctly
  })
})
