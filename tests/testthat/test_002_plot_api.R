

# Define dummy data for test
dabest_effectsize_obj <- list(
  idx = list(1:5, 6:10),
  bootstraps = list(replicate(5, rnorm(100)), replicate(5, rnorm(100))),
  proportional = FALSE,
  paired = FALSE,
  delta_x_labels = list("Label1", "Label2"),
  is_colour = FALSE,
  is_paired = FALSE,
  ylim = c(0, 1),
  control_summary = 0,
  test_summary = 0,
  boot_result = list(
    bootstraps = list(replicate(5, rnorm(100))),
    bca_ci_low = 0,
    bca_ci_high = 0,
    difference = 0
  )
)
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

# Test cases
describe("Testing plot_delta function", {
  test_that("Returns a list with correct components for valid inputs", {
    # Test case: Valid inputs
    result <- plot_delta(dabest_effectsize_obj, float_contrast, plot_kwargs)
    expect_type(result, "list")
    expect_named(result, c("delta_plot", "delta_range"))
    # TODO Add specific expectations to check if the components are created correctly
  })
})
