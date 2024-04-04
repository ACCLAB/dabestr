
describe("Testing add_scaling_component_to_delta_plot function", {
  test_that("Returns a list with correct components for valid inputs", {
    # Test case: Valid inputs with float_contrast = TRUE
    delta_plot <- ggplot2::ggplot()  # Dummy plot object
    float_contrast <- FALSE
    boot_result <- list(difference = 0.5)  # Dummy boot result
    delta_x_axis_params <- list(6, c("Label1", "Label2", "Label3"), c(2, 4, 6))  # Dummy x-axis parameters
    delta_y_axis_params <- list(0, 1, 0.5, c(0, 1))  # Dummy y-axis parameters
    summary_data <- list(0.5, 0.6)  # Dummy summary data
    plot_kwargs <- list(show_mini_meta = TRUE, show_delta2 = FALSE)  # Dummy plot kwargs
    result <- add_scaling_component_to_delta_plot(delta_plot, float_contrast, boot_result, delta_x_axis_params, delta_y_axis_params, summary_data, plot_kwargs)
    expect_is(result, "list")
    expect_equal(length(result), 3) # 3 components returned
    # Check delta_plot component
    expect_is(result[[1]], "gg")
  })
  
  test_that("Throws an error for invalid inputs", {
    # Test case: Invalid inputs (e.g., delta_plot not provided)
    expect_error(add_scaling_component_to_delta_plot(), 
                 message = "argument 'delta_plot' is missing, with no default")
  })
})