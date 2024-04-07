
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
    expect_type(result, "list")
    expect_equal(length(result), 3) # 3 components returned
    # Check delta_plot component
    expect_type(result[[1]], "list")
    # TODO Add specific expectations to check if the components are created correctly
  })
  
  test_that("Throws an error for invalid inputs", {
    # Test case: Invalid inputs (e.g., delta_plot not provided)
    expect_error(add_scaling_component_to_delta_plot(), 
                 message = "argument 'delta_plot' is missing, with no default")
  })
})

describe("Testing add_violinplot_component_to_delta_plot function", {
  test_that("Adds violin plot component to delta plot for valid inputs", {
    # Test case: Valid inputs
    delta_plot <- ggplot2::ggplot()  # Dummy plot object
    boots <- list(replicate(5, rnorm(100)), replicate(5, rnorm(100)), replicate(5, rnorm(100)), replicate(5, rnorm(100)), replicate(5, rnorm(100)))
    baseline_ec_boot_result <- list(bootstraps = boots)
    dabest_effectsize_obj <- list(baseline_ec_boot_result = baseline_ec_boot_result)
    main_violin_type <- "multicolour"
    flow <- TRUE
    float_contrast <- TRUE
    zero_dot_x_breaks <- list(1, 2, 3)

    expect_no_error(
      add_violinplot_component_to_delta_plot(delta_plot, dabest_effectsize_obj, main_violin_type, flow, float_contrast, zero_dot_x_breaks)
    )
    # TODO Add specific expectations to check if the components are created correctly
  })
  
  test_that("Throws an error for invalid inputs", {
    # Test case: Invalid inputs (e.g., dabest_effectsize_obj not provided)
    delta_plot <- ggplot2::ggplot()  # Dummy plot object
    expect_error(add_violinplot_component_to_delta_plot(delta_plot), 
                 message = "argument 'dabest_effectsize_obj' is missing, with no default")
  })
})

describe("Testing create_violinplot_components function", {
  test_that("Returns a list with correct components for valid inputs", {
    # Test case: Valid inputs
    boots <- list(replicate(5, rnorm(100)), replicate(5, rnorm(100)), replicate(5, rnorm(100)), replicate(5, rnorm(100)), replicate(5, rnorm(100)))
    idx <- list(1:5)
    float_contrast <- TRUE
    delta_y_max <- 1
    delta_y_min <- 0
    flow <- TRUE
    zero_dot <- TRUE
    result <- create_violinplot_components(boots, idx, float_contrast, delta_y_max, delta_y_min, flow, zero_dot)
    expect_type(result, "list")
    expect_equal(length(result), 5) # 5 components returned
    # TODO Add specific expectations to check if the components are created correctly
  })
})