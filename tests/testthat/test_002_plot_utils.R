describe("Testing add_swarm_bars_to_raw_plot function", {
    # Test Case 1: Custom Bar Color Provided
    test_that("Adds swarm bars with custom color when bars_color is specified", {
        # Generate dataset
        dataset <- generate_non_proportional_dataset()

        # Create dabest_effectsize_obj
        dabest_effectsize_obj <- load(
            data = dataset, x = Group, y = Measurement,
            idx = list(c("Control 1", "Test 1"), c("Control 2", "Test 2"), c("Control 3", "Test 3")),
            paired = "baseline", id_col = ID
        ) %>%
            mean_diff()

        # Define plot_kwargs with custom bars_color
        plot_kwargs <- list(
            params_swarm_bars = list(color = "purple", alpha = 0.6)
        )

        # Define x_values, y_values, y_min, main_plot_type
        x_values <- c(1, 2, 3)
        y_values <- c(5, 6, 7) # Sample y-values
        y_min <- 4
        main_plot_type <- "swarmplot"

        # Call the function
        geom_rect_layer <- add_swarm_bars_to_raw_plot(
            dabest_effectsize_obj,
            plot_kwargs,
            x_values,
            y_values,
            y_min,
            main_plot_type
        )

        # Extract data from the geom_rect layer
        rect_data <- geom_rect_layer$data

        # Expected rectangles data
        expected_rectangles <- data.frame(
            xmin = x_values - 0.25,
            xmax = x_values + 0.25,
            ymin = rep(y_min, length(x_values)),
            ymax = y_values,
            fill_colour = rep("purple", length(x_values))
        )

        # Check if the rectangles data matches expected
        expect_equal(rect_data, expected_rectangles)
    })
    # Test Case 2: Default to Black Color When Bars_Color is NULL and Conditions are Met
    test_that("Adds swarm bars with default black color when bars_color is NULL and is_paired is TRUE", {
        # Generate dataset
        dataset <- generate_non_proportional_dataset()

        # Create dabest_effectsize_obj with is_paired = TRUE
        dabest_effectsize_obj <- load(
            data = dataset, x = Group, y = Measurement,
            idx = list(
                c("Control 1", "Test 1"),
                c("Control 2", "Test 2")
            )
        ) %>%
            mean_diff()

        # Define plot_kwargs with bars_color as NULL
        plot_kwargs <- list(
            params_swarm_bars = list(color = NULL, alpha = 0.7)
        )

        # Define x_values, y_values, y_min, main_plot_type
        x_values <- c(1, 2)
        y_values <- c(5.5, 6.5)
        y_min <- 4.5
        main_plot_type <- "swarmplot"

        # Call the function
        geom_rect_layer <- add_swarm_bars_to_raw_plot(
            dabest_effectsize_obj,
            plot_kwargs,
            x_values,
            y_values,
            y_min,
            main_plot_type
        )

        # Extract data from the geom_rect layer
        rect_data <- geom_rect_layer$data

        # Expected rectangles data
        expected_rectangles <- data.frame(
            xmin = x_values - 0.25,
            xmax = x_values + 0.25,
            ymin = rep(y_min, length(x_values)),
            ymax = y_values,
            fill_colour = c("1", "2")
        )

        # Check if the rectangles data matches expected
        expect_equal(rect_data, expected_rectangles)
    })

    # Test Case 3: Handle main_plot_type as 'slope' Correctly
    test_that("Adds swarm bars with black color when main_plot_type is 'slope' and bars_color is NULL", {
        # Generate dataset
        dataset <- generate_non_proportional_dataset()

        # Create dabest_effectsize_obj with is_paired = FALSE and is_colour = FALSE
        dabest_effectsize_obj <- load(
            data = dataset, x = Group, y = Measurement,
            idx = list(
                c("Control 1", "Test 1"),
                c("Control 2", "Test 2")
            )
        ) %>%
            mean_diff()

        # Define plot_kwargs with bars_color as NULL
        plot_kwargs <- list(
            params_swarm_bars = list(color = NULL, alpha = 0.5)
        )

        # Define x_values, y_values, y_min, main_plot_type
        x_values <- c(1, 2, 3)
        y_values <- c(5, 6, 7)
        y_min <- 4
        main_plot_type <- "slope"

        # Call the function
        geom_rect_layer <- add_swarm_bars_to_raw_plot(
            dabest_effectsize_obj,
            plot_kwargs,
            x_values,
            y_values,
            y_min,
            main_plot_type
        )

        # Extract data from the geom_rect layer
        rect_data <- geom_rect_layer$data

        # Expected rectangles data
        expected_rectangles <- data.frame(
            xmin = x_values - 0.25,
            xmax = x_values + 0.25,
            ymin = rep(y_min, length(x_values)),
            ymax = y_values,
            fill_colour = rep("black", length(x_values))
        )

        # Check if the rectangles data matches expected
        expect_equal(rect_data, expected_rectangles)
    })

    # Test Case 4: Error Handling for Mismatched x_values and y_values Lengths
    test_that("Throws an error when lengths of x_values and y_values do not match", {
        # Generate dataset
        dataset <- generate_non_proportional_dataset()

        # Create dabest_effectsize_obj
        dabest_effectsize_obj <- load(
            data = dataset, x = Group, y = Measurement,
            idx = list(
                c("Control 1", "Test 1"),
                c("Control 2", "Test 2")
            )
        ) %>%
            mean_diff()

        # Define plot_kwargs
        plot_kwargs <- list(
            params_swarm_bars = list(color = "blue", alpha = 0.5)
        )

        # Define mismatched x_values and y_values
        x_values <- c(1, 2, 3)
        y_values <- c(5, 6) # Length mismatch
        y_min <- 4
        main_plot_type <- "slope"

        # Expect an error due to length mismatch
        expect_error(
            add_swarm_bars_to_raw_plot(
                dabest_effectsize_obj,
                plot_kwargs,
                x_values,
                y_values,
                y_min,
                main_plot_type
            ),
            regexp = "length\\(x_values\\) == length\\(y_values\\)"
        )
    })
})
