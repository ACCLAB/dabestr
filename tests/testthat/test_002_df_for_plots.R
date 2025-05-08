describe("Testing create_df_for_tufte function", {
  describe("Given effsize_type is median_diff", {
    test_that("it should return calculations for tufte lines based on median", {
      np_dataset <- generate_non_proportional_dataset()
      result <- create_df_for_tufte(
        raw_data = np_dataset,
        enquo_x = rlang::quo(Group),
        enquo_y = rlang::quo(Measurement),
        proportional = FALSE,
        gap = 0.025,
        effsize_type = "median_diff"
      )
      # hardcoded values because calculation logic is unlikely to change
      expect_equal(result$y_top_start[1], 3.062884, tolerance = 1e-5)
      expect_equal(result$y_top_end[1], 3.244776, tolerance = 1e-5)
      expect_equal(result$y_bot_start[1], 3.012884, tolerance = 1e-5)
      expect_equal(result$y_bot_end[1], 2.855177, tolerance = 1e-5)
    })
  })

  describe("Given effsize_type is NOT median_diff", {
    test_that("it should return calculations for tufte lines based on mean", {
      np_dataset <- generate_non_proportional_dataset()
      result <- create_df_for_tufte(
        raw_data = np_dataset,
        enquo_x = rlang::quo(Group),
        enquo_y = rlang::quo(Measurement),
        proportional = FALSE,
        gap = 0.025,
        effsize_type = "mean_diff"
      )
      # hardcoded values because calculation logic is unlikely to change
      expect_equal(result$y_top_start[1], 3.055607, tolerance = 1e-5)
      expect_equal(result$y_top_end[1], 3.364181, tolerance = 1e-5)
      expect_equal(result$y_bot_start[1], 3.005607, tolerance = 1e-5)
      expect_equal(result$y_bot_end[1], 2.697033, tolerance = 1e-5)
    })
  })
})

describe("Testing create_dfs_for_nonflow_tufte_lines function", {
  describe("Given valid inputs", {
    test_that("it should return repeated rows of values based on idx", {
      idx <- list(c("Control 1", "Test 1"), c("Control 1", "Test 2"))
      tufte_lines_df <- tibble::tibble(
        Group = c("Control 1", "Test 1", "Test 2"),
        value = c(1, 2, 3)
      )

      result <- create_dfs_for_nonflow_tufte_lines(
        idx = idx,
        tufte_lines_df = tufte_lines_df,
        enquo_x = rlang::quo(Group)
      )
      expect_equal(result$value, c(1, 2, 1, 3))
    })
  })
})

describe("Testing create_dfs_for_sankey function", {
  # TODO: create unit test
})

describe("Testing create_dfs_for_xaxis_redraw function", {
  # TODO: improve testing
  describe("Given valid inputs", {
    test_that("it should return a list of data frames with correct dimensions and values for valid inputs", {
      idx <- list(1:5, 6:10, 11:15)
      result <- create_dfs_for_xaxis_redraw(idx)
      expect_type(result, "list")
      expect_equal(length(result), 2) # 2 data frames returned

      # Check df_for_line
      expect_type(result$df_for_line, "list")
      expect_equal(nrow(result$df_for_line), length(idx))
      expect_equal(ncol(result$df_for_line), 2)
      expect_equal(sum(is.na(result$df_for_line)), 0)

      # Check df_for_ticks
      expect_type(result$df_for_ticks, "list")
      expect_equal(nrow(result$df_for_ticks), sum(lengths(idx)))
      expect_equal(ncol(result$df_for_ticks), 1)
      expect_equal(sum(is.na(result$df_for_ticks)), 0)
    })
  })
})

describe("Testing create_dfs_for_proportion_bar function", {
  # TODO: improve testing
  describe("Given valid inputs", {
    test_that("it should return the correct values", {
      proportion_success <- c(0.5)
      result <- create_dfs_for_proportion_bar(proportion_success)
      expect_type(result, "list")
      expect_equal(nrow(result), 4)
      expect_equal(ncol(result), 5)
      expect_equal(sum(is.na(result)), 0)

      # Test case 2: proportion_success contains 0 and 1
      proportion_success <- c(0, 1)
      result <- create_dfs_for_proportion_bar(proportion_success)
      expect_type(result, "list")
      expect_equal(nrow(result), 8)
      expect_equal(ncol(result), 5)
      expect_equal(sum(is.na(result)), 0)
    })
  })

  describe("Given proportion_success has at least a value outside [0, 1]", {
    test_that("it should throw an error when proportion_success contains values outside [0, 1]", {
      proportion_success <- c(0.5, 1.5, 0.75)
      expect_error(
        create_dfs_for_proportion_bar(proportion_success),
        "Proportion plots must be supplied with data of values between 0 and 1."
      )
    })
  })
})

describe("Testing create_dfs_for_baseline_ec_violin function", {
  # TODO: improve testing
  describe("Given valid inputs", {
    test_that("it should return a data frame with correct dimensions and values for valid inputs", {
      boots <- list(replicate(5, rnorm(100)), replicate(5, rnorm(100)), replicate(5, rnorm(100)))
      x_idx_position <- c(1, 2, 3)
      float_contrast <- TRUE
      result <- create_dfs_for_baseline_ec_violin(boots, x_idx_position, float_contrast)
      expect_type(result, "list")
      expect_equal(nrow(result), 1537) # 512 * 3 (number of x_idx_position) +1
      expect_equal(ncol(result), 3)
    })
  })
})
