describe("Testing validate_load_params function", {
  # pre-loading all dataset types
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
})

describe("Testing validate_minimeta_params function", {
  # pre-loading all dataset types
  np_dataset <- generate_non_proportional_dataset()
  p_dataset <- generate_proportional_dataset()

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

describe("Testing check_dabest_object function", {
  test_that("it should detect invalid dabest object", {
    expect_error(check_dabest_object(32), "dabest_obj must be a <dabest> obj")
  })
})

describe("Testing check_effectsize_object function", {
  test_that("it should detect invalid dabest_effectsize_obj object", {
    expect_error(check_effectsize_object("any_object"), "dabest_effectsize_obj must be a <dabest_effectsize> obj")
  })
})
