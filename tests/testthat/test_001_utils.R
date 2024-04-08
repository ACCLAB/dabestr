#### Detecting non-valid dabest objects
testthat::test_that("Able to detect non-valid dabest objects", {
  expect_error(check_dabest_object(32), "dabest_obj must be a <dabest> obj")
})

testthat::test_that("Able to detect non-valid dabest objects", {
  expect_error(check_effectsize_object("any_object"), "dabest_effectsize_obj must be a <dabest_effectsize> obj")
})
