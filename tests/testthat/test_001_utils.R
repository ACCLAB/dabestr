#### Detecting non-valid dabest objects
testthat::test_that("Able to detect non-valid dabest objects", {
  expect_error(dabestr::check_dabest_object(32), "dabest_obj must be a <dabest> obj")
})