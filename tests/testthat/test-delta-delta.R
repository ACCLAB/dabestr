library(dabestr)
context("delta delta")

test_that("Deltadelta is false but deltadelta name is provided", {
  test.data <- generate.canned.data()
  expect_error(dabest(test.data, Group, Measurement, paired = FALSE,
                      idx = list(c("Control1", "Control2"),
                                 c("Group1", "Group2")), 
                      deltadelta = FALSE, deltadelta.name = c("Ctrl", "Test")),
               "'deltadelta.name' supplied but 'deltadelta' is FALSE.")
  
})

test_that("Deltadelta is true but data does not fit", {
  test.data <- generate.canned.data()
  expect_error(dabest(test.data, Group, Measurement, paired = FALSE,
                      idx = list(c("Control1", "Control2"),
                                 c("Group1", "Group2", "Group3")), 
                      deltadelta = TRUE, deltadelta.name = c("Ctrl", "Test")),
               "'deltadelta' is currently only available for groups of length 2 by 2.")
  
})


test_that("Deltadelta name is of the wrong size", {
  test.data <- generate.canned.data()
  expect_error(dabest(test.data, Group, Measurement, paired = FALSE,
                      idx = list(c("Control1", "Control2"),
                                 c("Group1", "Group2")), 
                      deltadelta = TRUE, deltadelta.name = c("Ctrl", "Test", "Time")),
               "'deltadelta.name' is not of length 2.")
  
})

