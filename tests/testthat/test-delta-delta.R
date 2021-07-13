library(dabestr)
context("delta delta")

################################ 
# Testing the Numerical Output #
################################
test_that("Deltadelta is false but deltadelta name is provided", {
  test.data <- generate.canned.data()
  expect_error(dabest(test.data, Group, Measurement, paired = FALSE,
                      idx = list(c("Control1", "Control2"),
                                 c("Group1", "Group2")), 
                      delta2 = FALSE, delta2.name = c("Ctrl", "Test")),
               "'delta2.name' supplied but 'deltadelta' is FALSE.")
  
})

test_that("Deltadelta is true but data does not fit", {
  test.data <- generate.canned.data()
  expect_error(dabest(test.data, Group, Measurement, paired = FALSE,
                      idx = list(c("Control1", "Control2"),
                                 c("Group1", "Group2", "Group3")), 
                      delta2 = TRUE, delta2.name = c("Ctrl", "Test")),
               "'delta2' is currently only available for groups of length 2 by 2.")
  
})


test_that("Deltadelta name is of the wrong size", {
  test.data <- generate.canned.data()
  expect_error(dabest(test.data, Group, Measurement, paired = FALSE,
                      idx = list(c("Control1", "Control2"),
                                 c("Group1", "Group2")), 
                      delta2 = TRUE, delta2.name = c("Ctrl", "Test", "Time")),
               "'delta2.name' is not of length 2.")
  
})

################################
# Testing the Graphical Output #
################################

test_that("Deltadelta graph matches", {
  test.data <- generate.canned.data()
  # dd is TRUE
  test.mean.unpaired.dd <- 
    test.data %>% 
    dabest(Group, Measurement, paired = FALSE,
           idx = list(c("Control1", "Control2"),
                      c("Group1", "Group2")), 
           delta2 = TRUE) %>% mean_diff() %>%
    plot(color.column = Gender)
  test.mean.paired.dd <- 
    test.data %>% 
    dabest(Group, Measurement, paired = TRUE,
           idx = list(c("Control1", "Control2"),
                      c("Group1", "Group2")), id.col = ID,
           delta2 = TRUE) %>% mean_diff() %>%
    plot(color.column = Gender)
  
  # dd is FALSE
  test.mean.unpaired.no.dd <- 
    test.data %>% 
    dabest(Group, Measurement, paired = FALSE,
           idx = list(c("Control1", "Control2"),
                      c("Group1", "Group2")), 
           delta2 = FALSE) %>% mean_diff() %>%
    plot(color.column = Gender)
  test.mean.paired.no.dd <- 
    test.data %>% 
    dabest(Group, Measurement, paired = TRUE,
           idx = list(c("Control1", "Control2"),
                      c("Group1", "Group2")), id.col = ID,
           delta2 = FALSE) %>% mean_diff() %>%
    plot(color.column = Gender)
  
  # dd is TRUE, no legend
  test.mean.unpaired.dd.no.legend <- 
    test.data %>% 
    dabest(Group, Measurement, paired = FALSE,
           idx = list(c("Control1", "Control2"),
                      c("Group1", "Group2")), 
           delta2 = TRUE) %>% mean_diff() %>%
    plot(color.column = Gender, show.legend = FALSE)
  test.mean.paired.dd.no.legend <- 
    test.data %>% 
    dabest(Group, Measurement, paired = TRUE,
           idx = list(c("Control1", "Control2"),
                      c("Group1", "Group2")), id.col = ID,
           delta2 = TRUE) %>% mean_diff() %>%
    plot(color.column = Gender, show.legend = FALSE)
  
  # dd is FALSE, no legend
  test.mean.unpaired.no.dd.no.legend <- 
    test.data %>% 
    dabest(Group, Measurement, paired = FALSE,
           idx = list(c("Control1", "Control2"),
                      c("Group1", "Group2")), 
           delta2 = FALSE) %>% mean_diff() %>%
    plot(color.column = Gender, show.legend = FALSE)
  test.mean.paired.no.dd.no.legend <- 
    test.data %>% 
    dabest(Group, Measurement, paired = TRUE,
           idx = list(c("Control1", "Control2"),
                      c("Group1", "Group2")), id.col = ID,
           delta2 = FALSE) %>% mean_diff() %>%
    plot(color.column = Gender, show.legend = FALSE)
  
  
  vdiffr::expect_doppelganger("swarmgraph ddplot mean diff with legend",
                              test.mean.unpaired.dd)
  vdiffr::expect_doppelganger("slopegraph ddplot mean diff with legend",
                              test.mean.paired.dd)
  vdiffr::expect_doppelganger("swarmgraph no ddplot mean diff with legend",
                              test.mean.unpaired.no.dd)
  vdiffr::expect_doppelganger("slopegraph no ddplot mean diff with legend",
                              test.mean.paired.no.dd)
  
  vdiffr::expect_doppelganger("swarmgraph ddplot mean diff with no legend",
                              test.mean.unpaired.dd.no.legend)
  vdiffr::expect_doppelganger("slopegraph ddplot mean diff with no legend",
                              test.mean.paired.dd.no.legend)
  vdiffr::expect_doppelganger("swarmgraph no ddplot mean diff with no legend",
                              test.mean.unpaired.no.dd.no.legend)
  vdiffr::expect_doppelganger("slopegraph no ddplot mean diff with no legend",
                              test.mean.paired.no.dd.no.legend)
  
})
