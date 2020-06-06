library(dabestr)
context("Cumming plots")



test_that("Cumming two-groups unpaired mean diff", {
  test.data <- generate.canned.data()

  unpaired_mean_diff <-
    test.data %>%
    dabest(Group, Measurement,
           idx = c("Control1", "Group1"), paired = FALSE) %>%
    mean_diff()

  cumming.two.group.unpaired <-
    plot(unpaired_mean_diff,
         color.column = Gender,
         float.contrast = FALSE)

  vdiffr::expect_doppelganger("Cumming two-groups unpaired",
                              cumming.two.group.unpaired)
})



test_that("Cumming two-groups unpaired Hedges' g", {
  # test.data <- generate.canned.data()

  unpaired_mean_diff <-
    test.data %>%
    dabest(Group, Measurement,
           idx = c("Control1", "Group1"), paired = FALSE) %>%
    mean_diff()

  cumming.two.group.unpaired <-
    plot(unpaired_mean_diff,
         color.column = Gender,
         float.contrast = FALSE)

  vdiffr::expect_doppelganger("Cumming two-groups unpaired",
                              cumming.two.group.unpaired)
})





test_that("Cumming two-groups paired mean diff", {
  # test.data <- generate.canned.data()

  paired_mean_diff <-
    test.data %>%
    dabest(Group, Measurement,
           idx = c("Control1", "Group1"),
           paired = TRUE, id.col = ID) %>%
    mean_diff()

  cumming.two.group.paired <-
    plot(paired_mean_diff,
         color.column = Gender,
         float.contrast = FALSE)

  vdiffr::expect_doppelganger("Cumming two-group paired",
                  cumming.two.group.paired)
})




test_that("Cumming multi two-groups unpaired mean diff", {
  # test.data <- generate.canned.data()

  multi.two.group.unpaired <-
    test.data %>%
    dabest(Group, Measurement,
           idx = list(c("Control1", "Group1"),
                      c("Control2", "Group2")),
           paired = FALSE) %>%
    mean_diff

  cumming.multi.two.group.unpaired <-
    plot(multi.two.group.unpaired, color.column = Gender)

  vdiffr::expect_doppelganger("Cumming multi two-groups unpaired",
                  cumming.multi.two.group.unpaired)
})




test_that("Cumming multi two-groups paired mean diff", {
  test.data <- generate.canned.data()

  multi.two.group.paired <-
    test.data %>%
    dabest(Group, Measurement,
           idx = list(c("Control1", "Group1"),
                      c("Control2", "Group2")),
           paired = TRUE, id.col = ID) %>%
    mean_diff()

  cumming.multi.two.group.paired.slope <-
    plot(multi.two.group.paired,
         color.column = Gender,
         slopegraph = TRUE)

  vdiffr::expect_doppelganger("Cumming multi two-groups paired mean diff slopegraph",
                  cumming.multi.two.group.paired.slope)


  cumming.multi.two.group.paired.swarm <-
    plot(multi.two.group.paired,
         color.column = Gender,
         slopegraph = FALSE)

  vdiffr::expect_doppelganger("Cumming multi two-groups paired mean diff swarm",
                  cumming.multi.two.group.paired.swarm)


})



test_that("Cumming shared control mean diff", {
  # test.data <- generate.canned.data()

  shared.control <-
    test.data %>%
    dabest(Group, Measurement,
           idx = c("Control1", "Group1", "Group2", "Group3"),
           paired = FALSE) %>%
    mean_diff()

  shared.control.plot <-
    plot(shared.control, color.column = Gender)

  vdiffr::expect_doppelganger("Cumming shared control",
                  shared.control.plot)

})



test_that("Cumming multi-group shared control mean diff", {
  # test.data <- generate.canned.data()

  multi.group.shared.control <-
    test.data %>%
    dabest(Group, Measurement,
           list(c("Control1", "Group1", "Group3"),
                c("Control2", "Group2", "Group4")),
           paired = FALSE) %>%
    mean_diff()

  multi.group.shared.control.plot <-
    plot(multi.group.shared.control, color.column = Gender)

  vdiffr::expect_doppelganger("Cumming multi-group shared control",
                  multi.group.shared.control.plot)

})
