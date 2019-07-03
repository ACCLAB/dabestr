library(dabestr)
context("Cumming plots")



test_that("Cumming two-groups unpaired", {
  test.data <- generate.canned.data()

  unpaired <-
    test.data %>%
    dabest(Group, Measurement,
           idx = c("Control1", "Group1"), paired = FALSE)

  cumming.two.group.unpaired <-
    plot(unpaired, color.column = Gender, float.contrast = FALSE)

  vdiffr::expect_doppelganger("Cumming two-groups unpaired",
                  cumming.two.group.unpaired)
})





test_that("Cumming two-groups paired", {
  test.data <- generate.canned.data()

  paired <-
    test.data %>%
    dabest(Group, Measurement,
           idx = c("Control1", "Group1"), paired = TRUE, id.col = ID)

  cumming.two.group.paired <-
    plot(paired, color.column = Gender, float.contrast = FALSE)

  vdiffr::expect_doppelganger("Cumming two-group paired",
                  cumming.two.group.paired)
})




test_that("Cumming multi two-groups unpaired", {
  test.data <- generate.canned.data()

  multi.two.group.unpaired <-
    test.data %>%
    dabest(Group, Measurement,
           idx = list(c("Control1", "Group1"),
                      c("Control2", "Group2")),
           paired = FALSE)

  cumming.multi.two.group.unpaired <-
    plot(multi.two.group.unpaired, color.column = Gender)

  vdiffr::expect_doppelganger("Cumming multi two-groups unpaired",
                  cumming.multi.two.group.unpaired)
})




test_that("Cumming multi two-groups paired", {
  test.data <- generate.canned.data()

  multi.two.group.paired <-
    test.data %>%
    dabest(Group, Measurement,
           idx = list(c("Control1", "Group1"),
                      c("Control2", "Group2")),
           paired = TRUE, id.col = ID)

  cumming.multi.two.group.paired.slope <-
    plot(multi.two.group.paired, color.column = Gender, slopegraph = TRUE)

  vdiffr::expect_doppelganger("Cumming multi two-groups paired slopegraph",
                  cumming.multi.two.group.paired.slope)


  cumming.multi.two.group.paired.swarm <-
    plot(multi.two.group.paired, color.column = Gender, slopegraph = FALSE)

  vdiffr::expect_doppelganger("Cumming multi two-groups paired swarm",
                  cumming.multi.two.group.paired.swarm)


})





test_that("Cumming shared control", {
  test.data <- generate.canned.data()

  shared.control <-
    test.data %>%
    dabest(Group, Measurement,
           idx = c("Control1", "Group1", "Group2", "Group3"),
           paired = FALSE)

  shared.control.plot <-
    plot(shared.control, color.column = Gender)

  vdiffr::expect_doppelganger("Cumming shared control",
                  shared.control.plot)

})



test_that("Cumming multi-group shared control", {
  test.data <- generate.canned.data()

  multi.group.shared.control <-
    test.data %>%
    dabest(Group, Measurement,
           list(c("Control1", "Group1", "Group3"),
                c("Control2", "Group2", "Group4")),
           paired = FALSE
    )

  multi.group.shared.control.plot <-
    plot(multi.group.shared.control, color.column = Gender)

  vdiffr::expect_doppelganger("Cumming multi-group shared control",
                  multi.group.shared.control.plot)

})
