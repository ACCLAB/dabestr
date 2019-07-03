library(dabestr)
context("Gardner-Altman plots")



test_that("Gardner-Altman unpaired", {
  test.data <- generate.canned.data()

  unpaired <-
    test.data %>%
    dabest(Group, Measurement,
           idx = c("Control1", "Group1"), paired = FALSE)

  gardner.altman.unpaired <- plot(unpaired, color.column = Gender)

  vdiffr::expect_doppelganger("Gardner-Altman unpaired",
                              gardner.altman.unpaired)
})



test_that("Gardner-Altman unpaired reverse", {
  test.data <- generate.canned.data()

  unpaired <-
    test.data %>%
    dabest(Group, Measurement,
           idx = c("Group1", "Control1"), paired = FALSE)

  gardner.altman.unpaired.rev <- plot(unpaired, color.column = Gender)

  vdiffr::expect_doppelganger("Gardner-Altman unpaired reverse",
                              gardner.altman.unpaired.rev)
})



test_that("Gardner-Altman paired", {
  test.data <- generate.canned.data()

  paired <-
    test.data %>%
    dabest(Group, Measurement,
           idx = c("Control1", "Group1"), paired = TRUE, id.col = ID)

  gardner.altman.paired <- plot(paired, color.column = Gender)

  vdiffr::expect_doppelganger("Gardner-Altman paired",
                              gardner.altman.paired)
})




test_that("Gardner-Altman paired reverse", {
  test.data <- generate.canned.data()

  paired <-
    test.data %>%
    dabest(Group, Measurement,
           idx = c("Group1", "Control1"), paired = TRUE, id.col = ID)

  gardner.altman.paired.rev <- plot(paired, color.column = Gender)

  vdiffr::expect_doppelganger("Gardner-Altman paired reverse",
                              gardner.altman.paired.rev)
})



