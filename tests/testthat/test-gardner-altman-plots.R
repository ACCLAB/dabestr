library(dabestr)
context("Gardner-Altman plots")



test_that("Gardner-Altman unpaired", {
  test.data <- generate.canned.data()

  unpaired <-
    test.data %>%
    dabest(Group, Measurement,
           idx = c("Control1", "Group1"), paired = FALSE)

  gardner.altman.unpaired <- plot(unpaired, color.col = Gender)

  vdiffr::expect_doppelganger("Gardner-Altman unpaired", gardner.altman.unpaired)
})




test_that("Gardner-Altman paired", {
  test.data <- generate.canned.data()

  paired <-
    test.data %>%
    dabest(Group, Measurement,
           idx = c("Control1", "Group1"), paired = TRUE, id.col = ID)

  gardner.altman.paired <- plot(paired, color.col = Gender)

  vdiffr::expect_doppelganger("Gardner-Altman paired", gardner.altman.paired)
})



