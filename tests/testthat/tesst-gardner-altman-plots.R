library(dabestr)
context("Gardner-Altman plots")



test_that("Gardner-Altman unpaired mean diff", {
  test.data <- generate.canned.data()

  unpaired <-
    test.data %>%
    dabest(Group, Measurement,
           idx = c("Control1", "Group1"), paired = FALSE) %>%
    mean_diff()

  gardner.altman.unpaired <- plot(unpaired, color.column = Gender)

  vdiffr::expect_doppelganger("Gardner-Altman unpaired",
                              gardner.altman.unpaired)
})



test_that("Gardner-Altman unpaired mean diff without legend", {

  gardner.altman.unpaired.no.legend <- plot(unpaired,
                                            color.column = Gender,
                                            show.legend = FALSE)

  vdiffr::expect_doppelganger("Gardner-Altman unpaired",
                              gardner.altman.unpaired.no.legend)
})



# test_that("Gardner-Altman unpaired reverse", {
#   # test.data <- generate.canned.data()
#
#   unpaired_rev <-
#     test.data %>%
#     dabest(Group, Measurement,
#            idx = c("Group1", "Control1"), paired = FALSE) %>%
#     mean_diff()
#
#   gardner.altman.unpaired.rev <- plot(unpaired_rev, color.column = Gender)
#
#   vdiffr::expect_doppelganger("Gardner-Altman unpaired reverse",
#                               gardner.altman.unpaired.rev)
# })



test_that("Gardner-Altman paired mean diff", {
  # test.data <- generate.canned.data()

  paired <-
    test.data %>%
    dabest(Group, Measurement,
           idx = c("Control1", "Group1"),
           paired = TRUE, id.col = ID) %>%
    mean_diff()

  gardner.altman.paired <- plot(paired, color.column = Gender)

  vdiffr::expect_doppelganger("Gardner-Altman paired",
                              gardner.altman.paired)
})




# test_that("Gardner-Altman paired reverse", {
#   # test.data <- generate.canned.data()
#
#   paired_rev <-
#     test.data %>%
#     dabest(Group, Measurement,
#            idx = c("Control1", "Group1"),
#            paired = TRUE, id.col = ID) %>%
#     mean_diff()
#
#   gardner.altman.paired.rev <- plot(paired_rev, color.column = Gender)
#
#   vdiffr::expect_doppelganger("Gardner-Altman paired reverse",
#                               gardner.altman.paired.rev)
# })


test_that("Gardner-Altman unpaired Hedges' g" {

  unpaired_hedges_g <-
    test.data %>%
    dabest(Group, Measurement,
           idx = c("Control1", "Group1")) %>%
    hedges_g()

  gardner.altman.unpaired.hedges_g <- plot(unpaired_hedges_g,
                                           color.column = Gender)

  vdiffr::expect_doppelganger("Gardner-Altman unpaired Hedges' g",
                              gardner.altman.unpaired.hedges_g)
})




