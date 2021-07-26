library(dabestr)
context("Gardner-Altman plots")



test_that("Gardner-Altman unpaired", {
  test.data <- generate.canned.data()

  unpaired <-
    test.data %>%
    dabest(Group, Measurement,
           idx = c("Control1", "Group1"), paired = FALSE)



  unpaired.mean.diff <- unpaired %>% mean_diff()

  unpaired.mean.diff.legend <- unpaired.mean.diff %>%
                               plot(color.column = Gender)

  vdiffr::expect_doppelganger("Gardner-Altman unpaired mean diff",
                              unpaired.mean.diff.legend)




  unpaired.mean.diff.no.legend <- unpaired.mean.diff %>%
                              plot(color.column = Gender,
                                   show.legend = FALSE)

  vdiffr::expect_doppelganger("Gardner-Altman unpaired mean diff no legend",
                              unpaired.mean.diff.no.legend)




  unpaired.hedges.g <- unpaired %>% hedges_g() %>%
                        plot(color.column = Gender,
                             palette = c("darkorange", "black"))

  vdiffr::expect_doppelganger("Gardner-Altman unpaired Hedges' g",
                              unpaired.hedges.g)
})



test_that("Gardner-Altman paired", {
  test.data <- generate.canned.data()

  paired <-
    test.data %>%
    dabest(Group, Measurement,
           idx = c("Control1", "Group1"),
           paired = TRUE, id.col = ID)



  gardner.altman.paired.mean.diff <- paired %>% mean_diff() %>%
                                     plot(color.column = Gender)

  vdiffr::expect_doppelganger("Gardner-Altman paired mean diff",
                              gardner.altman.paired.mean.diff)



  gardner.altman.paired.hedges.g <- paired %>% hedges_g() %>%
                                    plot(color.column = Gender,
                                         palette = c("darkorange", "black"))

  vdiffr::expect_doppelganger("Gardner-Altman paired Hedges' g",
                              gardner.altman.paired.hedges.g)

})






