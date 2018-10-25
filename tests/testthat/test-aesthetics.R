library(dabestr)
context("aesthetics")



test_that("Gardner-Altman custom aesthetics", {
  #### Generate data. ####
  test.data <- generate.canned.data()

  unpaired <-
    test.data %>%
    dabest(Group, Measurement,
           idx = c("Control1", "Group1"),
           paired = FALSE)

  paired <-
    test.data %>%
    dabest(Group, Measurement,
           idx = c("Control1", "Group1"),
           paired = TRUE, id.col = ID)

  #### Unpaired Gardner-Altman plot. ####
  # Test custom ylims.
  gardner.altman.unpaired.custom.ylim <-
    plot(unpaired, color.col = Gender,
         rawplot.ylim = c(0, 200))

  expect_dpplgngr("Gardner-Altman unpaired custom ylimits",
                  gardner.altman.unpaired.custom.ylim)



  # Test custom ylabels.
  gardner.altman.unpaired.custom.ylabels <-
    plot(unpaired, color.col = Gender,
         rawplot.ylabel = "gardner-altman unpaired rawdata",
         effsize.ylabel = "gardner-altman unpaired effsize")

  expect_dpplgngr("Gardner-Altman unpaired custom ylabels",
                  gardner.altman.unpaired.custom.ylabels)



  # Test palette.
  gardner.altman.unpaired.custom.palette <-
    plot(unpaired, color.col = Gender,
         palette = "Dark2" # The default is "Set2".
  )

  expect_dpplgngr("Gardner-Altman unpaired custom palette",
                  gardner.altman.unpaired.custom.palette)





  #### Paired Gardner-Altman plot. ####
  # Test custom ylims.
  gardner.altman.paired.custom.ylim <-
    plot(paired, color.col = Gender,
         rawplot.ylim = c(25, 200))

  expect_dpplgngr("Gardner-Altman paired custom ylimits",
                  gardner.altman.paired.custom.ylim)

  # Test custom ylabels.
  gardner.altman.paired.custom.ylabels <-
    plot(paired, color.col = Gender,
         rawplot.ylabel = "gardner-altman paired rawdata",
         effsize.ylabel = "gardner-altman paired effsize")

  expect_dpplgngr("Gardner-Altman paired custom ylabels",
                  gardner.altman.paired.custom.ylabels)

  # Test palette.
  gardner.altman.paired.custom.palette <-
    plot(paired, color.col = Gender,
         palette = "Dark2" # The default is "Set2".
    )

  expect_dpplgngr("Gardner-Altman paired custom palette",
                              gardner.altman.paired.custom.palette)



})




test_that("Cumming custom aesthetics", {
  #### Generate data. ####
  test.data <- generate.canned.data()

  multi.group <-
    test.data %>%
    dabest(Group, Measurement,
           idx = list(c("Control1", "Group1", "Group3"),
                      c("Control2", "Group2", "Group4")),
           paired = FALSE
    )


  #### Multi-group Cumming plot. ####
  # Test custom ylims.
  cumming.multi.plot.custom.raw.ylim <-
    plot(multi.group, color.col = Gender,
       rawplot.ylim = c(-100, 200)
    )

  expect_dpplgngr("Cumming custom swarm ylimits",
                  cumming.multi.plot.custom.raw.ylim)


  cumming.multi.plot.custom.effsize.ylim <-
    plot(multi.group, color.col = Gender,
         effsize.ylim = c(-60, 60)
    )

  expect_dpplgngr("Cumming custom effect size ylimits",
                  cumming.multi.plot.custom.effsize.ylim)


  # Test custom ylabels.
  cumming.custom.ylabels <-
    plot(multi.group, color.col = Gender,
         rawplot.ylabel = "cumming plot rawdata",
         effsize.ylabel = "cumming plot effsize")

  expect_dpplgngr("Cumming custom ylabels",
                  cumming.custom.ylabels)


  # Test palette.
  cumming.custom.palette <-
    plot(multi.group, color.col = Gender,
         palette = "Dark2" # The default is "Set2".
    )

  expect_dpplgngr("Cumming custom palette",
                  cumming.custom.palette)


  # Test theme.
 cumming.custom.theme <-
    plot(multi.group, color.col = Gender,
         theme = ggplot2::theme_gray() # The default is `theme_classic()`.
    )

  expect_dpplgngr("Cumming custom theme",
                  cumming.custom.theme)

})



