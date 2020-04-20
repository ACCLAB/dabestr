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
    plot(unpaired, color.column = Gender,
         rawplot.ylim = c(0, 200))

  vdiffr::expect_doppelganger("Gardner-Altman unpaired custom ylimits",
                  gardner.altman.unpaired.custom.ylim)


  # Test custom ylabels.
  gardner.altman.unpaired.custom.ylabels <-
    plot(unpaired, color.column = Gender,
         rawplot.ylabel = "gardner-altman unpaired rawdata",
         effsize.ylabel = "gardner-altman unpaired effsize")

  vdiffr::expect_doppelganger("Gardner-Altman unpaired custom ylabels",
                  gardner.altman.unpaired.custom.ylabels)


  # Test palette, RColorBrwer.
  gardner.altman.unpaired.RColorBrewer.palette <-
    plot(unpaired, color.column = Gender,
         palette = "Dark2" # The default is "Set2".
  )

  vdiffr::expect_doppelganger("Gardner-Altman unpaired RColorBrewer palette",
                              gardner.altman.unpaired.RColorBrewer.palette)

  # Test palette, manual
  gardner.altman.unpaired.manual.palette <-
    plot(unpaired, color.column = Gender,
         palette = c("moccasin", "honeydew2")
    )

  vdiffr::expect_doppelganger("Gardner-Altman unpaired manaul palette",
                              gardner.altman.unpaired.manual.palette)


  # Test rawplot marker size.
  gardner.altman.unpaired.custom.rawplot.markersize <-
    plot(unpaired, color.column = Gender,
         rawplot.markersize = 1
    )

  vdiffr::expect_doppelganger("Gardner-Altman unpaired custom rawplot marker size",
                  gardner.altman.unpaired.custom.rawplot.markersize)

  # Test custom effect size marker.
  gardner.altman.unpaired.custom.effsize <-
    plot(unpaired, color.column = Gender,
         effsize.markersize = 6
    )

  vdiffr::expect_doppelganger("Gardner-Altman unpaired custom effect size marker",
                  gardner.altman.unpaired.custom.effsize)


  # Test custom group width.
  gardner.altman.unpaired.custom.group.width <-
    plot(unpaired, color.column = Gender,
         rawplot.groupwidth = 0.2
    )

  vdiffr::expect_doppelganger("Gardner-Altman unpaired custom group width",
                              gardner.altman.unpaired.custom.group.width)






  #### Paired Gardner-Altman plot. ####
  # Test custom ylims.
  gardner.altman.paired.custom.ylim <-
    plot(paired, color.column = Gender,
         rawplot.ylim = c(25, 200))

  vdiffr::expect_doppelganger("Gardner-Altman paired custom ylimits",
                  gardner.altman.paired.custom.ylim)


  # Test custom ylabels.
  gardner.altman.paired.custom.ylabels <-
    plot(paired, color.column = Gender,
         rawplot.ylabel = "gardner-altman paired rawdata",
         effsize.ylabel = "gardner-altman paired effsize")

  vdiffr::expect_doppelganger("Gardner-Altman paired custom ylabels",
                  gardner.altman.paired.custom.ylabels)


  # Test palette, RcolorBrewer.
  gardner.altman.paired.RcolorBrewer.palette <-
    plot(paired, color.column = Gender,
         palette = "Set1" # The default is "Set2".
    )

  vdiffr::expect_doppelganger("Gardner-Altman paired RcolorBrewer palette",
                              gardner.altman.paired.RcolorBrewer.palette)


  # Test palette, manual
  gardner.altman.paired.manual.palette <-
    plot(paired, color.column = Gender,
         palette = c("gold", "black")
    )

  vdiffr::expect_doppelganger("Gardner-Altman paired manual palette",
                              gardner.altman.paired.manual.palette)


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
    plot(multi.group, color.column = Gender,
       rawplot.ylim = c(-100, 200)
    )

  vdiffr::expect_doppelganger("Cumming custom swarm ylimits",
                  cumming.multi.plot.custom.raw.ylim)

  cumming.multi.plot.custom.effsize.ylim <-
    plot(multi.group, color.column = Gender,
         effsize.ylim = c(-60, 60)
    )

  vdiffr::expect_doppelganger("Cumming custom effect size ylimits",
                              cumming.multi.plot.custom.effsize.ylim)


  # Test rawplot marker size.
  cumming.multi.plot.custom.rawplot.markersize <-
    plot(multi.group, color.column = Gender,
         rawplot.markersize = 1
    )

  vdiffr::expect_doppelganger("Cumming custom rawplot marker size",
                  cumming.multi.plot.custom.rawplot.markersize)


  # Test custom effect size marker.
  cumming.multi.plot.custom.effsize.marker <-
    plot(multi.group, color.column = Gender,
         effsize.markersize = 6
    )

  vdiffr::expect_doppelganger("Cumming custom effect size marker",
                              cumming.multi.plot.custom.effsize.ylim)


  # Test groupwidth.
  cumming.multi.plot.custom.groupwidth <-
    plot(multi.group, color.column = Gender,
         rawplot.groupwidth = 0.4
    )

  vdiffr::expect_doppelganger("Cumming custom groupwidth",
                  cumming.multi.plot.custom.groupwidth)




  # Test custom ylabels.
  cumming.custom.ylabels <-
    plot(multi.group, color.column = Gender,
         rawplot.ylabel = "cumming plot rawdata",
         effsize.ylabel = "cumming plot effsize")

  vdiffr::expect_doppelganger("Cumming custom ylabels",
                  cumming.custom.ylabels)


  # Test RColorBrewer palette Cumming.
  cumming.RColorBrewer.palette <-
    plot(multi.group, color.column = Gender,
         palette = "Dark2" # The default is "Set2".
    )

  vdiffr::expect_doppelganger("Cumming RColorBrewer palette",
                              cumming.RColorBrewer.palette)


  # Test manually supplied palette.
  cumming.manual.palette <-
    plot(multi.group, color.column = Gender,
       # A custom palette consisting of a vector of colors,
       # specified as RGB hexcode, or as a R named color.
       # See all 657 named R colors with `colors()`.
       palette = c("#FFA500", "sienna4")
  )

  vdiffr::expect_doppelganger("Cumming manual palette",
                               cumming.manual.palette)


  # Test theme.
  cumming.custom.theme <-
    plot(multi.group, color.column = Gender,
         theme = ggplot2::theme_gray() # The default is `theme_classic()`.
    )

  vdiffr::expect_doppelganger("Cumming custom theme",
                  cumming.custom.theme)

})



