library(dabestr)
context("delta delta")
#' @details data obtained from 
#' Abebe, A. (n.d.). 
#' Introduction to Design and Analysis of Experiments with the Sas System. Lecture. 
#' http://halweb.uc3m.es/esp/Personal/personas/jmmarin/esp/Disenno/CursoDisExpSAS.pdf. 

################################ 
# Testing the Numerical Output #
################################
test_that("Deltadelta is false but deltadelta name is provided", {
  test.data <- generate.dd.data()
  expect_error(dabest(test.data, Group, Value, paired = FALSE,
                      idx = list(c("T1 Control", "T2 Control"),
                                 c("T1 BWW9", "T2 BWW9")), 
                      delta2 = FALSE, delta2.name = c("Ctrl", "Test")),
               "'delta2.name' supplied but 'delta2' is FALSE.")
  
})

test_that("Deltadelta is true but data does not fit", {
  test.data <- generate.dd.data()
  expect_error(dabest(test.data, Group, Value, paired = FALSE,
                      idx = list(c("T1 Control", "T2 Control"),
                                 c("T1 BWW9", "T2 BWW9", "T3 BWW9")), 
                      delta2 = TRUE, delta2.name = c("Ctrl", "Test")),
               "'delta2' is currently only available for groups of length 2 by 2.")
  
})


test_that("Deltadelta name is of the wrong size", {
  test.data <- generate.dd.data()
  expect_error(dabest(test.data, Group, Value, paired = FALSE,
                      idx = list(c("T1 Control", "T2 Control"),
                                 c("T1 BWW9", "T2 BWW9")), 
                      delta2 = TRUE, delta2.name = c("Ctrl", "Test", "Time")),
               "'delta2.name' is not of length 2.")
  
})


test_that("Unpaired deltadelta within 95 CI", {
  dummy.data  <- generate.dd.data()
  diff.ctrl <- mean(dummy.data[dummy.data$Group == "T2 Control",]$Value) - 
    mean(dummy.data[dummy.data$Group == "T1 Control",]$Value)
  diff.test <- mean(dummy.data[dummy.data$Group == "T2 BWW9",]$Value) - 
    mean(dummy.data[dummy.data$Group == "T1 BWW9",]$Value)
  diff <- diff.test - diff.ctrl
  
  ci.for.test <- 95
  ci.failures <- 0
  seeds       <- seq(51: 100)
  
  
  my.data <- dabest(dummy.data, Group, Value,
                    idx = list(c("T1 Control", "T2 Control"), 
                               c("T1 BWW9", "T2 BWW9")),
                    paired = FALSE, delta2 = TRUE)
  
  for (s in seeds) {
    boot.result <- my.data %>% mean_diff(seed = s * s)
    
    report      <- boot.result$del.del.store
    
    if (diff < report$bca_ci_low || diff > report$bca_ci_high) {
      ci.failures <- ci.failures + 1
    }
  }
  
  max.errors <- ceiling((1 - (ci.for.test/100)) * length(seeds))
  
  expect_lte(ci.failures, max.errors)
})


test_that("Paired deltadelta within 95 CI", {
  dummy.data  <- generate.dd.data()
  diff.ctrl <- mean(dummy.data[dummy.data$Group == "T2 Control",]$Value) - 
    mean(dummy.data[dummy.data$Group == "T1 Control",]$Value)
  diff.test <- mean(dummy.data[dummy.data$Group == "T2 BWW9",]$Value) - 
    mean(dummy.data[dummy.data$Group == "T1 BWW9",]$Value)
  diff <- diff.test - diff.ctrl
  
  ci.for.test <- 95
  ci.failures <- 0
  seeds       <- seq(51: 100)
  
  
  my.data <- dabest(dummy.data, Group, Value,
                    idx = list(c("T1 Control", "T2 Control"), 
                               c("T1 BWW9", "T2 BWW9")),
                    paired = TRUE, delta2 = TRUE, id.col = ids)
  
  for (s in seeds) {
    boot.result <- my.data %>% mean_diff(seed = s * s)
    
    report      <- boot.result$del.del.store
    
    if (diff < report$bca_ci_low || diff > report$bca_ci_high) {
      ci.failures <- ci.failures + 1
    }
  }
  
  max.errors <- ceiling((1 - (ci.for.test/100)) * length(seeds))
  
  expect_lte(ci.failures, max.errors)
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
