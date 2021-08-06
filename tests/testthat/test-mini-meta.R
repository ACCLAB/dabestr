library(dabestr)
context("mini meta")

################################ 
# Testing the Numerical Output #
################################

test_that("Both delta delta and mini meta provided", {
  test.data <- generate.canned.data()
  expect_error(dabest(test.data, Group, Measurement, paired = FALSE,
                      idx = list(c("Control1", "Group1"),
                                 c("Control2", "Group2")), 
                      mini.meta = TRUE, delta2 = TRUE),
               "'delta2' and 'mini.meta' can not be both TRUE.")
  
})

test_that("Wrong shape", {
  test.data <- generate.canned.data()
  expect_error(dabest(test.data, Group, Measurement, paired = FALSE,
                      idx = list(c("Control1", "Group1"),
                                 c("Control2", "Group2", "Group4")), 
                      mini.meta = TRUE),
               "'mini.meta' is currently only available for groups of size 2.")
  
})

test_that("Unpaired mini meta within 95 CI", {
  dummy.data  <- generate.canned.data()
  g1 <- dummy.data[dummy.data$Group == "Group1",]$Measurement
  c1 <- dummy.data[dummy.data$Group == "Control1",]$Measurement
  g2 <- dummy.data[dummy.data$Group == "Group2",]$Measurement
  c2 <- dummy.data[dummy.data$Group == "Control2",]$Measurement
  diff.1 <- mean(g1) - mean(c1)
  diff.2 <- mean(g2) - mean(c2)
  # calculating the weights
  w1 <- ((1/length(c1)) * (sd(c1)**2)) + 
    ((1/length(g1)) * (sd(g1)**2))
  w1 <- 1/w1
  w2 <- ((1/length(c2)) * (sd(c2)**2)) + 
    ((1/length(g2)) * (sd(g2)**2))
  w2 <- 1/w2
  
  p.diff <- (w1*diff.1) + (w2*diff.2)
  p.diff <- p.diff/(w1+w2)
  
  ci.for.test <- 95
  ci.failures <- 0
  seeds       <- seq(51: 100)
  
  
  my.data <- dabest(dummy.data, Group, Measurement,
                    idx = list(c("Control1", "Group1"),
                               c("Control2", "Group2")),
                    paired = FALSE, mini.meta = TRUE)
  
  for (s in seeds) {
    boot.result <- my.data %>% mean_diff(seed = s * s)
    
    report      <- boot.result$mini.meta.store
    
    if (p.diff < report$bca_ci_low || p.diff > report$bca_ci_high) {
      ci.failures <- ci.failures + 1
    }
  }
  
  max.errors <- ceiling((1 - (ci.for.test/100)) * length(seeds))
  
  expect_lte(ci.failures, max.errors)
})


test_that("Paired mini meta within 95 CI", {
  dummy.data  <- generate.canned.data()
  g1 <- dummy.data[dummy.data$Group == "Group1",]$Measurement
  c1 <- dummy.data[dummy.data$Group == "Control1",]$Measurement
  g2 <- dummy.data[dummy.data$Group == "Group2",]$Measurement
  c2 <- dummy.data[dummy.data$Group == "Control2",]$Measurement
  diff.1 <- mean(g1) - mean(c1)
  diff.2 <- mean(g2) - mean(c2)
  # calculating the weights
  w1 <- ((1/length(c1)) * (sd(c1)**2)) + 
    ((1/length(g1)) * (sd(g1)**2))
  w1 <- 1/w1
  w2 <- ((1/length(c2)) * (sd(c2)**2)) + 
    ((1/length(g2)) * (sd(g2)**2))
  w2 <- 1/w2
  
  p.diff <- (w1*diff.1) + (w2*diff.2)
  p.diff <- p.diff/(w1+w2)
  
  ci.for.test <- 95
  ci.failures <- 0
  seeds       <- seq(51: 100)
  
  
  my.data <- dabest(dummy.data, Group, Measurement,
                    idx = list(c("Control1", "Group1"),
                               c("Control2", "Group2")),
                    paired = TRUE, mini.meta = TRUE, id.col = ID)
  
  for (s in seeds) {
    boot.result <- my.data %>% mean_diff(seed = s * s)
    
    report      <- boot.result$mini.meta.store
    
    if (p.diff < report$bca_ci_low || p.diff > report$bca_ci_high) {
      ci.failures <- ci.failures + 1
    }
  }
  
  max.errors <- ceiling((1 - (ci.for.test/100)) * length(seeds))
  
  expect_lte(ci.failures, max.errors)
})

################################
# Testing the Graphical Output #
################################

test_that("Mini meta graph matches", {
  dummy.data  <- generate.canned.data()
  #Unpaired
  unpaired.data <- dabest(dummy.data, Group, Measurement,
                          idx = list(c("Control1", "Group1"),
                                     c("Control2", "Group2")),
                          paired = FALSE, mini.meta = TRUE) %>% 
    mean_diff()
  #Paired
  paired.data <- dabest(dummy.data, Group, Measurement,
                    idx = list(c("Control1", "Group1"),
                               c("Control2", "Group2")),
                    paired = TRUE, mini.meta = TRUE, id.col = ID) %>% 
    mean_diff()
  

  #Unpaired, no legend
  no.legend.unpaired.mm <- unpaired.data %>% plot()
  #Unpaired, legend
  legend.unpaired.mm <- unpaired.data %>% plot(color.column = Gender)
  #Paired, no legend
  no.legend.paired.mm <- paired.data %>% plot()
  #Paired, legend
  legend.paired.mm <- paired.data %>% plot(color.column = Gender)
  
  vdiffr::expect_doppelganger("swarmgraph mmplot mean diff with no legend",
                              no.legend.unpaired.mm)
  vdiffr::expect_doppelganger("swarmgraph mmplot mean diff with legend",
                              legend.unpaired.mm)
  vdiffr::expect_doppelganger("slopegraph mmplot mean diff with no legend",
                              no.legend.paired.mm)
  vdiffr::expect_doppelganger("slopegraph mmplot mean diff with legend",
                              legend.paired.mm)
})


