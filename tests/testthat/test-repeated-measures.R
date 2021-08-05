library(dabestr)
context("Repeated Measures")

############################################################
# Check loading                                            #
############################################################

test_that("Multigroup unpaired loads properly", {
  sample.size <- sample(50:100, 1) # random sample size.
  diff        <- sample(50:100, 1) # random difference in means.
  
  dummy.data  <- generate.time.groups(sampleN = sample.size,
                                      difference = diff)
  
  id <- c("Timepoint1", "Timepoint2", "Timepoint3")
  my.data <- dabest(dummy.data, Group, Value, paired = FALSE,
                    idx = id)
  
  
  # Check attributes are ok.
  my.data.class <- attr(my.data, "class")
  expect_match(my.data.class[1], "dabest")
  expect_match(my.data.class[2], "list")
})

test_that("Multigroup paired loads properly", {
  sample.size <- sample(50:100, 1) # random sample size.
  diff        <- sample(50:100, 1) # random difference in means.
  
  dummy.data  <- generate.time.groups(sampleN = sample.size,
                                      difference = diff)
  
  id <- c("Timepoint1", "Timepoint2", "Timepoint3")
  my.data <- dabest(dummy.data, Group, Value, paired = TRUE,
                    idx = id, id.col = ID)
  
  
  # Check attributes are ok.
  my.data.class <- attr(my.data, "class")
  expect_match(my.data.class[1], "dabest")
  expect_match(my.data.class[2], "list")
})

test_that("Multigroup baseline paired loads properly", {
  sample.size <- sample(50:100, 1) # random sample size.
  diff        <- sample(50:100, 1) # random difference in means.
  
  dummy.data  <- generate.time.groups(sampleN = sample.size,
                                      difference = diff)
  
  id <- c("Timepoint1", "Timepoint2", "Timepoint3")
  my.data <- dabest(dummy.data, Group, Value, paired = "baseline",
                    idx = id, id.col = ID)
  
  
  # Check attributes are ok.
  my.data.class <- attr(my.data, "class")
  expect_match(my.data.class[1], "dabest")
  expect_match(my.data.class[2], "list")
})

test_that("Multigroup sequential paired loads properly", {
  sample.size <- sample(50:100, 1) # random sample size.
  diff        <- sample(50:100, 1) # random difference in means.
  
  dummy.data  <- generate.time.groups(sampleN = sample.size,
                                      difference = diff)
  
  id <- c("Timepoint1", "Timepoint2", "Timepoint3")
  my.data <- dabest(dummy.data, Group, Value, paired = "sequential",
                    idx = id, id.col = ID)
  
  
  # Check attributes are ok.
  my.data.class <- attr(my.data, "class")
  expect_match(my.data.class[1], "dabest")
  expect_match(my.data.class[2], "list")
})

############################################################
# Basic Check                                              #
############################################################

test_that("Time-group unpaired mean diff parses properly", {
  sample.size <- sample(50:100, 1) # random sample size.
  diff        <- sample(50:100, 1) # random difference in means.
  
  dummy.data  <- generate.time.groups(sampleN = sample.size,
                                      difference = diff)
  
  id <- c("Timepoint1", "Timepoint2", "Timepoint3")
  my.data <- dabest(dummy.data, Group, Value, paired = FALSE,
                    idx = id)
  
  
  
  md.my.data <- my.data %>% mean_diff()
  
  report      <- md.my.data$result
  
  
  
  expect_match(as.character(report$control_group), "Timepoint1")
  expect_equal(report$control_size[1], sample.size)
  expect_equal(report$control_size[2], sample.size)
  
  expect_match(as.character(report$test_group[1]), "Timepoint2")
  expect_match(as.character(report$test_group[2]), "Timepoint3")
  expect_equal(report$test_size[1], sample.size)
  expect_equal(report$test_size[2], sample.size)
  
  expect_match(as.character(report$variable), "Value")
  
  expect_false(report$paired[1])
  expect_false(report$paired[2])
})

test_that("Time-group paired mean diff parses properly", {
  sample.size <- sample(50:100, 1) # random sample size.
  diff        <- sample(50:100, 1) # random difference in means.
  
  dummy.data  <- generate.time.groups(sampleN = sample.size,
                                      difference = diff)
  
  id <- c("Timepoint1", "Timepoint2", "Timepoint3")
  my.data <- dabest(dummy.data, Group, Value, paired = TRUE,
                    idx = id, id.col = ID)
  
  
  
  md.my.data <- my.data %>% mean_diff()
  
  report      <- md.my.data$result
  
  
  
  expect_match(as.character(report$control_group), "Timepoint1")
  expect_equal(report$control_size[1], sample.size)
  expect_equal(report$control_size[2], sample.size)
  
  expect_match(as.character(report$test_group[1]), "Timepoint2")
  expect_match(as.character(report$test_group[2]), "Timepoint3")
  expect_equal(report$test_size[1], sample.size)
  expect_equal(report$test_size[2], sample.size)
  
  expect_match(as.character(report$variable), "Value")
  
  expect_true(report$paired[1])
  expect_true(report$paired[2])
})

test_that("Time-group paired baseline mean diff parses properly", {
  sample.size <- sample(50:100, 1) # random sample size.
  diff        <- sample(50:100, 1) # random difference in means.
  
  dummy.data  <- generate.time.groups(sampleN = sample.size,
                                      difference = diff)
  
  id <- c("Timepoint1", "Timepoint2", "Timepoint3")
  my.data <- dabest(dummy.data, Group, Value, paired = "baseline",
                    idx = id, id.col = ID)
  
  
  
  md.my.data <- my.data %>% mean_diff()
  
  report      <- md.my.data$result
  
  
  
  expect_match(as.character(report$control_group), "Timepoint1")
  expect_equal(report$control_size[1], sample.size)
  expect_equal(report$control_size[2], sample.size)
  
  expect_match(as.character(report$test_group[1]), "Timepoint2")
  expect_match(as.character(report$test_group[2]), "Timepoint3")
  expect_equal(report$test_size[1], sample.size)
  expect_equal(report$test_size[2], sample.size)
  
  expect_match(as.character(report$variable), "Value")
  
  expect_true(report$paired[1])
  expect_true(report$paired[2])
})

test_that("Time-group paired sequential mean diff parses properly", {
  sample.size <- sample(50:100, 1) # random sample size.
  diff        <- sample(50:100, 1) # random difference in means.
  
  dummy.data  <- generate.time.groups(sampleN = sample.size,
                                      difference = diff)
  
  id <- c("Timepoint1", "Timepoint2", "Timepoint3")
  my.data <- dabest(dummy.data, Group, Value, paired = "sequential",
                    idx = id, id.col = ID)
  
  
  
  md.my.data <- my.data %>% mean_diff()
  
  report      <- md.my.data$result
  
  
  
  expect_match(as.character(report$control_group[1]), "Timepoint1")
  expect_match(as.character(report$control_group[2]), "Timepoint2")
  expect_equal(report$control_size[1], sample.size)
  expect_equal(report$control_size[2], sample.size)
  
  expect_match(as.character(report$test_group[1]), "Timepoint2")
  expect_match(as.character(report$test_group[2]), "Timepoint3")
  expect_equal(report$test_size[1], sample.size)
  expect_equal(report$test_size[2], sample.size)
  
  expect_match(as.character(report$variable), "Value")
  
  expect_true(report$paired[1])
  expect_true(report$paired[2])
})

############################################################
# Check Graphs                                             #
# 1) Paired = TRUE and Paired = Baseline is the same       #
# 2) Both options on show.pairs should show the same graph #
#    for sequential graphs                                 #
############################################################

test_that("Baseline plot same as paired plot", {
  test.data <- generate.canned.data()
  
  paired.multi <-
    test.data %>%
    dabest(Group, Measurement, 
           idx = list(c("Control1", "Group1", "Group2"), 
                      c("Control2", "Group3", "Group4")),
           paired = TRUE, id.col = ID
    )
  baseline.multi <-
    test.data %>%
    dabest(Group, Measurement, 
           idx = list(c("Control1", "Group1", "Group2"), 
                      c("Control2", "Group3", "Group4")),
           paired = "baseline", id.col = ID
    )
  
  paired.single <-
    test.data %>%
    dabest(Group, Measurement, 
           idx = list(c("Control1", "Group1", "Group2")),
           paired = TRUE, id.col = ID
    )
  baseline.single <-
    test.data %>%
    dabest(Group, Measurement, 
           idx = list(c("Control1", "Group1", "Group2")),
           paired = "baseline", id.col = ID
    )
  
  
  paired.multi.mean.diff <- paired.multi %>% mean_diff()
  paired.single.mean.diff <- paired.single %>% mean_diff()
  baseline.multi.mean.diff <- baseline.multi %>% mean_diff()
  baseline.single.mean.diff <- baseline.single %>% mean_diff()
  
  
  paired.multi.mean.diff.pairs <- paired.multi.mean.diff  %>%
    plot(color.column = Gender, show.pairs = TRUE)
  paired.multi.mean.diff.no.pairs <- paired.multi.mean.diff  %>%
    plot(color.column = Gender, show.pairs = FALSE)
  baseline.multi.mean.diff.pairs <- baseline.multi.mean.diff  %>%
    plot(color.column = Gender, show.pairs = TRUE)
  baseline.multi.mean.diff.no.pairs <- baseline.multi.mean.diff  %>%
    plot(color.column = Gender, show.pairs = FALSE)
  
  paired.single.mean.diff.pairs <- paired.single.mean.diff  %>%
    plot(color.column = Gender, show.pairs = TRUE)
  paired.single.mean.diff.no.pairs <- paired.single.mean.diff  %>%
    plot(color.column = Gender, show.pairs = FALSE)
  baseline.single.mean.diff.pairs <- baseline.single.mean.diff  %>%
    plot(color.column = Gender, show.pairs = TRUE)
  baseline.single.mean.diff.no.pairs <- baseline.single.mean.diff  %>%
    plot(color.column = Gender, show.pairs = FALSE)
  
  
  #checking if baseline is same as pair
  vdiffr::expect_doppelganger("slopegraph baseline multiplot mean diff",
                              paired.multi.mean.diff.pairs)
  vdiffr::expect_doppelganger("swarmgraph baseline multiplot mean diff",
                              paired.multi.mean.diff.no.pairs)
  vdiffr::expect_doppelganger("slopegraph baseline multiplot mean diff",
                              baseline.multi.mean.diff.pairs)
  vdiffr::expect_doppelganger("swarmgraph baseline multiplot mean diff",
                              baseline.multi.mean.diff.no.pairs)
  
  vdiffr::expect_doppelganger("slopegraph baseline timeplot mean diff",
                              paired.single.mean.diff.pairs)
  vdiffr::expect_doppelganger("swarmgraph baseline timeplot mean diff",
                              paired.single.mean.diff.no.pairs)
  vdiffr::expect_doppelganger("slopegraph baseline timeplot mean diff",
                              baseline.single.mean.diff.pairs)
  vdiffr::expect_doppelganger("swarmgraph baseline timeplot mean diff",
                              baseline.single.mean.diff.no.pairs)
  
  
  
})

test_that("Sequential plot is the same for both show pairs option", {
  test.data <- generate.canned.data()
  
  sequential.multi <-
    test.data %>%
    dabest(Group, Measurement, 
           idx = list(c("Control1", "Group1", "Group2"), 
                      c("Control2", "Group3", "Group4")),
           paired = "sequential", id.col = ID
    )
  
  sequential.single <-
    test.data %>%
    dabest(Group, Measurement, 
           idx = list(c("Control1", "Group1", "Group2")),
           paired = "sequential", id.col = ID
    )
  
  sequential.multi.mean.diff <- sequential.multi %>% mean_diff()
  sequential.single.mean.diff <- sequential.single %>% mean_diff()
  
  
  sequential.multi.mean.diff.pairs <- sequential.multi.mean.diff  %>%
    plot(color.column = Gender, show.pairs = TRUE)
  sequential.multi.mean.diff.no.pairs <- sequential.multi.mean.diff  %>%
    plot(color.column = Gender, show.pairs = FALSE)
  
  sequential.single.mean.diff.pairs <- sequential.single.mean.diff  %>%
    plot(color.column = Gender, show.pairs = TRUE)
  sequential.single.mean.diff.no.pairs <- sequential.single.mean.diff  %>%
    plot(color.column = Gender, show.pairs = FALSE)
  
  
  #checking if baseline is same as pair

  vdiffr::expect_doppelganger("slopegraph sequential multiplot mean diff",
                              sequential.multi.mean.diff.pairs)
  vdiffr::expect_doppelganger("slopegraph sequential multiplot mean diff",
                              sequential.multi.mean.diff.no.pairs)
  
  vdiffr::expect_doppelganger("slopegraph sequential timeplot mean diff",
                              sequential.single.mean.diff.pairs)
  vdiffr::expect_doppelganger("slopegraph sequential timeplot mean diff",
                              sequential.single.mean.diff.no.pairs)
  
  
  
})
