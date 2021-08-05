generate.two.groups <- function(sampleN = 40, populationN = 10000,
                                control_mean = 100, sd = 50,
                                difference = 25, seed = 54321) {
  set.seed(seed)

  pop1 <- rnorm(populationN, mean = control_mean, sd = sd)

  pop2 <- rnorm(populationN, mean = control_mean + difference, sd = sd)

  sample1 <- sample(pop1, sampleN)

  sample2 <- sample(pop2, sampleN)

  id      <- seq(1: sampleN)

  my.data <-
    tibble::tibble(Control = sample1, Test = sample2, ID = id) %>%
    tidyr::gather(key = Group, value = Value, -ID)

  return(my.data)
}



generate.canned.data <- function() {
  set.seed(54321)

  N = 40
  c1 <- rnorm(N, mean = 100, sd = 25)
  c2 <- rnorm(N, mean = 100, sd = 50)
  g1 <- rnorm(N, mean = 120, sd = 25)
  g2 <- rnorm(N, mean = 80, sd = 50)
  g3 <- rnorm(N, mean = 100, sd = 12)
  g4 <- rnorm(N, mean = 100, sd = 50)
  gender <- c(rep('Male', N/2), rep('Female', N/2))
  id <- 1: N

  wide.data <- tibble::tibble(
    Control1 = c1, Control2 = c2,
    Group1 = g1, Group2 = g2, Group3 = g3, Group4 = g4,
    Gender = gender, ID = id)

  my.data <- wide.data %>%
    tidyr::gather(key = Group, value = Measurement, -ID, -Gender)
}

generate.time.groups <- function(sampleN = 40, populationN = 10000,
                                 control_mean = 100, sd = 50,
                                 difference = 25, seed = 54321) {
  set.seed(seed)
  
  # 3 time points
  time1 <- rnorm(populationN, mean = control_mean, sd = sd)
  
  time2 <- rnorm(populationN, mean = control_mean + difference, sd = sd)
  
  time3 <- rnorm(populationN, mean = control_mean + difference + difference, sd = sd)
  
  sample1 <- sample(time1, sampleN)
  
  sample2 <- sample(time2, sampleN)
  
  sample3 <- sample(time3, sampleN)
  
  id      <- seq(1: sampleN)
  
  my.data <-
    tibble::tibble(Timepoint1 = sample1, Timepoint2 = sample2, 
                   Timepoint3 = sample3, ID = id) %>%
    tidyr::gather(key = Group, value = Value, -ID)
  
  return(my.data)
}

generate.dd.data <- function() {
  ax23.t1 <- c(72, 78, 71, 72, 66, 74, 62, 69)
  ax23.t2 <- c(86, 83, 82, 83, 79, 83, 73, 75)
  ax23.t3 <- c(81, 88, 81, 83, 77, 84, 78, 76)
  ax23.t4 <- c(77, 81, 75, 69, 66, 77, 70, 70)
  ax23 <- c(ax23.t1, ax23.t2, ax23.t3, ax23.t4)
  
  bww9.t1 <- c(85, 82, 71, 83, 86, 85, 79, 83)
  bww9.t2 <- c(86, 86, 78, 88, 85, 82, 83, 84)
  bww9.t3 <- c(83, 80, 70, 79, 76, 83, 80, 78)
  bww9.t4 <- c(80, 84, 75, 81, 76, 80, 81, 81)
  bww9 <- c(bww9.t1, bww9.t2, bww9.t3, bww9.t4)
  
  ctrl.t1 <- c(69, 66, 84, 80, 72, 65, 75, 71)
  ctrl.t2 <- c(73, 62, 90, 81, 72, 62, 69, 70)
  ctrl.t3 <- c(72, 67, 88, 77, 69, 65, 69, 65)
  ctrl.t4 <- c(74, 73, 87, 72, 70, 61, 68, 65)
  ctrl <- c(ctrl.t1, ctrl.t2, ctrl.t3, ctrl.t4)
  
  id <- c(1:8, 1:8, 1:8, 1:8)
  treatment <- c(rep("T1", 8), rep("T2", 8),
                 rep("T3", 8), rep("T4", 8))
  
  Value <- c(ctrl, ax23, bww9)
  ids <- c(id, id, id)
  Group <- c(paste(treatment, "Control"),
             paste(treatment, "AX23"),
             paste(treatment, "BWW9"))
  
  data.test3 <- tibble::tibble(ids, Value, Group)
  
  return(data.test3)
}