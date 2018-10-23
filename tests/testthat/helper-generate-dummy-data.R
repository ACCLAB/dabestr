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
