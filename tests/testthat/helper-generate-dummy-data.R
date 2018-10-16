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

