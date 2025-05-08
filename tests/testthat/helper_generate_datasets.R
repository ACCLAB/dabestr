get_gender_column <- function(N) {
  return(c(rep("Male", N / 2), rep("Female", N / 2)))
}

get_genotype_column <- function(N) {
  return(c(rep("M", N / 2), rep("W", N / 2)))
}

# TODO Add documentation
generate_non_proportional_dataset <- function(N = 40, seed = 12345) {
  set.seed(seed) # Fix the seed so the results are replicable.
  # pop_size = 10000 # Size of each population.
  N <- 20 # The number of samples taken from each population

  # Create samples
  c1 <- rnorm(N, mean = 3, sd = 0.4)
  c2 <- rnorm(N, mean = 3.5, sd = 0.75)
  c3 <- rnorm(N, mean = 3.25, sd = 0.4)

  t1 <- rnorm(N, mean = 3.5, sd = 0.5)
  t2 <- rnorm(N, mean = 2.5, sd = 0.6)
  t3 <- rnorm(N, mean = 3, sd = 0.75)
  t4 <- rnorm(N, mean = 3.5, sd = 0.75)
  t5 <- rnorm(N, mean = 3.25, sd = 0.4)
  t6 <- rnorm(N, mean = 3.25, sd = 0.4)

  # Add a `gender` column for coloring the data.
  gender <- get_gender_column(N)

  # Add an `id` column for paired data plotting.
  id <- 1:N

  # Combine samples and gender into a DataFrame.
  df <- tibble::tibble(
    `Control 1` = c1, `Control 2` = c2, `Control 3` = c3,
    `Test 1` = t1, `Test 2` = t2, `Test 3` = t3, `Test 4` = t4, `Test 5` = t5, `Test 6` = t6,
    Gender = gender, ID = id
  )

  df <- df %>%
    tidyr::gather(key = Group, value = Measurement, -ID, -Gender)

  return(df)
}

# TODO Add documentation
generate_proportional_dataset <- function(N = 40, seed = 12345) {
  set.seed(seed) # Fix the seed so the results are replicable.
  N <- 40 # The number of samples taken from each population

  # Create samples
  size <- 1
  c1 <- rbinom(N, size, prob = 0.2)
  c2 <- rbinom(N, size, prob = 0.2)
  c3 <- rbinom(N, size, prob = 0.8)

  t1 <- rbinom(N, size, prob = 0.35)
  t2 <- rbinom(N, size, prob = 0.2)
  t3 <- rbinom(N, size, prob = 0.3)
  t4 <- rbinom(N, size, prob = 0.4)
  t5 <- rbinom(N, size, prob = 0.5)
  t6 <- rbinom(N, size, prob = 0.6)
  t7 <- c(rep(1, N))
  t8 <- c(rep(0, N))

  # Add a `gender` column for coloring the data.
  gender <- get_gender_column(N)

  # Add an `id` column for paired data plotting.
  id <- 1:N

  # Combine samples and gender into a DataFrame.
  df <- tibble::tibble(
    `Control 1` = c1, `Control 2` = c2, `Control 3` = c3,
    `Test 1` = t1, `Test 2` = t2, `Test 3` = t3, `Test 4` = t4, `Test 5` = t5,
    `Test 6` = t6, `Test 7` = t7, `Test 8` = t8,
    Gender = gender, ID = id
  )

  df <- df %>%
    tidyr::gather(key = Group, value = Success, -ID, -Gender)

  return(df)
}

# TODO Add documentation
generate_deltadelta_dataset <- function(N = 40, seed = 12345) {
  set.seed(seed) # Fix the seed so the results are replicable.
  # pop_size = 10000 # Size of each population.
  N <- 20 # The number of samples taken from each population

  # Create samples
  placebo <- rnorm(N, mean = 3, sd = 0.4)
  drug <- rnorm(N, mean = 3.5, sd = 0.75)

  # Add a `Genotype` column as the second variable
  genotype <- get_genotype_column(N)

  # Add an `id` column for paired data plotting.
  id <- 1:N

  # Add a `Rep` column as the first variable for the 2 replicates of experiments done
  Rep <- rep(c("Rep1", "Rep2"), N / 2)

  # Combine all columns into a DataFrame.
  df <- tibble::tibble(
    Placebo = placebo,
    Drug = drug,
    Genotype = genotype,
    ID = id,
    Rep = Rep
  )

  df <- df %>%
    tidyr::gather(key = Treatment, value = Measurement, -ID, -Genotype, -Rep)

  return(df)
}
