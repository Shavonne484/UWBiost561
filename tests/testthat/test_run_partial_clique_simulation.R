context("Testing run_partial_clique_simulation")

# Category 1: Checking that the function outputs the correct type
test_that("run_partial_clique_simulation outputs a correctly structured data frame", {
  output <- run_partial_clique_simulation(
    n_levels = 5,
    n_trials = 1,
    methods = 4,
    time_limit = 2
  )

  expect_true(is.data.frame(output))
  expect_equal(nrow(output), 1)

  expected_names <- c(
    "n",
    "trial",
    "method",
    "alpha",
    "clique_fraction",
    "clique_edge_density",
    "density_low",
    "status",
    "valid",
    "clique_size",
    "reported_edge_density",
    "correct_edge_density",
    "elapsed_sec"
  )

  expect_true(all(expected_names %in% names(output)))
  expect_true(is.numeric(output$n))
  expect_true(is.numeric(output$method))
  expect_true(is.character(output$status))
  expect_true(is.logical(output$valid))
  expect_true(is.numeric(output$elapsed_sec))
})


# Category 2: Make sure result is within the correct range
test_that("run_partial_clique_simulation returns values in valid ranges", {
  output <- run_partial_clique_simulation(
    n_levels = 6,
    n_trials = 1,
    methods = c(1, 4),
    time_limit = 2
  )

  expect_true(all(output$n >= 5))
  expect_true(all(output$n <= 50))
  expect_true(all(output$method %in% 1:9))
  expect_true(all(output$alpha >= 0.5))
  expect_true(all(output$alpha <= 1))
  expect_true(all(output$status %in% c("completed", "timed_out", "error")))
  expect_true(all(output$elapsed_sec >= 0))

  non_missing_sizes <- output$clique_size[!is.na(output$clique_size)]
  expect_true(all(non_missing_sizes >= 1))
  expect_true(all(non_missing_sizes <= output$n[!is.na(output$clique_size)]))

  non_missing_densities <- output$correct_edge_density[
    !is.na(output$correct_edge_density)
  ]
  expect_true(all(non_missing_densities >= 0))
  expect_true(all(non_missing_densities <= 1))
})


# Category 3: Making sure the function gets the correct answer on a known example
test_that("run_partial_clique_simulation finds the full clique in a complete graph", {
  output <- run_partial_clique_simulation(
    n_levels = 5,
    n_trials = 1,
    clique_fraction = 1,
    clique_edge_density = 1,
    density_low = 0,
    alpha = 1,
    methods = 4,
    time_limit = 2
  )

  expect_equal(output$status, "completed")
  expect_true(output$valid)
  expect_equal(output$clique_size, 5)
  expect_equal(output$reported_edge_density, 1)
  expect_equal(output$correct_edge_density, 1)
})


# Category 4: Making sure the function runs on many different inputs
test_that("run_partial_clique_simulation runs on several graph sizes and trials", {
  output <- run_partial_clique_simulation(
    n_levels = c(5, 6, 7),
    n_trials = 2,
    methods = 4,
    time_limit = 2
  )

  expect_equal(nrow(output), 6)
  expect_equal(sort(unique(output$n)), c(5, 6, 7))
  expect_equal(sort(unique(output$trial)), c(1, 2))
  expect_true(all(output$method == 4))
})


# Unit test 5: Stress testing corner cases
test_that("run_partial_clique_simulation handles corner cases gracefully", {

  # Corner case 1: smallest allowed graph, using all 9 methods
  output_small <- run_partial_clique_simulation(
    n_levels = 5,
    n_trials = 1,
    methods = 1:9,
    time_limit = 2
  )

  expect_equal(nrow(output_small), 9)
  expect_equal(sort(output_small$method), 1:9)
  expect_true(all(output_small$status %in% c("completed", "timed_out", "error")))

  # Corner case 2: maximum allowed time limit
  output_time_limit <- run_partial_clique_simulation(
    n_levels = 5,
    n_trials = 1,
    methods = 4,
    time_limit = 30
  )

  expect_true(is.data.frame(output_time_limit))
  expect_equal(nrow(output_time_limit), 1)

  # Corner case 3: invalid n larger than allowed should cause an error
  expect_error(
    run_partial_clique_simulation(
      n_levels = 51,
      n_trials = 1,
      methods = 4,
      time_limit = 2
    )
  )

  # Corner case 4: invalid time limit larger than allowed should cause an error
  expect_error(
    run_partial_clique_simulation(
      n_levels = 5,
      n_trials = 1,
      methods = 4,
      time_limit = 31
    )
  )
})
