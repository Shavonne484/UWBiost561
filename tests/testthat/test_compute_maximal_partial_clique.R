context("Testing compute_maximal_partial_clique")

# Category 1: Checking that the function outputs the correct type
test_that("compute_maximal_partial_clique outputs a correctly structured list", {
  adj_mat <- matrix(0, nrow = 6, ncol = 6)
  diag(adj_mat) <- 1
  adj_mat[1:4, 1:4] <- 1

  output <- compute_maximal_partial_clique(adj_mat, alpha = 0.9)

  expect_true(is.list(output))
  expect_true("clique_idx" %in% names(output))
  expect_true("edge_density" %in% names(output))

  expect_true(is.numeric(output$clique_idx))
  expect_true(is.numeric(output$edge_density))
  expect_equal(length(output$edge_density), 1)
})


# Category 2: Simple checks to make sure result is within the correct range
test_that("compute_maximal_partial_clique returns values in valid ranges", {
  adj_mat <- matrix(0, nrow = 8, ncol = 8)
  diag(adj_mat) <- 1
  adj_mat[1:5, 1:5] <- 1

  output <- compute_maximal_partial_clique(adj_mat, alpha = 0.8)

  expect_true(all(output$clique_idx >= 1))
  expect_true(all(output$clique_idx <= 8))
  expect_true(output$edge_density >= 0)
  expect_true(output$edge_density <= 1)
  expect_true(output$edge_density >= 0.8)
})


# Category 3: Making sure the function gets the correct answer on a known example
test_that("compute_maximal_partial_clique finds a known complete clique", {
  adj_mat <- matrix(0, nrow = 10, ncol = 10)
  diag(adj_mat) <- 1

  # Nodes 1 to 5 form a complete clique
  adj_mat[1:5, 1:5] <- 1

  output <- compute_maximal_partial_clique(adj_mat, alpha = 1)

  expect_equal(output$clique_idx, 1:5)
  expect_equal(output$edge_density, 1)
})


# Category 4: Making sure the function runs on many different inputs
test_that("compute_maximal_partial_clique runs on several graph sizes", {
  set.seed(123)

  graph_sizes <- c(5, 10, 20, 30)

  for (n in graph_sizes) {
    output_generated <- generate_partial_clique(
      n = n,
      clique_fraction = 0.3,
      clique_edge_density = 1,
      density_low = 0
    )

    adj_mat <- output_generated$adj_mat

    output <- compute_maximal_partial_clique(adj_mat, alpha = 1)

    expect_true(is.list(output))
    expect_true(length(output$clique_idx) >= 1)
    expect_true(output$edge_density >= 1)
  }
})


# Unit test 5: Stress testing corner cases
test_that("compute_maximal_partial_clique handles corner cases gracefully", {

  #Corner case 1: smallest allowed matrix (n = 5)
  set.seed(1)
  output_small <- generate_partial_clique(
    n = 5,
    clique_fraction = 0.4,
    clique_edge_density = 1.0,
    density_low = 0
  )

  result_small <- compute_maximal_partial_clique(
    output_small$adj_mat,
    alpha = 1
  )

  expect_true(is.list(result_small))
  expect_equal(result_small$edge_density, 1)
  expect_true(all(result_small$clique_idx >= 1 & result_small$clique_idx <= 5))


  #Corner case 2: largest allowed matrix (n = 50)
  set.seed(2)
  output_large <- generate_partial_clique(
    n = 50,
    clique_fraction = 0.4,
    clique_edge_density = 1.0,
    density_low = 0
  )

  result_large <- compute_maximal_partial_clique(
    output_large$adj_mat,
    alpha = 1
  )

  expect_true(is.list(result_large))
  expect_equal(result_large$edge_density, 1)
  expect_true(all(result_large$clique_idx >= 1 & result_large$clique_idx <= 50))
  expect_gte(length(result_large$clique_idx), round(50 * 0.4))


  # Corner case 3: minimum allowed alpha = 0.5
  set.seed(3)
  output_alpha_low <- generate_partial_clique(
    n = 20,
    clique_fraction = 0.4,
    clique_edge_density = 0.6,
    density_low = 0
  )

  result_alpha_low <- compute_maximal_partial_clique(
    output_alpha_low$adj_mat,
    alpha = 0.5
  )

  expect_gte(result_alpha_low$edge_density, 0.5)
  expect_true(all(result_alpha_low$clique_idx >= 1 & result_alpha_low$clique_idx <= 20))


  #Corner case 4: alpha = 1.0
  n_strict <- 8
  adj_strict <- diag(1, nrow = n_strict, ncol = n_strict)
  adj_strict[1:5, 1:5] <- 1
  rownames(adj_strict) <- NULL
  colnames(adj_strict) <- NULL

  result_strict <- compute_maximal_partial_clique(
    adj_strict,
    alpha = 1.0
  )

  expect_equal(result_strict$clique_idx, 1:5)
  expect_equal(result_strict$edge_density, 1)


  # Corner case 5: diagonal-only graph
  adj_diag <- diag(1, nrow = 6, ncol = 6)
  rownames(adj_diag) <- NULL
  colnames(adj_diag) <- NULL

  result_diag <- compute_maximal_partial_clique(
    adj_diag,
    alpha = 0.5
  )

  expect_equal(length(result_diag$clique_idx), 1)
  expect_equal(result_diag$edge_density, 1)


  # Corner case 6: complete graph
  n_complete <- 7
  adj_complete <- matrix(1L, nrow = n_complete, ncol = n_complete)
  rownames(adj_complete) <- NULL
  colnames(adj_complete) <- NULL

  result_complete <- compute_maximal_partial_clique(
    adj_complete,
    alpha = 0.5
  )

  expect_equal(length(result_complete$clique_idx), n_complete)
  expect_equal(result_complete$edge_density, 1)
})
