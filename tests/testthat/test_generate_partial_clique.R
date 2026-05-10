context("Testing generate_partial_clique")

# Unit test 1: valid output test
test_that("generate_partial_clique outputs a valid adjacency matrix", {
  set.seed(10)

  output <- generate_partial_clique(
    n = 10,
    clique_fraction = 0.4,
    clique_edge_density = 0.8,
    density_low = 0.1
  )

  adj_mat <- output$adj_mat

  # Test output is a list with adj_mat
  expect_true(is.list(output))
  expect_true("adj_mat" %in% names(output))

  # Test adjacency matrix dimensions
  expect_equal(dim(adj_mat), c(10, 10))

  # Test matrix contains only 0s and 1s
  expect_true(all(adj_mat %in% c(0, 1)))

  # Test matrix is symmetric
  expect_true(all(adj_mat == t(adj_mat)))

  # Test diagonal entries are all 1
  expect_true(all(diag(adj_mat) == 1))

  # Test no row or column names
  expect_null(rownames(adj_mat))
  expect_null(colnames(adj_mat))
})


# Unit test 2: edge cases
test_that("generate_partial_clique handles edge cases correctly", {
  set.seed(20)

  output <- generate_partial_clique(
    n = 5,
    clique_fraction = 0,
    clique_edge_density = 1,
    density_low = 0
  )

  adj_mat <- output$adj_mat

  # With density_low = 0 and clique_fraction = 0, only diagonal entries should be 1
  expect_equal(sum(adj_mat), 5)
  expect_true(all(diag(adj_mat) == 1))
  expect_true(all(adj_mat[lower.tri(adj_mat)] == 0))
  expect_true(all(adj_mat[upper.tri(adj_mat)] == 0))

  output_full <- generate_partial_clique(
    n = 6,
    clique_fraction = 1,
    clique_edge_density = 1,
    density_low = 0
  )

  adj_mat_full <- output_full$adj_mat

  # With clique_fraction = 1 and clique_edge_density = 1, all nodes should form a complete clique
  expect_true(all(adj_mat_full == 1))
})
