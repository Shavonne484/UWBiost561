#' Compute the maximal partial clique given an adjacency matrix and required density
#'
#' @param adj_mat adjacency matrix that is symmetric, only has entries of 0 and 1, has row/column sizes between 5 and 50, has no row/col names, and has 1 across the diagonal
#' @param alpha required density for the partial clique, a number between 0.5 and 1
#'
#'The method works by beginning with considering the full set of nodes (ordered by degree/connectedness) and then seeing if the edge density requirement (for the given alpha) is achieved.
#'If it isn't, it removes the least connected node and tries again until a group of nodes (partial clique) fulfilling the edeg gensity requirements is found.
#'This is a faster solution but is not perfect, as it can exclude nodes that are indeed part of a grander partial clique early on.
#'Since we are generally specifying high alphas when testing, however, we are assuming that the partial cliques should be quite connected and thus nodes with very little connections often won't be part of the clique.
#'
#'
#' @returns a list with 1. the index positions of nodes in the maximum partial clique found and 2. the associated edge density for the partial clique
#' @export
compute_maximal_partial_clique_2 <- function(adj_mat, alpha) {
  stopifnot(
    isSymmetric(adj_mat), all(adj_mat %in% c(0, 1)), is.null(rownames(adj_mat)), is.null(colnames(adj_mat)), all(diag(adj_mat) == 1),
    nrow(adj_mat) >= 5, nrow(adj_mat) <= 50, ncol(adj_mat) >= 5, ncol(adj_mat) <= 50,
    length(alpha) ==1, alpha >= 0.5, alpha <= 1
  )

  #function for computing the denisty of a function..
  compute_edge_density_2 <- function(indecies) {
    m <- length(indecies)
    if (m <= 1) {
      return(1)
    }
    indexed_submatrix <- adj_mat[indecies, indecies, drop = FALSE]
    edges <- (sum(indexed_submatrix) - m) / 2
    possible_edges <-  m*(m-1) / 2
    edges / possible_edges
  }

  #(greedy pruning)
  n <- nrow(adj_mat)
  degrees <- rowSums(adj_mat) - 1
  current_nodes_considered <- order(degrees, decreasing = TRUE)
  while (length(current_nodes_considered) > 1) {
    current_density <- compute_edge_density_2(current_nodes_considered)

    if (current_density >= alpha) {
      break
    }
    submatrix_it <- adj_mat[current_nodes_considered, current_nodes_considered, drop = FALSE]
    degree_nodes <- rowSums(submatrix_it) - 1
    remove_node_idx <- which.min(degree_nodes)
    current_nodes_considered <- current_nodes_considered[-remove_node_idx]
  }


  edge_density <- compute_edge_density_2(current_nodes_considered)
  return(list(
    clique_idx = current_nodes_considered,
    edge_density = edge_density
  ))

}
