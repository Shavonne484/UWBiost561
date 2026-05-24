#' Compute a Maximal Partial Clique
#'
#' `compute_maximal_partial_clique_4()` is a function for finding a large partial clique in an
#' undirected graph represented by an adjacency matrix. A set of nodes is treated
#' as a valid partial clique if the edge density among those nodes is at least
#' `alpha`.
#'
#' @param adj_mat A symmetric adjacency matrix containing only 0 and 1.
#' The matrix must have 1 on the diagonal, no row or column names,
#' and between 5 and 50 rows and columns.
#' @param alpha A single numeric value between 0.5 and 1 (inclusive).
#' This is the required edge density for a set of nodes
#' to be considered a valid partial clique.
#'
#' @returns A list whose first named element is `clique_idx` and second named
#'   element is `edge_density`.
#'   `clique_idx` is a numeric vector of node indices
#'   selected as the maximal partial clique.
#'   `edge_density` is the edge density among the selected nodes.
#' @export
#'
#' @examples
#' adj_mat <- matrix(0, nrow = 8, ncol = 8)
#' diag(adj_mat) <- 1
#' adj_mat[1:5, 1:5] <- 1
#'
#' compute_maximal_partial_clique_4(adj_mat, alpha = 0.9)
compute_maximal_partial_clique_4 <- function(adj_mat, alpha) {
  stopifnot(
    is.numeric(alpha),
    length(alpha) == 1,
    !is.na(alpha),
    alpha >= 0.5,
    alpha <= 1,
    is.matrix(adj_mat),
    nrow(adj_mat) == ncol(adj_mat),
    nrow(adj_mat) >= 5,
    nrow(adj_mat) <= 50,
    all(adj_mat %in% c(0, 1)),
    all(adj_mat == t(adj_mat)),
    all(diag(adj_mat) == 1),
    is.null(rownames(adj_mat)),
    is.null(colnames(adj_mat))
  )

  n <- nrow(adj_mat)

  compute_density_4 <- function(idx) {
    m <- length(idx)

    if (m <= 1) {
      return(1)
    }

    num_edge <- (sum(adj_mat[idx, idx]) - m) / 2
    max_edge <- m * (m - 1) / 2

    return(num_edge / max_edge)
  }

  best_clique <- 1
  best_density <- 1

  for (start_node in 1:n) {
    current_clique <- start_node
    remaining_nodes <- setdiff(1:n, current_clique)
    keep_going <- TRUE

    while (length(remaining_nodes) > 0 && keep_going) {
      candidate_results <- lapply(remaining_nodes, function(candidate_node) {
        candidate_clique <- c(current_clique, candidate_node)
        candidate_density <- compute_density_4(candidate_clique)

        list(
          node = candidate_node,
          clique = candidate_clique,
          density = candidate_density
        )
      })

      candidate_densities <- sapply(
        candidate_results,
        function(one_candidate) one_candidate$density
      )

      valid_candidates <- which(candidate_densities >= alpha)

      if (length(valid_candidates) == 0) {
        keep_going <- FALSE
      } else {
        best_candidate_position <- valid_candidates[
          which.max(candidate_densities[valid_candidates])
        ]

        current_clique <- candidate_results[[best_candidate_position]]$clique
        remaining_nodes <- setdiff(1:n, current_clique)
      }
    }

    current_density <- compute_density_4(current_clique)

    if (length(current_clique) > length(best_clique)) {
      best_clique <- current_clique
      best_density <- current_density
    } else if (length(current_clique) == length(best_clique) &&
               current_density > best_density) {
      best_clique <- current_clique
      best_density <- current_density
    }
  }

  best_clique <- sort(unique(best_clique))
  best_density <- compute_density_4(best_clique)

  return(list(
    clique_idx = best_clique,
    edge_density = best_density
  ))
}
