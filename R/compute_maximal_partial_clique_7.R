#' Compute the maximal partial clique in an adjacency matrix
#'
#' Uses a greedy seed-and-grow approach followed by local search to find
#' a large set of nodes whose induced subgraph meets the required edge
#' density \code{alpha}. Starting from the node with the highest degree,
#' the algorithm greedily adds nodes that keep the density above \code{alpha},
#' then attempts to swap out nodes one at a time to grow the clique further.
#' Multiple random restarts are used to avoid local optima.
#'
#' @param adj_mat A symmetric 0/1 matrix with 1s on the diagonal, no row/col
#'   names, and between 5 and 50 rows/columns.
#' @param alpha Numeric in `[0.5, 1]`. Required edge density among clique nodes.
#'
#' @return A list with \code{clique_idx} (integer vector of node indices) and
#'   \code{edge_density} (numeric, the actual edge density of the returned set).
#' @export
compute_maximal_partial_clique_7 <- function(adj_mat, alpha) {
  # --- Input validation ---------------------------------------------------
  stopifnot(
    "adj_mat must be a matrix"            = is.matrix(adj_mat),
    "adj_mat must contain only 0s and 1s"  = all(adj_mat %in% c(0L, 1L)),
    "adj_mat must be symmetric"            = isSymmetric(adj_mat),
    "adj_mat must have 1s on the diagonal" = all(diag(adj_mat) == 1),
    "adj_mat must have no row/col names"   = is.null(rownames(adj_mat)) &&
      is.null(colnames(adj_mat)),
    "adj_mat must have 5 to 50 rows"       = nrow(adj_mat) >= 5 &&
      nrow(adj_mat) <= 50,
    "alpha must be a single numeric"       = is.numeric(alpha) &&
      length(alpha) == 1,
    "alpha must be between 0.5 and 1"      = alpha >= 0.5 && alpha <= 1
  )
  n <- nrow(adj_mat)
  # --- Helper: compute edge density for a set of nodes -------------------
  .edge_density_7 <- function(idx) {
    m <- length(idx)
    if (m <= 1) return(1)
    (sum(adj_mat[idx, idx]) - m) / (m * (m - 1))
  }
  # --- Helper: greedy grow from a seed node ------------------------------
  .greedy_grow_7 <- function(seed) {
    clique <- seed
    candidates <- setdiff(1:n, seed)
    repeat {
      if (length(candidates) == 0) break
      scores <- sapply(candidates, function(v)
        sum(adj_mat[v, clique]))
      best <- candidates[which.max(scores)]
      candidate_clique <- c(clique, best)
      if (.edge_density_7(candidate_clique) >= alpha) {
        clique <- candidate_clique
        candidates <- setdiff(candidates, best)
      } else {
        candidates <- setdiff(candidates, best)
      }
    }
    clique
  }
  # --- Helper: local search (swap to improve) ----------------------------
  .local_search_7 <- function(clique) {
    improved <- TRUE
    while (improved) {
      improved <- FALSE
      outside <- setdiff(1:n, clique)
      for (v in outside) {
        candidate <- c(clique, v)
        if (.edge_density_7(candidate) >= alpha) {
          clique <- candidate
          improved <- TRUE
          break
        }
      }
    }
    clique
  }
  # --- Main: multiple restarts, keep best result -------------------------
  best_clique <- c(1)
  degrees <- rowSums(adj_mat) - 1
  seed_order <- order(degrees, decreasing = TRUE)
  seeds_to_try <- unique(c(
    seed_order[1:min(5, n)],
    sample(1:n, min(10, n))
  ))
  for (seed in seeds_to_try) {
    clique <- .greedy_grow_7(c(seed))
    clique <- .local_search_7(clique)
    if (length(clique) > length(best_clique)) {
      best_clique <- clique
    }
  }
  list(
    clique_idx   = sort(best_clique),
    edge_density = .edge_density_7(best_clique)
  )
}
