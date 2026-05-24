#' Compute a large partial clique in an adjacency matrix
#'
#' `compute_maximal_partial_clique_5()` searches for a large set of nodes whose
#' induced subgraph has edge density at least `alpha`. The method is a heuristic:
#' it tries deterministic greedy growth from many starting nodes, also uses a
#' peeling step that removes sparse nodes from larger candidate sets, and then
#' keeps the largest valid candidate found. Because the exact maximal partial
#' clique problem is computationally difficult, this function aims for a
#' reasonable valid answer rather than a guaranteed global optimum.
#'
#' @param adj_mat A symmetric binary adjacency matrix with 1s on the diagonal,
#'   no row names, no column names, and between 5 and 50 rows and columns.
#' @param alpha A single numeric value between 0.5 and 1 giving the required
#'   minimum edge density for the returned partial clique.
#' @param max_starts A positive integer giving the maximum number of seed nodes
#'   used for greedy starts.
#'
#' @return A list whose first element, `clique_idx`, is a numeric vector of node
#'   indices for the selected partial clique. The second element,
#'   `edge_density`, is the edge density among those selected nodes.
#' @export
#'
#' @examples
#' adj_mat <- diag(5)
#' adj_mat[1:3, 1:3] <- 1
#' compute_maximal_partial_clique_5(adj_mat, alpha = 0.8)
compute_maximal_partial_clique_5 <- function(adj_mat, alpha, max_starts = 50) {
  .cmpc_check_adj_mat_5(adj_mat)
  .cmpc_check_alpha_5(alpha)
  .cmpc_check_positive_integer_5(max_starts, "max_starts")

  n <- nrow(adj_mat)
  degrees <- rowSums(adj_mat) - 1
  node_order <- order(degrees, seq_len(n), decreasing = c(TRUE, FALSE))
  start_nodes <- node_order[seq_len(min(max_starts, n))]

  candidates <- list()

  for (seed in start_nodes) {
    candidates[[length(candidates) + 1]] <- .cmpc_grow(
      clique_idx = seed,
      adj_mat = adj_mat,
      alpha = alpha,
      degrees = degrees
    )
  }

  candidates[[length(candidates) + 1]] <- .cmpc_grow(
    clique_idx = .cmpc_make_valid(seq_len(n), adj_mat, alpha, degrees),
    adj_mat = adj_mat,
    alpha = alpha,
    degrees = degrees
  )

  for (seed in start_nodes) {
    neighbor_set <- c(seed, setdiff(which(adj_mat[seed, ] == 1), seed))
    candidates[[length(candidates) + 1]] <- .cmpc_grow(
      clique_idx = .cmpc_make_valid(neighbor_set, adj_mat, alpha, degrees),
      adj_mat = adj_mat,
      alpha = alpha,
      degrees = degrees
    )
  }

  for (prefix_size in seq_len(n)) {
    prefix_set <- node_order[seq_len(prefix_size)]
    candidates[[length(candidates) + 1]] <- .cmpc_grow(
      clique_idx = .cmpc_make_valid(prefix_set, adj_mat, alpha, degrees),
      adj_mat = adj_mat,
      alpha = alpha,
      degrees = degrees
    )
  }

  best <- candidates[[1]]
  for (candidate in candidates[-1]) {
    if (.cmpc_is_better(candidate, best, adj_mat)) {
      best <- candidate
    }
  }

  best <- sort(as.integer(best))
  list(
    clique_idx = best,
    edge_density = .cmpc_edge_density(adj_mat, best)
  )
}

.cmpc_check_adj_mat_5 <- function(adj_mat) {
  if (!is.matrix(adj_mat)) {
    stop("adj_mat must be a matrix.", call. = FALSE)
  }

  if (nrow(adj_mat) != ncol(adj_mat)) {
    stop("adj_mat must be square.", call. = FALSE)
  }

  if (nrow(adj_mat) < 5 || nrow(adj_mat) > 50) {
    stop("adj_mat must have between 5 and 50 rows and columns.",
         call. = FALSE)
  }

  if (!is.null(rownames(adj_mat)) || !is.null(colnames(adj_mat))) {
    stop("adj_mat must not have row names or column names.", call. = FALSE)
  }

  if (anyNA(adj_mat) || !all(adj_mat %in% c(0, 1))) {
    stop("adj_mat must contain only 0 and 1 values.", call. = FALSE)
  }

  if (!isTRUE(all(adj_mat == t(adj_mat)))) {
    stop("adj_mat must be symmetric.", call. = FALSE)
  }

  if (!all(diag(adj_mat) == 1)) {
    stop("adj_mat must have 1s along the diagonal.", call. = FALSE)
  }
}

.cmpc_check_alpha_5 <- function(alpha) {
  if (!is.numeric(alpha) || length(alpha) != 1 || is.na(alpha) ||
      !is.finite(alpha) || alpha < 0.5 || alpha > 1) {
    stop("alpha must be a single numeric value between 0.5 and 1.",
         call. = FALSE)
  }
}

.cmpc_check_positive_integer_5 <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1 || is.na(x) || !is.finite(x) ||
      x <= 0 || x %% 1 != 0) {
    stop(name, " must be a single positive integer.", call. = FALSE)
  }
}

.cmpc_edge_density <- function(adj_mat, clique_idx) {
  clique_size <- length(clique_idx)
  if (clique_size <= 1) {
    return(1)
  }

  n_edges <- (sum(adj_mat[clique_idx, clique_idx, drop = FALSE]) -
    clique_size) / 2
  n_possible_edges <- clique_size * (clique_size - 1) / 2
  n_edges / n_possible_edges
}

.cmpc_make_valid <- function(clique_idx, adj_mat, alpha, degrees) {
  clique_idx <- sort(unique(as.integer(clique_idx)))

  while (length(clique_idx) > 1 &&
         .cmpc_edge_density(adj_mat, clique_idx) + 1e-12 < alpha) {
    subgraph <- adj_mat[clique_idx, clique_idx, drop = FALSE]
    internal_degrees <- rowSums(subgraph) - 1
    remove_order <- order(internal_degrees, degrees[clique_idx], clique_idx)
    clique_idx <- clique_idx[-remove_order[1]]
  }

  clique_idx
}

.cmpc_grow <- function(clique_idx, adj_mat, alpha, degrees) {
  n <- nrow(adj_mat)
  clique_idx <- sort(unique(as.integer(clique_idx)))

  repeat {
    remaining <- setdiff(seq_len(n), clique_idx)
    if (length(remaining) == 0) {
      break
    }

    feasible_nodes <- integer(0)
    feasible_edges <- numeric(0)
    feasible_density <- numeric(0)

    for (node in remaining) {
      trial_idx <- c(clique_idx, node)
      trial_density <- .cmpc_edge_density(adj_mat, trial_idx)
      if (trial_density + 1e-12 >= alpha) {
        feasible_nodes <- c(feasible_nodes, node)
        feasible_edges <- c(feasible_edges, sum(adj_mat[node, clique_idx]))
        feasible_density <- c(feasible_density, trial_density)
      }
    }

    if (length(feasible_nodes) == 0) {
      break
    }

    pick_order <- order(
      feasible_edges,
      feasible_density,
      degrees[feasible_nodes],
      -feasible_nodes,
      decreasing = TRUE
    )
    clique_idx <- sort(c(clique_idx, feasible_nodes[pick_order[1]]))
  }

  clique_idx
}

.cmpc_is_better <- function(candidate, current_best, adj_mat) {
  if (length(candidate) != length(current_best)) {
    return(length(candidate) > length(current_best))
  }

  candidate_density <- .cmpc_edge_density(adj_mat, candidate)
  current_density <- .cmpc_edge_density(adj_mat, current_best)
  if (!isTRUE(all.equal(candidate_density, current_density))) {
    return(candidate_density > current_density)
  }

  paste(candidate, collapse = ",") < paste(current_best, collapse = ",")
}
