#' Compute maximal partial clique
#'
#' @param adj_mat adj_mat Numeric matrix (n_n) of 0/1's, symmetric, 1's on diagonal,
#'        no row/col names, with 5:50.
#' @param alpha Single numeric between 0.5 and 1, the minimum required edge-density.
#'
#' @return A list with clique_idx and edge_density
#' @export

compute_maximal_partial_clique_6 <- function(adj_mat, alpha) {
  
  ## --- 1. INPUT VALIDATION ---------------------------------------
  # Must be a numeric matrix
  if (!is.matrix(adj_mat) || !is.numeric(adj_mat)) {
    stop("`adj_mat` must be a numeric matrix.")
  }
  n <- nrow(adj_mat)
  # Must be square
  if (n != ncol(adj_mat)) {
    stop("`adj_mat` must be square.")
  }
  # No row or column names
  if (!is.null(rownames(adj_mat)) || !is.null(colnames(adj_mat))) {
    stop("`adj_mat` must not have row or column names.")
  }
  # Size between 5 and 50
  if (n < 5 || n > 50) {
    stop("`adj_mat` must have between 5 and 50 rows/columns.")
  }
  # Entries only 0 or 1
  if (!all(adj_mat %in% c(0, 1))) {
    stop("All entries of `adj_mat` must be 0 or 1.")
  }
  # Diagonal must be all 1's
  if (any(diag(adj_mat) != 1)) {
    stop("`adj_mat` must have 1's on the diagonal.")
  }
  # Must be symmetric (undirected graph)
  if (!all(adj_mat == t(adj_mat))) {
    stop("`adj_mat` must be symmetric.")
  }
  
  # Validate alpha
  if (!is.numeric(alpha) || length(alpha) != 1) {
    stop("`alpha` must be a single numeric value.")
  }
  if (alpha < 0.5 || alpha > 1) {
    stop("`alpha` must be between 0.5 and 1 (inclusive).")
  }
  
  ## --- 3. GREEDY-REMOVAL LOOP -------------------------------------
  # Start with all nodes in S
  S <- seq_len(n)
  d <- edge_density_6(S, adj_mat)    # current density
  
  # While we are below target alpha and have >1 node
  while (d < alpha && length(S) > 1) {
    
    # 3a. For each candidate node k, compute density if we drop k
    densities <- sapply(S, function(k) {
      edge_density_6(setdiff(S, k), adj_mat)
    })
    
    # 3b. Find the node whose removal yields the highest density
    idx_to_drop <- which.max(densities)
    k_remove    <- S[idx_to_drop]
    
    # 3c. Remove it from S, update current density
    S <- setdiff(S, k_remove)
    d <- edge_density_6(S, adj_mat)
  }
  
  
  ## --- 4. RETURN RESULT ----------------------------------------
  list(
    clique_idx   = S,
    edge_density = d
  )
}

## --- 2. HELPER: EDGE-DENSITY CALCULATION --------------------------
# Given a vector of node indices S, compute
#   (# of edges among S) / choose(|S|, 2)
edge_density_6 <- function(S, adj_mat) {
  m <- length(S)
  # A single node trivially has "density 1"
  if (m < 2) return(1)
  submat     <- adj_mat[S, S, drop = FALSE]
  # Sum only upper triangle to count each undirected edge once
  num_edges  <- sum(submat[upper.tri(submat)])
  denom      <- choose(m, 2)  # total possible edges among m nodes
  num_edges / denom
}