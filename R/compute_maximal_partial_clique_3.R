#' find maximal partial clique
#'
#' @param adj_mat the matrix to look for maximal partial clique
#' @param alpha density that the partial clique needs to reach
#'
#' @returns returns a list of two things, the nodes in the maximal partial clique, and its density
#' @export
compute_maximal_partial_clique_3 <- function(adj_mat, alpha) {
  if (!is.matrix(adj_mat) ||nrow(adj_mat) != ncol(adj_mat) || !all(adj_mat == t(adj_mat))) {
    stop("adj_mat must be a symmetric matrix")
  }
  if (!all(adj_mat %in% c(0, 1))) {
    stop("adj_mat must only contain 0s and 1s")
  }
  if (any(diag(adj_mat) != 1)) {
    stop("Diagonal of adj_mat must be 1")
  }
  if (!is.null(rownames(adj_mat)) || !is.null(colnames(adj_mat))) {
    stop("adj_mat must not have row or column names")
  }
  if (nrow(adj_mat) < 5 || nrow(adj_mat) > 50) {
    stop("adj_mat must have between 5 and 50 rows/columns")
  }
  if (length(alpha) != 1 || !is.numeric(alpha) || alpha < 0.5 || alpha > 1) {
    stop("alpha must be a single numeric value between 0.5 and 1")
  }
  
  n <- dim(adj_mat)[1]
  ran_clique <- list()
  maximal_clique <- c()
  
  for (nodei in 1:n) {
    connected_nodes <- which(adj_mat[nodei, ] == 1)
    connected_nodes <- connected_nodes[connected_nodes != nodei]
    current_clique <- c(nodei, connected_nodes)
    
    # Ensure current clique fits alpha
    current_density <- calculate_density_3(current_clique, adj_mat)
    while (current_density < alpha) {
      current_clique <- remove_one_node_3(current_clique, adj_mat)
      current_density <- calculate_density_3(current_clique, adj_mat)
    }
    
    expanded_clique <- current_clique
    for (nodej in connected_nodes) {
      nodej_clique <- setdiff(which(adj_mat[nodej, ] == 1), current_clique)
      if (length(nodej_clique) >= 1) {
        for (nodej1 in nodej_clique) {
          expanded_clique <- c(expanded_clique, nodej1)
          expanded_density <- calculate_density_3(expanded_clique, adj_mat)
          
          if (expanded_density >= alpha) {
            current_clique <- expanded_clique
          }
          
          # Check if current clique is already in ran_clique to avoid redundancy
          if (!list_match_3(ran_clique, current_clique)) {
            if (length(current_clique) > length(maximal_clique)) {
              maximal_clique <- current_clique
            }
            ran_clique <- append(ran_clique, list(current_clique))
          }
        }
      }
    }
  }
  density <- calculate_density_3(maximal_clique, adj_mat)
  
  return(list(
    clique_idx = maximal_clique,
    edge_density = density
  ))
}

calculate_density_3 <- function(clique, adj_mat) {
  m <- length(clique)
  if (m <= 1){return(1)}
  num_edge <- 0
  for (i in 1:(m - 1)) {
    for (j in (i + 1):m) {
      if (adj_mat[clique[i], clique[j]] == 1) {
        num_edge <- num_edge + 1
      }
    }
  }
  max_edge <- m * (m - 1) / 2
  return(num_edge / max_edge)
}

#remove 1 node from the clique with greedy algorithm.
remove_one_node_3 <- function(clique, adj_mat) {
  densities <- sapply(clique, function(node) {
    temp_clique <- clique[clique != node]
    return(calculate_density_3(temp_clique, adj_mat))
  })
  node_to_remove <- clique[which.min(densities)]
  return(clique[clique != node_to_remove])
}

# if current clique circumstance is already ran
list_match_3 <- function(ran_clique, clique) {
  for (ran in ran_clique) {
    if (identical(ran, clique)) {
      return(TRUE)
    }
  }
  return(FALSE)
}