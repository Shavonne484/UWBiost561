#' Compute the maximal partial clique
#'
#' A function that computes the maximal partial clique within an adjacency
#' matrix given a set edge density for what constitutes a valid partial clique.
#'
#' This function identifies the maximal partial clique within
#' an adjacency matrix and its edge density. It uses a greedy process,
#' starting with each node in the matrix and iteratively expanding to new
#' neighbors with the greatest number of edge connections.
#'
#' If there are multiple maximal partial cliques found,
#' then only the first one identified is returned.
#'
#' @param adj_mat symmetric adjacency matrix with 5 to 50 rows/columns
#' @param alpha minimum edge density required for a valid partial clique
#'
#'
#' @return
#'
#' A list containing a maximal partial clique, along with its edge density.
#'
#' @author Nicos E. Soares \cr Maintainer: Nicos E. Soares <nicos.soares20@@gmail.com>
#' @export

compute_maximal_partial_clique_9 <- function(adj_mat, alpha){
  #Stop the function if:
  #a) there is more than one numeric value in alpha
  #b) adj_mat is not a symmetric matrix with numeric 0/1 values
  #c) adj_mat has 1 along the diagonal with 5-50 rows/columns
  #d) adj_mat has row/column names
  #e) alpha is not between 0.5 and 1
  stopifnot(
    is.matrix(adj_mat), isSymmetric.matrix(adj_mat),
    all(is.numeric(adj_mat)), all(adj_mat %in% c(0,1)),
    all(diag(adj_mat) == 1),
    is.null(dimnames(adj_mat)),
    nrow(adj_mat) >= 5, nrow(adj_mat) <= 50,
    is.numeric(alpha), length(alpha) == 1,
    alpha >= 0.5, alpha <= 1)

  n <- nrow(adj_mat)

  #Storage
  clique_idx <- c()

  for (seed in 1:n){
    #Start with one node, find neighbors, remove self link
    potent_clique <- c(seed)
    neighbor_nodes <- which(adj_mat[seed, ] == 1)
    neighbor_nodes <- neighbor_nodes[neighbor_nodes != seed]

    while(length(neighbor_nodes) > 0){
      #Check all nodes not currently in clique and temporary storage
      poss_nodes <- setdiff(1:n, potent_clique)
      neighbor_nodes <- c()
      neighbor_connect <- c()

      for(pn in poss_nodes){
        #Look at sub-clique for each potential node
        sub_clique <- c(potent_clique, pn)
        m <- length(sub_clique)
        sub_mat <- adj_mat[sub_clique, sub_clique]

        #Calculate edge density - "if" is to avoid /0
        num_edges <- (sum(sub_mat) - m)/2
        max_edges <- m * (m - 1) / 2
        edge_density <- if(m == 1) 1 else num_edges / max_edges

        #Keep node if it satisfies alpha
        if(edge_density >= alpha){
          neighbor_nodes <- c(neighbor_nodes, pn)
          neighbor_connect <- c(neighbor_connect, num_edges)
        }
      }

      #If no valid nodes left
      if(length(neighbor_nodes) == 0){break}

      #Choose neighbor with most connections, add to clique
      most_connect <- which.max(neighbor_connect)
      new_node <- neighbor_nodes[most_connect]
      potent_clique <- c(potent_clique, new_node)
    }

    #Replace clique_idx with potent_clique if it is the largest one
    #Ties in max clique size keep the first one found
    if(length(potent_clique) > length(clique_idx)){
      clique_idx <- potent_clique
    }
  }

  #Calculate edge_density for maximal clique
  m <- length(clique_idx)
  sub_mat <- adj_mat[clique_idx, clique_idx]
  num_edges <- (sum(sub_mat) - m)/2
  max_edges <- m * (m - 1) / 2
  edge_density <- if(m == 1) 1 else num_edges / max_edges

  return(list(
    clique_idx = clique_idx,
    edge_density = edge_density
  ))
}
