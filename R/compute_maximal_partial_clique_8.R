#' Compute partial clique
#'
#' Randomly try a bunch of cliques and return the largest one that passes the threshold
#'
#' @param adj_mat an adjacency matrix that is 0,1
#' @param alpha minimum edge density
#' @param time_limit maximum number of seconds to run before returning the best clique found so far
#'
#' @return a list containing the maximum partial clique and its density
#' @export
compute_maximal_partial_clique_8 <- function(adj_mat, alpha, time_limit = 15) {
  n <- nrow(adj_mat)
  max_combn <- 2^n
  attempts <- ceiling(max_combn/2)

  max_clique_idx <- numeric(0)
  max_edge_density <- 1

  start_time <- Sys.time()

  for(i in 1:attempts){
    if(as.numeric(Sys.time() - start_time) >= time_limit) break

    set.seed(i)
    attempted_clique <- which(sample(c(0,1), size = n, replace = TRUE) == 1)
    if(length(attempted_clique) == 0) attempted_clique <- 1

    dens <- .compute_density_8(adj_mat, attempted_clique)
    if(dens > alpha & length(attempted_clique) > length(max_clique_idx)){
      max_edge_density <- dens
      max_clique_idx <- attempted_clique
    }
  }

  return(list(clique_idx = max_clique_idx,
              edge_density = max_edge_density))
}

.compute_density_8 <- function(adj_mat, clique_idx){
  m <- length(clique_idx)
  if(length(clique_idx) == 1) return(0)

  total_size <- m*(m-1)/2
  adj_size <- (sum(adj_mat[clique_idx, clique_idx]) - m)/2

  return(adj_size/total_size)
}
