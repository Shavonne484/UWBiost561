#' Generate a Random Graph with a Partial Clique
#'
#'`generate_partial_clique()` creates an `n x n` symmetric adjacency matrix
#' for an undirected graph. A subset of nodes is chosen to form a partial clique,
#' where the number of clique nodes is determined by `clique_fraction` and the
#' number of edges among those clique nodes is determined by
#' `clique_edge_density`.
#'
#' @param n A positive integer.
#' It means the number of nodes in the graph, and
#' therefore the number of rows and columns in the output adjacency matrix.
#' @param clique_fraction A single numeric value between 0 and 1 (inclusive).
#'   It is the fraction of the `n` nodes that are included in the partial clique.
#' @param clique_edge_density A single numeric value between 0 and 1 (inclusive).
#'   It is the desired edge density among the clique nodes.
#' @param density_low A single numeric value between 0 and 1 (inclusive).
#' It is the background probability of an edge in the random graph before the partial
#'   clique structure is imposed. Default value is 0.1.
#'
#' @returns A list whose first named element is `adj_mat`. `adj_mat` is an `n x n` symmetric adjacency
#' matrix containing only 0s and 1s, with 1s on the diagonal and no row or column names.
#' @export
#'
#' @examples
#' output <- generate_partial_clique(
#'   n = 10,
#'   clique_fraction = 0.4,
#'   clique_edge_density = 0.8
#' )
#'
generate_partial_clique <- function(n = 50,
                                    clique_fraction = 0.2,
                                    clique_edge_density = 1,
                                    density_low = 0.1) {
  stopifnot(
    is.numeric(n),
    length(n) == 1,
    !is.na(n),
    n %% 1 == 0,
    n > 0,

    is.numeric(clique_fraction),
    length(clique_fraction) == 1,
    !is.na(clique_fraction),
    clique_fraction >= 0,
    clique_fraction <= 1,

    is.numeric(clique_edge_density),
    length(clique_edge_density) == 1,
    !is.na(clique_edge_density),
    clique_edge_density >= 0,
    clique_edge_density <= 1,

    is.numeric(density_low),
    length(density_low) == 1,
    !is.na(density_low),
    density_low >= 0,
    density_low <= 1
  )

  adj_mat <- matrix(
    sample(
      x = c(0, 1),
      size = n^2,
      prob = c(1 - density_low, density_low),
      replace = TRUE
    ),
    nrow = n,
    ncol = n
  )

  adj_mat <- adj_mat + t(adj_mat)
  adj_mat[adj_mat > 0] <- 1
  diag(adj_mat) <- 1

  clique_size <- round(n * clique_fraction)

  if (clique_size >= 2) {
    possible_edges <- t(combn(1:clique_size, 2))

    max_edges <- clique_size * (clique_size - 1) / 2

    n_clique_edges <- round(clique_edge_density * max_edges)

    adj_mat[1:clique_size, 1:clique_size] <- 0
    diag(adj_mat)[1:clique_size] <- 1

    if (n_clique_edges > 0) {
      selected_edges <- possible_edges[
        sample(seq_len(max_edges), size = n_clique_edges, replace = FALSE),
        ,
        drop = FALSE
      ]

      for (i in seq_len(nrow(selected_edges))) {
        node1 <- selected_edges[i, 1]
        node2 <- selected_edges[i, 2]

        adj_mat[node1, node2] <- 1
        adj_mat[node2, node1] <- 1
      }
    }
  }

  sample_idx <- sample(seq_len(n))
  adj_mat <- adj_mat[sample_idx, sample_idx]

  rownames(adj_mat) <- NULL
  colnames(adj_mat) <- NULL

  return(list(adj_mat = adj_mat))
}
