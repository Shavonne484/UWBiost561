#' Run a Partial Clique Simulation Study
#'
#' `run_partial_clique_simulation()` generates random graphs using
#' `generate_partial_clique()` and runs all 9 implementations of
#' `compute_maximal_partial_clique` on each generated graph.
#'
#' @param n_levels Numeric vector of graph sizes to study.
#' @param n_trials Number of random graphs to generate for each graph size.
#' @param clique_fraction Fraction of nodes in the planted partial clique.
#' @param clique_edge_density Edge density within the planted partial clique.
#' @param density_low Background edge probability outside the planted clique.
#' @param alpha Required partial clique density.
#' @param methods Numeric vector of implementation numbers to run.
#' @param time_limit Maximum number of seconds allowed for each method.
#'
#' @return A data frame with one row per graph-method combination.
#' @export
run_partial_clique_simulation <- function(n_levels = c(10, 20, 30),
                                          n_trials = 3,
                                          clique_fraction = 0.5,
                                          clique_edge_density = 0.95,
                                          density_low = 0.1,
                                          alpha = 0.95,
                                          methods = 1:9,
                                          time_limit = 30) {
  stopifnot(
    is.numeric(n_levels),
    length(n_levels) >= 1,
    all(n_levels %% 1 == 0),
    all(n_levels >= 5),
    all(n_levels <= 50),
    is.numeric(n_trials),
    length(n_trials) == 1,
    n_trials %% 1 == 0,
    n_trials >= 1,
    is.numeric(methods),
    all(methods %in% 1:9),
    is.numeric(time_limit),
    length(time_limit) == 1,
    time_limit <= 30
  )

  results <- list()
  result_counter <- 1

  # Repeat the simulation for each graph size in the study.
  for (n in n_levels) {
    # Generate multiple random graphs for each graph size.
    for (trial in seq_len(n_trials)) {
      graph_data <- generate_partial_clique(
        n = n,
        clique_fraction = clique_fraction,
        clique_edge_density = clique_edge_density,
        density_low = density_low
      )

      adj_mat <- graph_data$adj_mat

      # Run every selected method on the same graph
      for (method in methods) {
        start_time <- Sys.time()
        method_result <- tryCatch(
          compute_maximal_partial_clique_master(
            adj_mat = adj_mat,
            alpha = alpha,
            number = method,
            time_limit = time_limit
          ),
          error = function(e) {
            list(
              clique_idx = NA,
              edge_density = NA,
              status = "error",
              valid = FALSE
            )
          }
        )
        end_time <- Sys.time()

        elapsed_sec <- as.numeric(
          difftime(end_time, start_time, units = "secs")
        )

        if (!all(is.na(method_result$clique_idx))) {
          clique_size <- length(unique(method_result$clique_idx))
          # Recompute density to verify the method's returned clique.
          correct_density <- tryCatch(
            compute_correct_density(
              adj_mat = adj_mat,
              clique_idx = method_result$clique_idx
            ),
            error = function(e) NA_real_
          )
        } else {
          clique_size <- NA_integer_
          correct_density <- NA_real_
        }

        # Store one row for this graph-method combination.
        results[[result_counter]] <- data.frame(
          n = n,
          trial = trial,
          method = method,
          alpha = alpha,
          clique_fraction = clique_fraction,
          clique_edge_density = clique_edge_density,
          density_low = density_low,
          status = method_result$status,
          valid = method_result$valid,
          clique_size = clique_size,
          reported_edge_density = method_result$edge_density,
          correct_edge_density = correct_density,
          elapsed_sec = elapsed_sec
        )

        result_counter <- result_counter + 1
      }
    }
  }

  do.call(rbind, results)
}
