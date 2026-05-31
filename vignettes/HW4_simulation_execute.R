rm(list = ls())

library(UWBiost561)

n_levels <- c(10, 20, 30)
n_trials <- 3
clique_fraction <- 0.5
clique_edge_density <- 0.95
density_low <- 0.1
alpha <- 0.95
methods <- 1:9
time_limit <- 30

simulation_results <- UWBiost561::run_partial_clique_simulation(
  n_levels = n_levels,
  n_trials = n_trials,
  clique_fraction = clique_fraction,
  clique_edge_density = clique_edge_density,
  density_low = density_low,
  alpha = alpha,
  methods = methods,
  time_limit = time_limit
)

date_of_run <- Sys.time()
session_info <- devtools::session_info()

save(
  simulation_results,
  n_levels,
  n_trials,
  clique_fraction,
  clique_edge_density,
  density_low,
  alpha,
  methods,
  time_limit,
  date_of_run,
  session_info,
  file = "~/HW4_simulation.RData"
)
