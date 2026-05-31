rm(list = ls())

load("~/HW4_simulation.RData")
library(ggplot2)

needed_cols <- c("n", "trial", "method", "valid", "clique_size")
stopifnot(all(needed_cols %in% names(simulation_results)))

# Keep only method runs that returned a valid clique with a recorded size.
valid_results <- simulation_results[
  simulation_results$valid & !is.na(simulation_results$clique_size),
]

# For each generated graph, find the method(s) with the largest valid clique.
winner_list <- by(
  valid_results,
  list(valid_results$n, valid_results$trial),
  function(one_trial) {
    max_size <- max(one_trial$clique_size)
    one_trial[one_trial$clique_size == max_size, c("n", "trial", "method")]
  }
)

winner_df <- do.call(rbind, winner_list)
rownames(winner_df) <- NULL

# Count how often each method won for each graph size.
win_counts <- aggregate(
  x = list(number_wins = rep(1, nrow(winner_df))),
  by = list(n = winner_df$n, method = winner_df$method),
  FUN = sum
)

plot_grid <- expand.grid(
  n = sort(unique(simulation_results$n)),
  method = sort(unique(simulation_results$method))
)

win_counts <- merge(
  plot_grid,
  win_counts,
  by = c("n", "method"),
  all.x = TRUE
)

win_counts$number_wins[is.na(win_counts$number_wins)] <- 0
win_counts$method <- factor(win_counts$method, levels = sort(unique(win_counts$method)))
win_counts$n <- factor(win_counts$n, levels = sort(unique(win_counts$n)))

plot_results <- ggplot(
  win_counts,
  aes(x = method, y = number_wins, fill = n)
) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_y_continuous(breaks = seq(0, max(win_counts$number_wins))) +
  labs(
    title = "Partial Clique Simulation Results",
    subtitle = "A win means the method found a valid clique with the largest size for that trial",
    x = "Implementation number",
    y = "Number of wins",
    fill = "Number of nodes"
  ) +
  theme_minimal()

ggsave(
  filename = "HW4_simulation.png",
  plot = plot_results,
  width = 9,
  height = 6,
  units = "in",
  dpi = 300
)
