# Rank sort, and plot model data
# functions: run_vis2()
# subroutines: run_rank()

require(data.table)
require(ggplot2)
# require(ggjoy)
run_vis2 <- function(results, save = FALSE) {
  #---Sanitize input------------------------------------
  stopifnot(is.data.table(results))
  #---Maximum outbreak length subroutine-----------------
  run_rank <- function() {
    # A subroutine to calculate the maximum outbreak time for each run_rank
    # and sort by this length.
    # Args:
    #   none.
    # Returns:
    #   results : (data.table) with the maximum times appended
    #---Initialize variables-----------------------------------------
    n <- 0
    m <<- results[order(iter, time)][.N, iter]
    tf <- results[.N, "time"]
    rank <- vector()
    maxes <- vector()
    #---Error check for outbreaks that ran over simulation time-------
    setkey(results, time)
    proc <- results[.(tf)][, I]
    if (any(proc != 0)) {
      stop("Outbreak ran over simulation at least once!")
    }
    # re-sort to ensure proper analysis
    setkey(results, iter, time)
    #---Call root finder function on the Infection counts--------------
    results[, roots := Croots(I)]
    #---re-sort for faster subsetting----------------------------------
    setkey(results, iter, roots)
    #---The meat of 'er------------------------------------------------
    maxes <- foreach(i = 1:results[.N, iter],
                     .combine = 'c') %do% {
                       n <- results[iter == i][, .N]
                       outbreak_time <- 0
                       mat <- results[.(i, TRUE)]
                       for (j in 2:nrow(mat)) {
                         if (mat$I[j - 1] > 0) {
                           # Reduce memory usage from O(N) to O(1)
                           if ((mat$time[j] - mat$time[j - 1]) > outbreak_time) {
                             outbreak_time <- (mat$time[j] - mat$time[j - 1])
                           }
                         }
                       }
                       rep(outbreak_time, n)
                     }
    #---Append the results--------------------------------------------
    results[, "max" := maxes]
    #---Rank----------------------------------------------------------
    results[, "rank" := frank(results, -max, cols = "max",
                              ties.method = "dense")]
  }
  #---Run subroutine------------------------------------
  run_rank()
  #---plot----------------------------------------------
  # How would a 2d density plot look?
  graph <- ggplot(results, aes(time, rank, z = I)) +
            geom_point(aes(colour = I)) +
            # stat_density2d() +
            labs(x = "Time (days)",
                 y = "Iteration",
                 caption = "Sorted by maximum outbreak length",
                 title = "Density plot of infection count",
                 subtitle = paste0("in ", m, " model iterations")) +
            theme(panel.background = element_rect(fill = "#132B43"),
                  panel.grid = element_blank(),
                  plot.title = element_text(size = 20,
                                            hjust = 0.5),
                  plot.subtitle = element_text(size = 17,
                                               hjust = 0.5),
                  plot.caption = element_text(size = 10))
#   graph <- ggplot(results, aes(x = time, y = rank))
#     for (i in (results[.N, rank] - 10):results[.N, rank]) {
#       graph <- graph + geom_joy(data = results[rank == i], scale = 100,
#                                 size = 0.5, rel_min_height = 0, fill = "blue",
#                                 alpha = 0.2)
#     }
# #     for (i in 1:10) {
# #       graph <- graph + geom_joy(data = results[rank == i], scale = 100,
# #                        size = 0.5, rel_min_height = 0, fill = "blue",
# #                        alpha = 0.2)
# #     }
#   graph <- graph + theme_joy() +
#       ggtitle("Model results") +
#       labs(x = "Time (days)",
#            y = "Iterations",
#            caption = paste0("Results of ", n, " Stochastic Runs"))

  plot(graph)
  if (save) {
    graph <<- graph
    print("Saved.")
  }
}
