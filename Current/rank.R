# Rank sort, and plot model data
# functions: run_vis2()
# subroutines: run_rank()

require(data.table)
require(ggplot2)
require(ggjoy)
run_vis2 <- function(results, save = FALSE) {
  #---Sort by outbreak length---------------------------
  run_rank()
  #---plot----------------------------------------------
  graph <- ggplot(results, aes(x = time, y = rank))
    for (i in (results[.N, rank] - 10):results[.N, rank]) {
      graph <- graph + geom_joy(data = results[rank == i], scale = 100, 
                                size = 0.5, rel_min_height = 0, fill = "blue",
                                alpha = 0.2)
    }
#     for (i in 1:10) {
#       graph <- graph + geom_joy(data = results[rank == i], scale = 100, 
#                        size = 0.5, rel_min_height = 0, fill = "blue",
#                        alpha = 0.2)
#     }
  graph <- graph + theme_joy() +
      ggtitle("Model results") +
      labs(x = "Time (days)",
           y = "Iterations",
           caption = paste0("Results of ", n, " Stochastic Runs"))
             
  plot(graph)
  if (save) {
    graph <<- graph
    print("Saved.")
  }
}

run_rank <- function() {
  # A subroutine to calculate the maximum outbreak time for each run_rank
  # and sort by this length.
  # Args:
  #   none.
  # Returns:
  #   results : (data.table) with the maximum times appended
  
  #---Ensure input exists-----------------------------------------------
  stopifnot("results" %in% ls())
  #---Initialize variables-----------------------------------------
  n <- 0
  m <- results[order(iter, time)][.N, iter]
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
  #---Append the results and sort-------------------------------------
  results[, max := maxes]
  setorder(results, -max, time)
  #---Rank------------------------------------------------------------
  ############################################################################
  # BUG : Somehow largest values of max are ordered last when put in the loop#
  #       Nothing seems to solve this, so the rank will alway be reversed.   #
  ############################################################################
  lvls <- unique(results[order(-max, time)][, iter])
  rank <- foreach(i = seq_along(lvls), .combine = 'c') %do% {
    num <- results[iter == lvls[i]][, .N]
    rep(i, num)
  }
  
  setorder(results, -max, time)
  setkey(results, max, time)
  results[, rank := rank]
  n <<- m
}
