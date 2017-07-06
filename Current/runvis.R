# Run Visualization function
# 29 June 2017

require("ggplot2")
require("data.table")
require("splines")

# TODO: Finish this function
run_vis <- function(results, save = FALSE, log = FALSE) {
  #---Round off time input to daily---------------------
  results$t2 <- round(results$time, 0)
  #---Sanitize / Handle as data.table-------------------
  if (!is.data.table(results)) {
    .results <- as.data.table(results)
  } else {
    .results <- results
  }
  #---Calculate summary statistics----------------------
  mean <- .results[, .(I.mean = mean(I)), by = t2]
  min <- .results[, .(I.lb10 = min(I)), by = t2]
  lb25 <- .results[, .(I.lb25 = quantile(I, 0.25)), by = t2]
  ub75 <- .results[, .(I.ub75 = quantile(I, 0.75)), by = t2]
  max <- .results[, .(I.ub90 = max(I)), by = t2]
  setkey(mean, t2)
  setkey(min, t2)
  setkey(lb25, t2)
  setkey(ub75, t2)
  setkey(max, t2)
  sum_dat <- merge(mean, min, all.x = TRUE)
  sum_dat <- merge(sum_dat, lb25, all.x = TRUE)
  sum_dat <- merge(sum_dat, ub75, all.x = TRUE)
  sum_dat <- merge(sum_dat, max, all.x = TRUE)
  colnames(sum_dat) <- c("time", "mean", "min", "low", "high", "max")
  rm("mean", "min", "lb25", "ub75", "max")
  assign("sum_dat", sum_dat, envir = .GlobalEnv)
  n <- max(results$iter)
  #---Plot data-------------------------------------------
  graph <- ggplot(sum_dat) +
            geom_point(aes(x = time, y = max), alpha = 0.1, colour = "green") +
            geom_point(aes(x = time, y = min), alpha = 0.1, colour = "red") +
            geom_smooth(aes(x = time, y = mean), size = 0.5, method = "glm",
                        formula = y ~ bs(x, 20)) +
            # geom_smooth(aes(x = time, y = low), size = 0.25, alpha = 0.5,
            #             method = "glm", formula = y ~ bs(x, 20)) +
            # geom_smooth(aes(x = time, y = high), size = 0.25, alpha = 0.5,
            #             method = "glm", formula = y ~ bs(x, 20)) +
            # geom_ribbon(aes(x = time, ymin = low, ymax = high),
            #             alpha = 0.25) +
            # geom_ribbon(aes(x = time, ymin = lower, ymax = higher),
            #             alpha = 0.25) +
            ggtitle(paste0("Results of ", n, " Stochastic Runs")) +
            xlab("Time (days)") +
            ylab("Infected (count)") +
            theme(legend.position = "right")
  if (log) {
    graph <- graph + scale_x_log10()
  }
  plot(graph)
  if (save == TRUE) {
    assign("graph", graph, envir = parent.frame())
  }
}
