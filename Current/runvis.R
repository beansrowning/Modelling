# Run Visualization function
# 29 June 2017

require("ggplot2")
require("data.table")
require("splines")

# TODO: Finish this function
run_vis <- function(results, save = FALSE) {
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
  lb10 <- .results[, .(I.lb10 = quantile(I, 0.10)), by = t2]
  lb25 <- .results[, .(I.lb25 = quantile(I, 0.25)), by = t2]
  ub75 <- .results[, .(I.ub75 = quantile(I, 0.75)), by = t2]
  ub90 <- .results[, .(I.ub90 = quantile(I, 0.90)), by = t2]
  setkey(mean, t2)
  setkey(lb10, t2)
  setkey(lb25, t2)
  setkey(ub75, t2)
  setkey(ub90, t2)
  sum_dat <- merge(mean, lb10, all.x = TRUE)
  sum_dat <- merge(sum_dat, lb25, all.x = TRUE)
  sum_dat <- merge(sum_dat, ub75, all.x = TRUE)
  sum_dat <- merge(sum_dat, ub90, all.x = TRUE)
  colnames(sum_dat) <- c("time", "mean", "lower", "low", "high", "higher")
  rm("mean", "lb10", "lb25", "ub75", "ub90")
  assign("sum_dat", sum_dat, envir = .GlobalEnv)
  n <- max(results$iter)
  #---Plot data-------------------------------------------
  graph <- ggplot(sum_dat) +
            geom_point(aes(x = time, y = mean), alpha = 0.1) +
            geom_smooth(aes(x = time, y = mean), size = 0.5, method = "glm",
                        formula = y ~ bs(x, 20)) +
            geom_smooth(aes(x = time, y = low), size = 0.25, alpha = 0.5,
                        method = "glm", formula = y ~ bs(x, 20))
            geom_smooth(aes(x = time, y = high), size = 0.25, alpha = 0.5,
                        method = "glm", formula = y ~ bs(x, 20))
            # geom_ribbon(aes(x = time, ymin = low, ymax = high),
            #             alpha = 0.25) +
            # geom_ribbon(aes(x = time, ymin = lower, ymax = higher),
            #             alpha = 0.25) +
            ggtitle(paste0("Results of ", n, " stochastic runs")) +
            xlab("Time (days)") +
            ylab("Infected (count)") +
            theme(legend.position = "right") +
            scale_x_log10()
  plot(graph)
  if (save == TRUE) {
    assign("graph", graph, envir = parent.frame())
  }
}
