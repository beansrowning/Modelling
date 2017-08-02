# Run Visualization function
# 29 June 2017

require("ggplot2")
require("data.table")
require("splines")
#---Testing with GGjoy----------------------------------
if(!require("ggjoy")) {
  install.packages("ggjoy")
  require("ggjoy")
}

# TODO: Finish this function
run_vis <- function(results, save = FALSE, log = FALSE) {
  #---Round off time input to daily---------------------
  .results <- results
  .results$t2 <- round(.results$time, 0)
  #---Sanitize / Handle as data.table-------------------
  if (!is.data.table(.results)) {
    .results <- as.data.table(.results)
  }
  #---Calculate summary statistics----------------------
  mean <- .results[, .(I.mean = mean(I)), by = t2]
  min <- .results[, .(I.lb10 = min(I)), by = t2]
  lb25 <- .results[, .(I.lb25 = quantile(I, 0.25)), by = t2]
  ub75 <- .results[, .(I.ub75 = quantile(I, 0.75)), by = t2]
  max <- .results[, .(I.ub90 = max(I)), by = t2]

  # sapply(sum_vals, setkey, t2)
  # setkey(mean, t2)
  # setkey(min, t2)
  # setkey(lb25, t2)
  # setkey(ub75, t2)
  # setkey(max, t2)
  sum_vals <- c("mean", "min", "low", "high", "max")
  # for (var in sum_vals) {
  #   setkey(var, t2)
  #   sum_dat <- merge(sum_dat, var, all.x = TRUE)
  # }
  sum_dat <- merge(mean, min, all.x = TRUE)
  sum_dat <- merge(sum_dat, lb25, all.x = TRUE)
  sum_dat <- merge(sum_dat, ub75, all.x = TRUE)
  sum_dat <- merge(sum_dat, max, all.x = TRUE)
  colnames(sum_dat) <- c("time", "mean", "min", "low", "high", "max")
  sum_dat <- melt(sum_dat, id.vars = "time", measure.vars = sum_vals,
                  variable.name = "measure", value.name = "infected count")
  rm("mean", "min", "lb25", "ub75", "max", "sum_vals")
  # assign("sum_dat", sum_dat, envir = parent.frame())
  #---Plot data-------------------------------------------
  # graph <- ggplot(sum_dat) +
  #           geom_point(aes(x = time, y = max), alpha = 0.1, colour = "green") +
  #           geom_point(aes(x = time, y = min), alpha = 0.1, colour = "red") +
  #           geom_smooth(aes(x = time, y = mean), size = 0.5, method = "glm",
  #                       formula = y ~ bs(x, 20)) +
  #           # geom_smooth(aes(x = time, y = low), size = 0.25, alpha = 0.5,
  #           #             method = "glm", formula = y ~ bs(x, 20)) +
  #           # geom_smooth(aes(x = time, y = high), size = 0.25, alpha = 0.5,
  #           #             method = "glm", formula = y ~ bs(x, 20)) +
  #           # geom_ribbon(aes(x = time, ymin = low, ymax = high),
  #           #             alpha = 0.25) +
  #           # geom_ribbon(aes(x = time, ymin = lower, ymax = higher),
  #           #             alpha = 0.25) +
  #           ggtitle(paste0("Results of ", n, " Stochastic Runs")) +
  #           xlab("Time (days)") +
  #           ylab("Infected (count)") +
  #           theme(legend.position = "right")
  sum_levels <- levels(sum_dat[,(measure)])
  setkey(sum_dat, time)
  graph <- ggplot(sum_dat, aes(x = time, y = measure, group = measure)) +
            geom_joy(data = sum_dat[measure == "min"], scale = 1000, size = .5,
                     rel_min_height = 0, fill = "red", alpha = 0.2) +
            geom_joy(data = sum_dat[measure == "low"], scale = 1000, size = .5,
                     rel_min_height = 0, fill = "red", alpha = 0.2) +
            geom_joy(data = sum_dat[measure == "mean"], scale = 1000, size = .5,
                     rel_min_height = 0, fill = "red", alpha = 0.2) +
            geom_joy(data = sum_dat[measure == "high"], scale = 1000, size = 0.5,
                     rel_min_height = 0, fill = "red", alpha = 0.2) +
            geom_joy(data = sum_dat[measure == "max"], scale = 1000, size = .5,
                     rel_min_height = 0, fill = "red", alpha = 0.2) +
            theme_joy() +
            ggtitle("Simulation Results") +
            scale_y_discrete(breaks = 1:length(sum_levels),
                               labels = function(x) {sum_levels[x]}) +
            labs(x = "Time (days)",
                 y = "",
                 caption = paste0("Results of ", max(results$iter),
                                  " Stochastic Runs"))
  if (log) {
    graph <- graph + scale_x_log10()
  }
  plot(graph)
  if (save) {
    assign("graph", graph, envir = parent.frame())

  }
  rm(list = ls())
}
