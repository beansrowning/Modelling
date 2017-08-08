# Run Visualization function
# 29 June 2017

require("ggplot2")
require("data.table")
require("splines")
#---Testing with GGjoy----------------------------------
if (!require("ggjoy")) {
  install.packages("ggjoy")
  require("ggjoy")
}

# TODO: Finish this function
run_vis <- function(results, save = FALSE, log = FALSE) {
  #---Sanitize / Handle as data.table-------------------
  if (!is.data.table(results)) {
    results <- as.data.table(results)
  }
  #---Grab number of iterations---------------------
  n <- max(results[, iter])

  #---Calculate summary statistics----------------------
  sum_vals <- c("min", "low", "mean", "high", "max")
  results[, time := round(time, 0)]
  results[, c("min", "low", "mean", "high", "max") :=
         list(min(I),
              quantile(I, 0.25),
              mean(I),
              quantile(I, 0.75),
              max(I)),
              by = time][, c("iter", "I") := NULL]

  results <- melt(results, id.vars = "time", measure.vars = sum_vals,
                  variable.name = "measure", value.name = "infected count")
  assign("results", results, envir = .GlobalEnv)

  sum_levels <- levels(results[, (measure)])
  #--- Sort by time for faster subsetting -------------------------
  setkey(results, time)
  graph <- ggplot(results, aes(x = time, y = measure, group = measure)) +
            geom_joy(data = results[measure == "min"], scale = 1000, size = 0.5,
                     rel_min_height = 0, fill = "blue", alpha = 0.2) +
            geom_joy(data = results[measure == "low"], scale = 1000, size = 0.5,
                     rel_min_height = 0, fill = "blue", alpha = 0.2) +
            geom_joy(data = results[measure == "mean"], scale = 1000, size = 0.5,
                     rel_min_height = 0, fill = "blue", alpha = 0.2) +
            geom_joy(data = results[measure == "high"], scale = 1000, size = 0.5,
                     rel_min_height = 0, fill = "blue", alpha = 0.2) +
            geom_joy(data = results[measure == "max"], scale = 1000, size = 0.5,
                     rel_min_height = 0, fill = "blue", alpha = 0.2) +
            theme_joy() +
            ggtitle("Simulation Results") +
            scale_y_discrete(breaks = 1:length(sum_levels),
                               labels = c("Min", "25%", "Mean", "75%", "Max")) +
            labs(x = "Time (days)",
                 caption = paste0("Results of ", n,
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
