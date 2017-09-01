# Combination plots
require(data.table)
require(ggplot2)

#---Plot 1: Mean outbreak length varying with weighted introduction------
makeplotOne <- function(ins_val, logy = FALSE, xlab = NULL, ylab = NULL,
                        zlab = NULL, title = NULL, sub = NULL, scalelab = NULL,
                        scalevals = vector()) {
  # This function allows for the comparison of more than one run on a single
  # plot. It is assumed that the x value will be found in run_1-3 and in the
  # solutions environment.
  #---Data shaping----------------------------------------------
  plot_1_dat <- solutions$run_1[, c("ins", "vacc", "mean")]
  plot_1_dat[, mean2 := solutions$run_2[, mean]]
  plot_1_dat[, mean3 := solutions$run_3[, mean]]
  plot_1_dat <- plot_1_dat[ins == ins_val]
  plot_1_dat <- melt(plot_1_dat, id.vars = "vacc",
                     measure.vars = c("mean", "mean2", "mean3"),
                     variable.name = "Mean",
                     value.name = "Length")
  plot_1_dat <<- plot_1_dat
  #---plotting--------------------------------------------------
  plot_1 <- ggplot(plot_1_dat, aes(x = vacc, y = Length, colour = Mean)) +
              geom_line() +
              theme_bw()
  #---Adjust if LOG---------------------------------------------
  if (logy) {
    plot_1 <- plot_1 + scale_y_log10()
  }
  #---More Style if needed--------------------------------------
  if (!is.null(xlab) || !is.null(ylab) || !is.null(title) || !is.null(sub)) {
    plot_1 <- plot_1 + labs(x = xlab,
                          y = ylab,
                          title = title,
                          subtitle = sub) +
                    theme(plot.title = element_text(size = 20,
                                                    hjust = 0.5),
                          plot.subtitle = element_text(size = 17,
                                                       hjust = 0.5),
                          panel.grid.major.y = element_line(
                            colour="#d5d5d5"
                            ))
  }
  #---And even more----------------------------------------------
  if (!is.null(scalelab) || length(scalevals) > 0) {
    plot_1 <- plot_1 + scale_colour_manual(name = scalelab,
                                           values = c("red", "green", "blue"),
                                           breaks = c("mean", "mean2", "mean3"),
                                           labels = scalevals)
  }
  #---Return------------------------------------------------------
  graph <<- plot_1
}
