# Defines 2 and 3 dimensional plotting functions for a single run
# threedPlot(), twodPlot()
require(ggplot2)
require(plotly)
require(data.table)

threedPlot <- function(data, envir = .GlobalEnv, variables, cutoff = c(365),
                       xlab = "x", ylab = "y", zlab = "z", title = "") {
  # A function to generate 3D plots in plotly from gridsearch runs
  # Uses makeplotdat function and plot_ly to produce both graph data
  # and return the plot data
  # Args :
  #   data : (data.table) where the plot data can be found
  #   envir : (environment) where the output should be assigned
  #   variables : (character vector) of the variables to plot
  #   cutoff : (numeric vector) values of z to show a cutoff plane
  #   xlab : (character) label for the x-axis
  #   ylab : (character) label for the y-axis
  #   zlab : (character) label for the z-axis
  #   title : (character) Title of the graph
  # Returns :
  #   graph : Plot_ly 3d surface plot using the variables provided
  #   plot_dat : (data.table) plot data for bug checking

  #---Sanitize those inputs------------------------------------------
  stopifnot(is.data.table(data),
            is.vector(variables) && length(variables) > 0 && is.character(variables),
            is.environment(envir))
  #---Define data casting routine------------------------------------
  makeplotdat <- function(dat, zedd) {
    # 3d Plotly graph data producing function
    # Transforms data produced by solutionSpace() into
    # data that plotly can understand
    # Args :
    #   dat : (data.table) from a solutionSpace run
    #   zedd: (String) the name of the value representing z
    # Returns :
    #   (list) Data coerced into a format where :
    #     x = (numeric vector) values of ins
    #     y = (numeric vector) values of vacc
    #     z = (numeric matrix) x*y values of variable specified

    #---Cast data wide and store as a matrix----------
    z <- dcast(dat, vacc ~ ins, value.var = zedd)
    z <- z[, vacc := NULL]
    z <- as.matrix(z)
    colnames(z) <- NULL
    return(list(x = unique(dat[, ins]),
                y = unique(dat[, vacc]),
                z = z))
  }
  #---Initialize-----------------------------------------------------
  plot_dat <- makeplotdat(data, variables[1])
  newz <- matrix()
  #---And Plot-------------------------------------------------------
  graph <- plot_ly(x = plot_dat$x, y = plot_dat$y) %>%
                  add_surface(z = plot_dat$z) %>%
                  layout(title = title,
                         scene = list(xaxis = list(title = xlab),
                                      yaxis = list(title = ylab),
                                      zaxis = list(title = zlab)))
  #---If there are more variable, grab those surfaces----------------
  if (length(variables) > 1) {
    for (i in 2:length(variables)) {
      newz <- makeplotdat(data, variables[i])$z
      nam <- paste0("z",i)
      plot_dat[[nam]] <- newz
      graph <- graph %>% add_surface(z = newz)
    }
  }
  #---Add a cutoff to show roughly what values are relevant-----------
  if (!is.null(cutoff)) {
    for (i in 1:length(cutoff)) {
      mask <- matrix(cutoff[i],
                     nrow = nrow(plot_dat$z),
                     ncol = ncol(plot_dat$z))
      graph <- graph %>% add_surface(z = mask, opacity = 0.5, showscale = FALSE)
    }

  }

  #---Return data-----------------------------------------------------
  assign("graph", graph, envir)
  assign("plot_dat", plot_dat, envir)
}

twodPlot <- function(dat, xvar, yvar, zvar, zval,
                     logy = FALSE, xlab = NULL,
                     ylab = NULL, title = NULL,
                     sub = NULL) {
  # This function will look at two dimensions of output, with the option
  # of having multiple y variable levels (such as different summary
  # measures of outbreak length). Additionally, a z variable must be specified
  # to determine which observations to get the data from.
  # Args :
  #  dat  : (data.table) of the data to use for the plot
  #  Xvar : (character) The name of the variable on the x-axis
  #  yvar : (character) The name of the variable on the y-axis
  #         optionally, a character vector may be passed if more than
  #         one variable should be shown
  #  zvar : (character) the name of the z variable to subset from
  #  zval : (integer) The value of the z variable in the table which gives
  #       rise to the previous two.
  #  logy : (bool) Should the y-axis scale logarithmically?
  # Returns :
  #  graph : ggplot output given the inputs specified

  #---Init-----------------------------------------------
  plot_dat <- data.table()
  #---Data shaping---------------------------------------
  plot_dat <- dat[get(zvar) == zval]
  plot_dat <- melt(plot_dat, id.vars = xvar,
                      measure.vars = yvar,
                      variable.name = "Measure",
                      value.name = "Length")
  #---Ploting--------------------------------------------
  graph <- ggplot(plot_dat, aes(x = get(xvar), y = Length, colour = Measure)) +
            geom_line() +
            theme_bw()
  #---Adjust if LOG--------------------------------------
  if (logy) {
    graph <- graph + scale_y_log10(breaks = c(0, 10, 50, 100, 365, 500, 730))
  }
  #---More Style if needed-------------------------------
  if (!is.null(xlab) || !is.null(ylab) || !is.null(title) || !is.null(sub)) {
    graph <- graph + labs(x = xlab,
                          y = ylab,
                          title = title,
                          subtitle = sub) +
                    theme(plot.title = element_text(size = 20,
                                                    hjust = 0.5),
                          plot.subtitle = element_text(size = 17,
                                                       hjust = 0.5),
                          panel.grid.major.y = element_line(colour = "#d5d5d5"))
  }
  #---Return---------------------------------------------
  graph <<- graph
}

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
                          panel.grid.major.y = element_line(colour = "#d5d5d5"))
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
