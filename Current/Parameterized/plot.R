require(plotly)
require(data.table)

threedPlot <- function(data, envir = .GlobalEnv, variables, cutoff = c(365), xlab = "x",
                   ylab = "y", zlab = "z", title = "") {
  # A function to generate 3D plots in plotly from gridsearch runs
  # Uses makeplotdat function and plot_ly to produce both graph data
  # and return the plot data
  # Args :
  #   TODO : This
  # Returns :

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
  plot <- plot_ly(x = plot_dat$x, y = plot_dat$y) %>%
                  add_surface(z = plot_dat$z) %>%
                  layout(title = title,
                         scene = list(xaxis = list(title = xlab),
                                      yaxis = list(title = ylab),
                                      zaxis = list(title = zlab)))
  #---If there are more variable, grab those surfaces----------------
  if (length(variables) > 1) {
    for (i in 2:length(variables)) {
      newz <- makeplotdat(data, variables[i])$z
      assign(paste0("plot_dat$z", i), newz)
      plot <- plot %>% add_surface(z = newz)
    }
  }
  #---Add a cutoff to show roughly what values are relevant-----------
  if (!is.null(cutoff)) {
    for (i in 1:length(cutoff)) {
      mask <- matrix(cutoff[i], nrow = nrow(plot_dat$z), ncol = ncol(plot_dat$z))
      plot <- plot %>% add_surface(z = mask, opacity = 0.5, showscale = FALSE)
    }

  }

  #---Return data-----------------------------------------------------
  assign("plot", plot, envir)
  assign("plot_dat", plot_dat, envir)
}
