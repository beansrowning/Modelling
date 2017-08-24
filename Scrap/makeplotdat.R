# Take in data.table run data from hyperparameter search
# output plot_dat to make a 3d plot in plot_ly
require("data.table")

makeplotdat <- function(dat) {
  # 3d Plotly graph data producing function
  # Transforms data produced by solutionSpace() into
  # data that plotly can understand
  # Args :
  #   dat : (data.table) from a solutionSpace run
  # Returns :
  #   (list) Data coerced into a format where :
  #     x = (numeric vector) values of ins
  #     y = (numeric vector) values of vacc
  #     z = (numeric matrix) x*y values of max
  stopifnot(is.data.table(dat))
  #---Cast data wide and store as a matrix----------
  z <- dcast(dat, vacc ~ ins, value.var = "max")
  z <- z[, vacc := NULL]
  z <- as.matrix(z)
  colnames(z) <- NULL
  return(list(x = unique(dat[, ins]),
              y = unique(dat[, vacc]),
              z = z))
}
