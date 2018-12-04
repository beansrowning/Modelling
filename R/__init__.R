# Modelling load file
# June 28 2017

# Check and load dependencies
depends <- list("adaptivetau", "Rcpp", "ggplot2", "splines", "parallel",
            "doParallel", "data.table")
for (pkg in depends) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dep = TRUE)
    require(pkg, character.only = TRUE)
  } else {
    require(pkg, character.only = TRUE)
  }
}

# Source data and functions
files <- c("data.r", "Epi_detect.R", "multicore.R", "runvis.R", "handling.R")
sapply(files, source, .GlobalEnv)

# Done.
rm("depends", "files", "pkg")
print("Done.")
