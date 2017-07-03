# Modelling load file
# June 28 2017

# Check and load dependencies
dep <- list("adaptivetau", "Rcpp", "tidyverse", "parallel", "doParallel",
            "foreach", "data.table")
for (pkg in dep) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dep = TRUE)
    require(pkg, character.only = TRUE)
  } else {
    require(pkg, character.only = TRUE)
  }
}

# Source data and functions
files <- c("data.r", "Epi_detect.R", "multicore.R")
sapply(files, source, .GlobalEnv)

# Done.
rm(list("dep", "files"))
print("Done.")

# TODO: S4 classes and methods? Object file definitions?
