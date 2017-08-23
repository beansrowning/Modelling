# Modelling load file
# Aug 21 2017

# Check and load dependencies
depends <- list("adaptivetau", "Rcpp", "parallel",
            "doParallel", "data.table", "Rmpi")
for (pkg in depends) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dep = TRUE)
    require(pkg, character.only = TRUE)
  } else {
    require(pkg, character.only = TRUE)
  }
}
sourceCpp("../src/Croots.cpp")
sourceCpp("../src/lenfind.cpp")

# Source data and functions
files <- c("datap.r", "solution_mpi.R")
sapply(files, source, .GlobalEnv)

# Done.
rm("depends", "files", "pkg")
print("All dependencies loaded.")
