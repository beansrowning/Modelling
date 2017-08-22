# Modelling load file
# Aug 21 2017

# Check and load dependencies
depends <- list("adaptivetau", "Rcpp", "ggplot2", "parallel",
            "doParallel", "data.table")
for (pkg in depends) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dep = TRUE)
    require(pkg, character.only = TRUE)
  } else {
    require(pkg, character.only = TRUE)
  }
}
tryCatch(sourceCpp("../src/Croots.cpp"),
         warning = function(w) {
           print("Croots couldn't load, trying package instead... ")
           tryCatch(require("Croots"),
                    error = function(e){
                      print("Library failed to load. Is it installed?")
                      print(paste(e, w, sep = " "))
                    })
         })
# Source data and functions
files <- c("datap.r", "solutionp.R")
sapply(files, source, .GlobalEnv)

# Done.
rm("depends", "files", "pkg")
print("Done.")
