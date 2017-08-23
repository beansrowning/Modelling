# Modelling load file for OpenMP

# Check and load dependencies
depends <- list("adaptivetau", "Rcpp", "parallel",
            "doParallel", "data.table", "foreach")
for (pkg in depends) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dep = TRUE)
    require(pkg, character.only = TRUE)
  } else {
    require(pkg, character.only = TRUE)
  }
}
tryCatch(sourceCpp("./Croots.cpp"),
         error = function(w) {
           print("Croots couldn't load, trying package instead... ")
           tryCatch(system("cd ../../Data/; R CMD INSTALL Croots"),
                    error = function(e){
                      print("Library failed to load. Is it installed?")
                      print(paste(e, w, sep = " "))
                    })
         })
tryCatch(sourceCpp("./lenfind.cpp"),
                    error = function(e) {
                      stop(e)
                      })

# Source data and functions
files <- c("datap.r", "../Parameterized/solution_mp.R")
sapply(files, source, .GlobalEnv)

# Done.
rm("depends", "files", "pkg")
print("All dependencies loaded.")