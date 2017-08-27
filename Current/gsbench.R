# Benchmark for Fork Vs PSOCK on the same PC
require(Rcpp)
require(data.table)
require(adaptivetau)
require(doParallel)
require(foreach)

if (!"Croots" %in% ls()) {
sourceCpp("./src/Croots.cpp")
}
if (!"lenFind" %in% ls()) {
sourceCpp("./src/lenfind.cpp")
}
cat("Running as a ", ifelse(.Platform$OS.type == "windows", "PSOCK", "FORK"), " cluster", "\n")
cat("On", detectCores(), " cores", "\n")
source("./Parameterized/gridsearch1.R")
source("../Data/model_global.R")
source("./Parameterized/get_popvalues.R")
measles_land$parameters["introduction.rate"] <- 0.01
t1 <- system.time(solution <- solutionSpace(measles_land,
                          count = 2000,
                          insbound = c(300000, 310000),
                          vaccbound = c(0.9),
                          len = 365,
                          offset = 2000,
                          sero.p = c(0.92, 0.92)))