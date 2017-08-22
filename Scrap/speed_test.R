source("solutionp.R")
source("solutionpc.R")
source("datap.r")
require("adaptivetau")
require("parallel")
require("foreach")
require("doParallel")
require("iterators")
require("data.table")
require("ggplot2")
require("Rcpp")
require("microbenchmark")
sourceCpp("../src/lenfind.cpp")
sourceCpp("../src/Croots.cpp")
print("All Sourced")
solutions <- new.env()
bench <- microbenchmark("R Only" = solutions$run_2 <- solutionSpace(swe, insbound = seq(0.01, 0.08, 0.01),
                                   vaccbound = c(0.94),
                                   len = 365),
                        "C++ enhanced" = solutions$run_2c <- solutionSpacec(swe, insbound = seq(0.01, 0.08, 0.01),
                                                           vaccbound = c(0.94),
                                                           len = 365),
                           times = 5)
graph <- autoplot(bench) + ggtitle("Two Runs, 4x8 parameter space")
plot(graph)
