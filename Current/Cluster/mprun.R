source("mpload.R")
solutions <- new.env()
print(paste0("Starting Run 1 - ", date()))
# Cluster openMP benchmark
solutions$t1 <- system.time(solutions$run_1 <- solutionSpace(swe,
                              insbound = c(0.01, 0.02),
                              vaccbound = c(0.90, 0.91),
                              len = 365))
print("Run finished")
print(solutions$t1)
save(solutions, file = "1run.dat")
print(paste0("Done. - ", date()))
quit()