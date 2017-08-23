source("loadmp.R")
solutions <- new.env()
print(paste0("Starting Run 1 - ", date()))
# Run 1
# Sweden model
# ------------
# Old and young equally likely to be introduced
# length = 365 days
# end lag = 600 days
# Introduction rates : 0.01-0.1
# Vaccination rates  : 0.90-0.98
# Parameter space area : 9x10 = 90
solutions$t1 <- system.time(solutions$run_1 <- solutionSpace(swe,
                                 insbound = c(0.01, 0.02, 0.03, 0.04, 0.05,
                                              0.06, 0.07, 0.08, 0.09, 0.1),
                                 vaccbound = c(0.90, 0.91, 0.92, 0.93,
                                               0.94, 0.95, 0.96, 0.97, 0.98),
                                 len = 365))
print("Run finished")
print(solutions$t1)
save(solutions, file = "1run.dat")
print(paste0("Done. - ", date()))
quit()