source("load.R")

solutions <- new.env()

#---First Run-------------------------------------
# vaccine protection : 94, 95%
# Rate of introduction : 0.01-0.05
print(date())
print("Starting first run")
solutions$run_1 <- solutionSpace(swe, insbound = seq(0.01, 0.05, 0.01),
                                 vaccbound = c(0.94, 0.95),
                                 len = 365)
print("First run done")
print(date())
#---Second Run-------------------------------------
# vaccine protection : 93-96%
# Rate of introduction : 0.01-0.08
solutions$run_2 <- solutionSpace(swe, insbound = seq(0.01, 0.08, 0.01),
                                 vaccbound = seq(0.93, 0.96, 0.01),
                                 len = 365)
print("Second run done")
print(date())
print("Done.")
