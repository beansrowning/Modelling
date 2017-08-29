# And so begins part 2 of this saga.
# Let us recap what we've done so far :
# We've started with question one, and the idea that there is some combination of
# variables: population size, MMR vaccination rate, and case introduction rate,
# which would yield an outbreak. We then took it two ways
# 1 : by fixing introduction rate to a fixed value, and changing population
# size from 300,000 to 500,000.
# 2 : by modelling three different nations and varying case introduction rate

# Now we will look at the second part, which is similiar to the first part, yet
# we will be adding a delay to the start of insertion time. 
# Since we anticipated this from the beginning, nothing about our model needs to
# change, save appending a different value to the p["start.time"] value i accordance
# to the length of delay we want to impose

# Here's what that will look like for Sweden: 
# -------------------------------------------
# Using : Gridsearch2.R (or Gridsearch2_mpi in the cluster)
# Using : Lenfind.cpp + Croots.cpp (C++ helper routines)
# Using : model_global.R (where all the models are located)

solutions <- new.env()
# Run 1 - 12 month delay
# Insertion rates :   0.01-0.1 
# vaccinations rates: 0.9-1
# Total grid size : 10 * 11 = 110
# Search depth : 10,000 runs
# All other population values fixed
# Case introduction over the course of 1 year
# Offest by 2000 days beyond
print(paste0("Begining Run 1 - ", date()))
# Start time at 12 mo
swe$parameters["start.time"] <- 365
solutions$t1 <- system.time(solutions$run_1 <- solutionSpace(swe,
                                    insbound = c(0.01, 0.02, 0.03, 0.04, 0.05,
                                                 0.06, 0.07, 0.08, 0.09, 0.1),
                                    vaccbound = c(0.9, 0.91, 0.92, 0.93, 0.94,
                                                  0.95, 0.96, 0.97, 0.98, 0.99, 1),
                                    # Len is start.time + insertion time 
                                    len = 730,
                                    # Offset is appended at the end of insertion
                                    offset = 2000))
print(paste0("Run 1 done - ", solutions$t1[3]))
save(solutions, file = "../../Data/sweden_2.dat")

# Run 2 - 24 month delay
# Insertion rates :   0.01-0.1 
# vaccinations rates: 0.9-1
# Total grid size : 10 * 11 = 110
# Search depth : 10,000 runs
# All other population values fixed
# Case introduction over the course of 1 year
# Offest by 2000 days beyond
print(paste0("Begining Run 2 - ", date()))
# Start time at 24 mo
swe$parameters["start.time"] <- 730
solutions$t2 <- system.time(solutions$run_2 <- solutionSpace(swe,
                                    insbound = c(0.01, 0.02, 0.03, 0.04, 0.05,
                                                 0.06, 0.07, 0.08, 0.09, 0.1),
                                    vaccbound = c(0.9, 0.91, 0.92, 0.93, 0.94,
                                                  0.95, 0.96, 0.97, 0.98, 0.99, 1),
                                    # Len is start.time + insertion time 
                                    len = 1095,
                                    # Offset is appended at the end of insertion
                                    offset = 2000))
print(paste0("Run 2 done - ", solutions$t2[3]))
save(solutions, file = "../../Data/sweden_2.dat")

# Run 3 - 36 month delay
# Insertion rates :   0.01-0.1 
# vaccinations rates: 0.9-1
# Total grid size : 10 * 11 = 110
# Search depth : 10,000 runs
# All other population values fixed
# Case introduction over the course of 1 year
# Offest by 2000 days beyond
source("../../Data/model_global.R")
print(paste0("Begining Run 3 - ", date()))
# Start time at 24 mo
swe$parameters["start.time"] <- 1095
solutions$t3 <- system.time(solutions$run_3 <- solutionSpace(swe,
                                    insbound = c(0.01, 0.02, 0.03, 0.04, 0.05,
                                                 0.06, 0.07, 0.08, 0.09, 0.1),
                                    vaccbound = c(0.9, 0.91, 0.92, 0.93, 0.94,
                                                  0.95, 0.96, 0.97, 0.98, 0.99, 1),
                                    # Len is start.time + insertion time 
                                    len = 1460,
                                    # Offset is appended at the end of insertion
                                    offset = 2000))
print(paste0("Run 3 done - ", solutions$t3[3]))
save(solutions, file = "../../Data/sweden_2.dat")

# For Malta, these runs will be similar: 
# --------------------------------------
# Using : Gridsearch2.R (or Gridsearch2_mpi in the cluster)
# Using : Lenfind.cpp + Croots.cpp (C++ helper routines)
# Using : model_global.R (where all the models are located)
solutions <- new.env()
# Run 1 - 12 month delay
# Insertion rates :   0.01-0.1 
# vaccinations rates: 0.9-1
# Total grid size : 10 * 11 = 110
# Search depth : 10,000 runs
# All other population values fixed
# Case introduction over the course of 1 year
# Offest by 2000 days beyond
print(paste0("Begining Run 1 - ", date()))
# Start time at 12 mo
malta$parameters["start.time"] <- 365
solutions$t1 <- system.time(solutions$run_1 <- solutionSpace(malta,
                                    insbound = c(0.01, 0.02, 0.03, 0.04, 0.05,
                                                 0.06, 0.07, 0.08, 0.09, 0.1),
                                    vaccbound = c(0.9, 0.91, 0.92, 0.93, 0.94,
                                                  0.95, 0.96, 0.97, 0.98, 0.99, 1),
                                    # Len is start.time + insertion time 
                                    len = 730,
                                    # Offset is appended at the end of insertion
                                    offset = 2000))
print(paste0("Run 1 done - ", solutions$t1[3]))
save(solutions, file = "../../Data/malta_2.dat")

# Run 2 - 24 month delay
# Insertion rates :   0.01-0.1 
# vaccinations rates: 0.9-1
# Total grid size : 10 * 11 = 110
# Search depth : 10,000 runs
# All other population values fixed
# Case introduction over the course of 1 year
# Offest by 2000 days beyond
print(paste0("Begining Run 2 - ", date()))
# Start time at 24 mo
malta$parameters["start.time"] <- 730
solutions$t2 <- system.time(solutions$run_2 <- solutionSpace(malta,
                                    insbound = c(0.01, 0.02, 0.03, 0.04, 0.05,
                                                 0.06, 0.07, 0.08, 0.09, 0.1),
                                    vaccbound = c(0.9, 0.91, 0.92, 0.93, 0.94,
                                                  0.95, 0.96, 0.97, 0.98, 0.99, 1),
                                    # Len is start.time + insertion time 
                                    len = 1095,
                                    # Offset is appended at the end of insertion
                                    offset = 2000))
print(paste0("Run 2 done - ", solutions$t2[3]))
save(solutions, file = "../../Data/malta_2.dat")

# Run 3 - 36 month delay
# Insertion rates :   0.01-0.1 
# vaccinations rates: 0.9-1
# Total grid size : 10 * 11 = 110
# Search depth : 10,000 runs
# All other population values fixed
# Case introduction over the course of 1 year
# Offest by 2000 days beyond
print(paste0("Begining Run 3 - ", date()))
# Start time at 24 mo
malta$parameters["start.time"] <- 1095
solutions$t3 <- system.time(solutions$run_3 <- solutionSpace(malta,
                                    insbound = c(0.01, 0.02, 0.03, 0.04, 0.05,
                                                 0.06, 0.07, 0.08, 0.09, 0.1),
                                    vaccbound = c(0.9, 0.91, 0.92, 0.93, 0.94,
                                                  0.95, 0.96, 0.97, 0.98, 0.99, 1),
                                    # Len is start.time + insertion time 
                                    len = 1460,
                                    # Offset is appended at the end of insertion
                                    offset = 2000))
print(paste0("Run 3 done - ", solutions$t3[3]))
save(solutions, file = "../../Data/malta_2.dat")

# Finally, Latvia: 
# --------------------------------------
# Using : Gridsearch2.R (or Gridsearch2_mpi in the cluster)
# Using : Lenfind.cpp + Croots.cpp (C++ helper routines)
# Using : model_global.R (where all the models are located)
solutions <- new.env()
# Run 1 - 12 month delay
# Insertion rates :   0.01-0.1 
# vaccinations rates: 0.9-1
# Total grid size : 10 * 11 = 110
# Search depth : 10,000 runs
# All other population values fixed
# Case introduction over the course of 1 year
# Offest by 2000 days beyond
print(paste0("Begining Run 1 - ", date()))
# Start time at 12 mo
latvia$parameters["start.time"] <- 365
solutions$t1 <- system.time(solutions$run_1 <- solutionSpace(latvia,
                                    insbound = c(0.01, 0.02, 0.03, 0.04, 0.05,
                                                 0.06, 0.07, 0.08, 0.09, 0.1),
                                    vaccbound = c(0.9, 0.91, 0.92, 0.93, 0.94,
                                                  0.95, 0.96, 0.97, 0.98, 0.99, 1),
                                    # Len is start.time + insertion time 
                                    len = 730,
                                    # Offset is appended at the end of insertion
                                    offset = 2000))
print(paste0("Run 1 done - ", solutions$t1[3]))
save(solutions, file = "../../Data/latvia_2.dat")

# Run 2 - 24 month delay
# Insertion rates :   0.01-0.1 
# vaccinations rates: 0.9-1
# Total grid size : 10 * 11 = 110
# Search depth : 10,000 runs
# All other population values fixed
# Case introduction over the course of 1 year
# Offest by 2000 days beyond
print(paste0("Begining Run 2 - ", date()))
# Start time at 24 mo
latvia$parameters["start.time"] <- 730
solutions$t2 <- system.time(solutions$run_2 <- solutionSpace(latvia,
                                    insbound = c(0.01, 0.02, 0.03, 0.04, 0.05,
                                                 0.06, 0.07, 0.08, 0.09, 0.1),
                                    vaccbound = c(0.9, 0.91, 0.92, 0.93, 0.94,
                                                  0.95, 0.96, 0.97, 0.98, 0.99, 1),
                                    # Len is start.time + insertion time 
                                    len = 1095,
                                    # Offset is appended at the end of insertion
                                    offset = 2000))
print(paste0("Run 2 done - ", solutions$t2[3]))
save(solutions, file = "../../Data/latvia_2.dat")

# Run 3 - 36 month delay
# Insertion rates :   0.01-0.1 
# vaccinations rates: 0.9-1
# Total grid size : 10 * 11 = 110
# Search depth : 10,000 runs
# All other population values fixed
# Case introduction over the course of 1 year
# Offest by 2000 days beyond
print(paste0("Begining Run 3 - ", date()))
# Start time at 24 mo
latvia$parameters["start.time"] <- 1095
solutions$t3 <- system.time(solutions$run_3 <- solutionSpace(latvia,
                                    insbound = c(0.01, 0.02, 0.03, 0.04, 0.05,
                                                 0.06, 0.07, 0.08, 0.09, 0.1),
                                    vaccbound = c(0.9, 0.91, 0.92, 0.93, 0.94,
                                                  0.95, 0.96, 0.97, 0.98, 0.99, 1),
                                    # Len is start.time + insertion time 
                                    len = 1460,
                                    # Offset is appended at the end of insertion
                                    offset = 2000))
print(paste0("Run 3 done - ", solutions$t3[3]))
save(solutions, file = "../../Data/latvia_2.dat")
