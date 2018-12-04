require(data.table)
# load("../Data/sweden_2.dat")
# solutions$run_1[, delay := 12]
# solutions$run_2[, delay := 24]
# solutions$run_3[, delay := 36]
# b <- solutions$run_1[ins == 0.1]
# d <- solutions$run_2[ins == 0.1]
# e <- solutions$run_3[ins == 0.1]
# load("../Data/sweden_1.dat")
# solutions$run_1[, delay := 0]
# a <- solutions$run_1[ins == 0.1]
# rm("run_1", "t1", envir = solutions)
# solutions$global <- rbind(a, b, d, e)
# save(solutions, file = "../Data/sweden_five.dat")

# load("../Data/sweden_2_wc.dat")
# solutions$run_1[, delay := 12]
# solutions$run_2[, delay := 24]
# solutions$run_3[, delay := 36]
# b <- solutions$run_1[ins == 0.1]
# d <- solutions$run_2[ins == 0.1]
# e <- solutions$run_3[ins == 0.1]
# load("../Data/sweden_1_wc.dat")
# solutions$run_1[, delay := 0]
# a <- solutions$run_1[ins == 0.1]
# rm("run_1", "t1", envir = solutions)
# solutions$global <- rbind(a, b, d, e)
# save(solutions, file = "../Data/sweden_five_wc.dat")

# load("../Data/malta_2.dat")
# solutions$run_1[, delay := 12]
# solutions$run_2[, delay := 24]
# solutions$run_3[, delay := 36]
# b <- solutions$run_1[ins == 0.1]
# d <- solutions$run_2[ins == 0.1]
# e <- solutions$run_3[ins == 0.1]
# load("../Data/malta_1.dat")
# solutions$run_1[, delay := 0]
# a <- solutions$run_1[ins == 0.1]
# rm("run_1", "t1", envir = solutions)
# solutions$global <- rbind(a, b, d, e)
# save(solutions, file = "../Data/malta_five.dat")
#
# load("../Data/malta_2_wc.dat")
# solutions$run_1[, delay := 12]
# solutions$run_2[, delay := 24]
# solutions$run_3[, delay := 36]
# b <- solutions$run_1[ins == 0.1]
# d <- solutions$run_2[ins == 0.1]
# e <- solutions$run_3[ins == 0.1]
# load("../Data/malta_1_wc.dat")
# solutions$run_1[, delay := 0]
# a <- solutions$run_1[ins == 0.1]
# rm("run_1", "t1", envir = solutions)
# solutions$global <- rbind(a, b, d, e)
# save(solutions, file = "../Data/malta_five_wc.dat")
#
# load("../Data/latvia_2.dat")
# solutions$run_1[, delay := 12]
# solutions$run_2[, delay := 24]
# solutions$run_3[, delay := 36]
# b <- solutions$run_1[ins == 0.1]
# d <- solutions$run_2[ins == 0.1]
# e <- solutions$run_3[ins == 0.1]
# load("../Data/latvia_1.dat")
# solutions$run_1[, delay := 0]
# a <- solutions$run_1[ins == 0.1]
# rm("run_1", "t1", envir = solutions)
# solutions$global <- rbind(a, b, d, e)
# save(solutions, file = "../Data/latvia_five.dat")
#
load("../Data/latvia_2_wc.dat")
solutions$run_1[, delay := 12]
solutions$run_2[, delay := 24]
solutions$run_3[, delay := 36]
b <- solutions$run_1[ins == 0.1]
d <- solutions$run_2[ins == 0.1]
e <- solutions$run_3[ins == 0.1]
load("../Data/latvia_1_wc.dat")
solutions$run_1[, delay := 0]
a <- solutions$run_1[ins == 0.1]
rm("run_1", "t1", envir = solutions)
solutions$global <- rbind(a, b, d, e)
save(solutions, file = "../Data/latvia_five_wc.dat")
