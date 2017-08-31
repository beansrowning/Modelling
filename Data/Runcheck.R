require(data.table)
source("data_test.R")

#---Sweden------------------------------
load("sweden_1.dat")
data_test(solutions)

load("sweden_2.dat")
data_test(solutions)

#---Malta-------------------------------
load("malta_1.dat")
data_test(solutions)

load("malta_2.dat")
data_test(solutions)

#---Latvia-------------------------------
load("latvia_1.dat")
data_test(solutions)

load("latvia_2.dat")
data_test(solutions)

# Done.
