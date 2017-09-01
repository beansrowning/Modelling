require(data.table)
source("data_test.R")

#---Sweden------------------------------
load("sweden_1.dat")
data_test(solutions)
stopifnot(is.null(warnings()))

load("sweden_2.dat")
data_test(solutions)
stopifnot(is.null(warnings()))

load("sweden_1_wc.dat")
data_test(solutions)
stopifnot(is.null(warnings()))

load("sweden_2_wc.dat")
data_test(solutions)
stopifnot(is.null(warnings()))
#---Malta-------------------------------
load("malta_1.dat")
data_test(solutions)
stopifnot(is.null(warnings()))

load("malta_2.dat")
data_test(solutions)
stopifnot(is.null(warnings()))

# load("malta_1_wc.dat")
# data_test(solutions)
# stopifnot(is.null(warnings()))
#
# load("malta_2_wc.dat")
# data_test(solutions)
# stopifnot(is.null(warnings()))
#---Latvia-------------------------------
load("latvia_1.dat")
data_test(solutions)
stopifnot(is.null(warnings()))

load("latvia_2.dat")
data_test(solutions)
stopifnot(is.null(warnings()))

load("latvia_1_wc.dat")
data_test(solutions)
stopifnot(is.null(warnings()))

load("latvia_2_wc.dat")
data_test(solutions)
stopifnot(is.null(warnings()))
# Done.
