# benchmarking ed_subroutine
require("parallel")
require("foreach")
require("data.table")
require("Rcpp")
require("microbenchmark")
require("adaptivetau")
require("ggplot2")
source("multicore.R")
source("data.r")
if (!"Croots" %in% ls()) {
  tryCatch(sourceCpp("Croots.cpp"),
           error = function(e) {
             stop("Could not load src/Croots.cpp, does it exist? ", e)
           })
}
gettype <- ifelse(.Platform$OS.type == "windows", "PSOCK", "FORK")
cl <- makeCluster(detectCores(logical = FALSE), type = gettype)

ed_sub1 <- function() {
  # outbreak length subroutine
  # analyzes the model output from the calling enviroment to determine the maximum
  # outbreak length out of the simulations
  # Args :
  #   None
  # Input :
  #   mod_run : (data.table) the output from mod_sub in the parent environment
  # Returns :
  #   max : (int) maximum outbreak time out of the model data provided, stored
  #         in the parent enviroment

  #---init----------------------------------------------------------
  outbreak_max <- 0
  tf <- mod_run[nrow(mod_run), "time"]
  iter_num <- vector()
  outbreak_time <- vector()
  proc <- vector()
  outbreaks <- vector()
  #---Error check for outbreaks that ran over simulation time------
  setkey(mod_run, time)
  proc <- mod_run[.(tf)][, I]
  if (any(proc != 0)) {
    stop("Outbreak ran over simulation at least once!")
  }
  # re-sort to ensure proper analysis
  setkey(mod_run, iter, time)
  #---Call root finder function on the Infection counts-------------
  mod_run[, roots := Croots(I)]
  #---The meat of 'er------------------------------------------------
  #---Possibly even better implementation----------------------------
  # if only the max is needed, can the outer for loop be removed for
  # optimal speed?
  # could also use %dopar% for parallelized execution
  setkey(mod_run, roots)
  mat <- mod_run[J(TRUE)] # Take all roots
  outbreaks <- foreach(i = 2:nrow(mat),
                       .combine = c) %do% {
                      if ((mat$I[i - 1] > 0)) {
                        (mat$time[i] - mat$time[i - 1])
                      }
                }
  max1 <<- max(outbreaks)
}

ed_sub2 <- function() {
  # outbreak length subroutine
  # analyzes the model output from the calling enviroment to determine the maximum
  # outbreak length out of the simulations
  # Args :
  #   None
  # Input :
  #   mod_run : (data.table) the output from mod_sub in the parent environment
  # Returns :
  #   max : (int) maximum outbreak time out of the model data provided, stored
  #         in the parent enviroment

  #---init----------------------------------------------------------
  outbreak_max <- 0
  tf <- mod_run[nrow(mod_run), "time"]
  iter_num <- vector()
  outbreak_time <- vector()
  proc <- vector()
  outbreaks <- vector()
  #---Parallel init------------------------------------------------
  registerDoParallel(cl)
  #---Error check for outbreaks that ran over simulation time------
  setkey(mod_run, time)
  proc <- mod_run[.(tf)][, I]
  if (any(proc != 0)) {
    stop("Outbreak ran over simulation at least once!")
  }
  # re-sort to ensure proper analysis
  setkey(mod_run, iter, time)
  #---Call root finder function on the Infection counts-------------
  mod_run[, roots := Croots(I)]
  #---The meat of 'er------------------------------------------------
  #---Possibly even better implementation----------------------------
  # if only the max is needed, can the outer for loop be removed for
  # optimal speed?
  # could also use %dopar% for parallelized execution
  setkey(mod_run, roots)
  mat <- mod_run[J(TRUE)] # Take all roots
  outbreaks <- foreach(i = 2:nrow(mat),
                       .combine = c) %dopar% {
                      if ((mat$I[i - 1] > 0)) {
                        (mat$time[i] - mat$time[i - 1])
                      }
                }
  max2 <<- max(outbreaks)
  registerDoSEQ()
}

ed_sub3 <- function() {
  # outbreak length subroutine
  # analyzes the model output from the calling enviroment to determine the maximum
  # outbreak length out of the simulations
  # Args :
  #   None
  # Input :
  #   mod_run : (data.table) the output from mod_sub in the parent environment
  # Returns :
  #   max : (int) maximum outbreak time out of the model data provided, stored
  #         in the parent enviroment

  #---init----------------------------------------------------------
  outbreak_max <- 0
  tf <- mod_run[nrow(mod_run), "time"]
  iter_num <- vector()
  outbreak_time <- vector()
  proc <- vector()
  outbreaks <- vector()
  #---Error check for outbreaks that ran over simulation time------
  setkey(mod_run, time)
  proc <- mod_run[.(tf)][, I]
  if (any(proc != 0)) {
    stop("Outbreak ran over simulation at least once!")
  }
  # re-sort to ensure proper analysis
  setkey(mod_run, iter, time)
  #---Call root finder function on the Infection counts-------------
  mod_run[, roots := Croots(I)]
  #---The meat of 'er------------------------------------------------
  #---Possible new implementation-----------------------------------
  # Using foreach instead of base for loop method
  setkey(mod_run, iter, roots)
  outbreaks <- foreach(i = 1:mod_run$iter[nrow(mod_run)],
                       .combine = c) %do% {
                       outbreak_time <- NULL
                       mat <- NULL
                       mat <- mod_run[.(i, TRUE)]
                       for (j in 2:nrow(mat)) {
                        if (mat$I[j - 1] > 0) {
                            outbreak_time <- c(outbreak_time,
                                               (mat$time[j] - mat$time[j - 1]))
                        }
                       }
                       outbreak_time
                }
  max3 <<- max(outbreaks)
}

ed_sub31 <- function() {
  # outbreak length subroutine
  # analyzes the model output from the calling enviroment to determine the maximum
  # outbreak length out of the simulations
  # Args :
  #   None
  # Input :
  #   mod_run : (data.table) the output from mod_sub in the parent environment
  # Returns :
  #   max : (int) maximum outbreak time out of the model data provided, stored
  #         in the parent enviroment

  #---init----------------------------------------------------------
  outbreak_max <- 0
  tf <- mod_run[nrow(mod_run), "time"]
  iter_num <- vector()
  outbreak_time <- vector()
  proc <- vector()
  outbreaks <- vector()
  #---Error check for outbreaks that ran over simulation time------
  setkey(mod_run, time)
  proc <- mod_run[.(tf)][, I]
  if (any(proc != 0)) {
    stop("Outbreak ran over simulation at least once!")
  }
  # re-sort to ensure proper analysis
  setkey(mod_run, iter, time)
  #---Call root finder function on the Infection counts-------------
  mod_run[, roots := Croots(I)]
  #---The meat of 'er------------------------------------------------
  #---Possible new implementation-----------------------------------
  # Using foreach instead of base for loop method
  setkey(mod_run, iter, roots)
  outbreaks <- foreach(i = 1:mod_run[.N, iter],
                       .combine = c) %do% {
                       outbreak_time <- NULL
                       mat <- NULL
                       mat <- mod_run[.(i, TRUE)]
                       for (j in 2:mat[, .N]) {
                        if (mat[j - 1, I] > 0) {
                            outbreak_time <- c(outbreak_time,
                                               (mat[j, time] - mat[j - 1, time]))
                        }
                       }
                       outbreak_time
                }
  max31 <<- max(outbreaks)
}

ed_sub32 <- function() {
  # outbreak length subroutine
  # analyzes the model output from the calling enviroment to determine the maximum
  # outbreak length out of the simulations
  # Args :
  #   None
  # Input :
  #   mod_run : (data.table) the output from mod_sub in the parent environment
  # Returns :
  #   max : (int) maximum outbreak time out of the model data provided, stored
  #         in the parent enviroment

  #---init----------------------------------------------------------
  outbreak_max <- 0
  tf <- mod_run[nrow(mod_run), "time"]
  iter_num <- vector()
  outbreak_time <- vector()
  proc <- vector()
  outbreaks <- vector()
  #---Register Parallel---------------------------------------------
  registerDoParallel(cl)
  #---Error check for outbreaks that ran over simulation time------
  setkey(mod_run, time)
  proc <- mod_run[.(tf)][, I]
  if (any(proc != 0)) {
    stop("Outbreak ran over simulation at least once!")
  }
  # re-sort to ensure proper analysis
  setkey(mod_run, iter, time)
  #---Call root finder function on the Infection counts-------------
  mod_run[, roots := Croots(I)]
  #---The meat of 'er------------------------------------------------
  #---Possible new implementation-----------------------------------
  # Using foreach instead of base for loop method
  setkey(mod_run, iter, roots)
  outbreaks <- foreach(i = 1:mod_run$iter[nrow(mod_run)],
                       .combine = c,
                       .export = "mod_run",
                       .packages = "data.table") %dopar% {
                       outbreak_time <- NULL
                       mat <- NULL
                       mat <- mod_run[.(i, TRUE)]
                       for (j in 2:nrow(mat)) {
                        if (mat$I[j - 1] > 0) {
                            outbreak_time <- c(outbreak_time,
                                               (mat$time[j] - mat$time[j - 1]))
                        }
                       }
                       outbreak_time
                }
  max32 <<- max(outbreaks)
  registerDoSEQ()
}

ed_sub33 <- function() {
  # outbreak length subroutine
  # analyzes the model output from the calling enviroment to determine the maximum
  # outbreak length out of the simulations
  # Args :
  #   None
  # Input :
  #   mod_run : (data.table) the output from mod_sub in the parent environment
  # Returns :
  #   max : (int) maximum outbreak time out of the model data provided, stored
  #         in the parent enviroment

  #---init----------------------------------------------------------
  outbreak_max <- 0
  tf <- mod_run[nrow(mod_run), "time"]
  iter_num <- vector()
  outbreak_time <- vector()
  proc <- vector()
  outbreaks <- vector()
  #---Register Parallel---------------------------------------------
  registerDoParallel(cl)
  #---Error check for outbreaks that ran over simulation time------
  setkey(mod_run, time)
  proc <- mod_run[.(tf)][, I]
  if (any(proc != 0)) {
    stop("Outbreak ran over simulation at least once!")
  }
  # re-sort to ensure proper analysis
  setkey(mod_run, iter, time)
  #---Call root finder function on the Infection counts-------------
  mod_run[, roots := Croots(I)]
  #---The meat of 'er------------------------------------------------
  #---Possible new implementation-----------------------------------
  # Using foreach instead of base for loop method
  setkey(mod_run, iter, roots)
  outbreaks <- foreach(i = 1:mod_run[.N, iter],
                       .combine = c,
                       .export = "mod_run",
                       .packages = "data.table") %dopar% {
                       outbreak_time <- NULL
                       mat <- NULL
                       mat <- mod_run[.(i, TRUE)]
                       for (j in 2:mat[, .N]) {
                        if (mat[j - 1, I] > 0) {
                            outbreak_time <- c(outbreak_time,
                                               (mat[j, time] - mat[j - 1, time]))
                        }
                       }
                       outbreak_time
                }
  max33 <<- max(outbreaks)
  registerDoSEQ()
}

ed_sub4 <- function() {
  # outbreak length subroutine
  # analyzes the model output from the calling enviroment to determine the maximum
  # outbreak length out of the simulations
  # Args :
  #   None
  # Input :
  #   mod_run : (data.table) the output from mod_sub in the parent environment
  # Returns :
  #   max : (int) maximum outbreak time out of the model data provided, stored
  #         in the parent enviroment

  #---init----------------------------------------------------------
  outbreak_max <- 0
  tf <- mod_run[nrow(mod_run), "time"]
  iter_num <- vector()
  outbreak_time <- vector()
  proc <- vector()
  outbreaks <- vector()
  #---Error check for outbreaks that ran over simulation time------
  setkey(mod_run, time)
  proc <- mod_run[.(tf)][, I]
  if (any(proc != 0)) {
    stop("Outbreak ran over simulation at least once!")
  }
  # re-sort to ensure proper analysis
  setkey(mod_run, iter, time)
  #---Call root finder function on the Infection counts-------------
  mod_run[, roots := Croots(I)]
  #---The meat of 'er------------------------------------------------
  #---Old implementation--------------------------------------------
  for (i in 1:mod_run$iter[nrow(mod_run)]) {
    setkey(mod_run, iter, roots)
    mat <- mod_run[.(i, TRUE)]
    for (j in 2:nrow(mat)) {
      if (mat$I[j - 1] > 0) {
        outbreak_time <- c(outbreak_time, (mat$time[j] - mat$time[j - 1]))
      }
    }
    outbreaks <- c(outbreaks, outbreak_time)
    outbreak_time <- NULL
    mat <- NULL
  }
 #--- Results---------------------------------------------------------
  max4 <<- max(outbreaks)
}

ed_sub11 <- function() {
  # outbreak length subroutine
  # analyzes the model output from the calling enviroment to determine the maximum
  # outbreak length out of the simulations
  # Args :
  #   None
  # Input :
  #   mod_run : (data.table) the output from mod_sub in the parent environment
  # Returns :
  #   max : (int) maximum outbreak time out of the model data provided, stored
  #         in the parent enviroment

  #---init----------------------------------------------------------
  tf <- mod_run[nrow(mod_run), "time"]
  iter_num <- vector()
  proc <- vector()
  outbreaks <- vector()
  #---Error check for outbreaks that ran over simulation time------
  setkey(mod_run, time)
  proc <- mod_run[.(tf)][, I]
  if (any(proc != 0)) {
    stop("Outbreak ran over simulation at least once!")
  }
  # re-sort to ensure proper analysis
  setkey(mod_run, iter, time)
  #---Call root finder function on the Infection counts-------------
  mod_run[, roots := Croots(I)]
  #---The meat of 'er------------------------------------------------
  #---Possibly even better implementation----------------------------
  # if only the max is needed, can the outer for loop be removed for
  # optimal speed?
  # could also use %dopar% for parallelized execution
  setkey(mod_run, roots)
  mat <- mod_run[J(TRUE)] # Take all roots
  for (i in 2:nrow(mat)) {
      if (mat$I[i - 1] > 0) {
        outbreaks <- c(outbreaks, (mat$time[i] - mat$time[i - 1]))
      }
  }
  max11 <<- max(outbreaks)
}



print("Modelling")
if (!"mod_run" %in% ls()) {
  mod_run <- batch_run_mc(envir = Kyrgyz, insertion = 10, i_number = 10, occ = 1,
                        length = 1000, grp = "y", batch = 10000)
  mod_run <- as.data.table(mod_run)
}
print("Model finished, Analyzing")
ed_sub3()
print("Function 1, done")
ed_sub31()
print("function 2, done")
ed_sub32()
print("function 3, done")
ed_sub33()
print("function 4, done")
ed_sub4()
print("function 5, done")
ed_sub11()
print("function 6, done")
ed_sub1()
ed_sub2()
print(all.equal(max3, max31))
print(all.equal(max31, max32))
print(all.equal(max32, max33))
print(all.equal(max33, max4))
print(all.equal(max11, max4))
print(all.equal(max4, max1))
print(all.equal(max1, max2))
stopifnot(all.equal(max3, max31),
          all.equal(max31, max32),
          all.equal(max32, max33),
          all.equal(max33, max4),
          all.equal(max11, max4))
print("benchmarking")
bench <- microbenchmark("new foreach ST" = ed_sub1(),
                        "new foreach MT" = ed_sub2(),
                        "old foreach ST" = ed_sub3(),
                        "old foreach ST mod 1" = ed_sub31(),
                        "old foreach MT" = ed_sub32(),
                        "old foreach MT mod 1" = ed_sub33(),
                        "old for loop" = ed_sub4(),
                        "new for loop" = ed_sub11(),
                        times = 5)
