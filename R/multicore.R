# Multicore Batch Run function
# Working with environments
# 11 Jun 2017

# Depends
require("adaptivetau")
require("parallel")
require("doParallel")

batch_run_mc <- function(envir, ...) {
  # Runs a specified number of adaptivetau simulations utilizing multithreaded
  # processing by assigning CPU cores to a processing cluster.
  # The type of cluster is determined by OS :
  # Windows uses a Parallel Socket Cluster (PSOCK)
  # Mac/UNIX use a FORK cluster.
  # Virtualized systems seem to have trouble doing either and are not advisable
  # Performance increase over base function is close to sqrt(n), however
  # this depends heavily on the size of the batch and the number of cores.
  #
  # Args:
  #   envir : Environment where the model parameters can be found
  #   batch : Number of desired ssa runs
  #   length : Approximate model run length
  #   grp : 'y' or 'a' to indicate which age strata to insert into
  #   insertion : Time point of first insertion (in days)
  #   i_number : Number of infected persons to insert each time
  #   occ : How many total insertion events should occur
  # Returns:
  #   sim_dat : data frame of time and infected counts for each run

  #---Begin Match Call / Parameter Assignment---------------
  call <- match.call(expand.dots = TRUE)

  if (!is.null(call$batch)) {
    assign("batch", call$batch)
  } else {
    assign("batch", 10000);
    warning("No batch count given, 10000 assumed")
  }
  if (!is.null(call$insertion) && call$insertion >= 0) {
    assign("insertion", call$insertion)
  } else {
    assign("insertion", 0)
    warning("No start time given, 0 assumed")
  }
  if (!is.null(call$i_number)) {
    assign("i_number", call$i_number)
  } else {
    assign("i_number", 0)
    warning("No infected count given, 0 assumed")
  }
  if (!is.null(call$occ)) {
    assign("occ", call$occ)
  } else {
    assign("occ", 2)
    warning("No insertion count given, 2 assumed")
  }
  if (!is.null(call$envir))  {
    .envir <- envir
    .vars <- c("init.values", "transitions", "parameters", "RateF")
    stopifnot(any(.vars %in% ls(.envir)), is.environment(.envir))
  } else {
    .envir <- .GlobalEnv
  }
  if (!is.null(call$length)) {
    assign("tf", call$length)
  } else {
    assign("tf", 365)
  }
  stopifnot(insertion < tf, !is.null(call$grp))
  grp <- call$grp
  init <- get("init.values", envir = .envir)
  t <- get("transitions", envir = .envir)
  rf <- get("RateF", envir = .envir)
  p <- get("parameters", envir = .envir)


  fun_list <- list(init, t, rf, p, tf)
  #---End Match Call / Parameter assignment---------------

  #---Define Cluster--------------------------------------
  core <- detectCores(logical = FALSE)
  gettype <- ifelse(.Platform$OS.type == "windows", "PSOCK", "FORK")
  cl <- makeCluster(core, type = gettype)
  registerDoParallel(cl)

  clusterExport(cl, c("mul_ins", "batch", "fun_list",
                      "grp", "insertion", "i_number", "occ"),
                envir = environment())

 #---Define per-thread simulation routine-----------------
  par_run <- function() {
    results <- mul_ins()
    results <- cbind(results, I = rowSums(results[, c("I1", "I2")]), iter = i)
    results <- results[, c("time", "I", "iter"), drop = FALSE]
    return(results)
  }

  #---Initiate Parallel Execution------------------------
  sim_dat <- data.frame(time = NULL, I = NULL, iter = NULL)
  sim_dat <- foreach(i = 1:batch, .packages = "adaptivetau",
                      .combine = rbind) %dopar% {
      par_run()

    }
  #---Stop cluster---------------------------------
  stopCluster(cl)
  closeAllConnections()
  #---Garbage Collection---------------------------------
  rm("cl", "batch", "fun_list", "grp", "insertion", "i_number", "occ")

  #---Return run data-----------------------
  return(as.data.frame(sim_dat))
}

mul_ins <- function() {
  # - batch_run_mc subroutine interfacing with ssa.apaptivetau
  # - Allows for single or multiple insertions of infected persons
  #   at any time point.
  # - If multiple insertions are called, it will space them out equally
  #   over the specified run length. No current way of having other spacing.
  # - Appends ssa run matrices together, and adjusts for time offset.
  # - Now with improved environment handling
  #
  # Args:
  #   None, inherits from batch_run_mc
  # Returns:
  #   results : matrix of conglomerated run data

  # __Inherits__
  ins <- get("occ", envir = parent.frame())
  i_num <- get("i_number", envir = parent.frame())
  i_start <- get("insertion", envir = parent.frame())
  age <- get("grp", envir = parent.frame())
  .fun_list <- get("fun_list", envir = parent.frame())
  init <- .fun_list[[1]]
  t <- .fun_list[[2]]
  RF <- .fun_list[[3]]
  P <- .fun_list[[4]]
  tf <- .fun_list[[5]]

  inf_grp <- ifelse(age == "a", "I2", "I1")

  #---Check for first insertion time------------------------
  if (i_start == 0) {
    t_start <- tf * (1 / ins)
    init[inf_grp] <- init[inf_grp] + i_num
    ins <- ins - 1
  } else {
    t_start <- i_start
  }

  #---First run----------------------------------------------
  results <- ssa.adaptivetau(init, t, RF, P, t_start)

  #---Subsequent runs----------------------------------------
  for (i in 1:ins) {
    # Add infected
    results[nrow(results), inf_grp] <- results[nrow(results), inf_grp] +
      i_num

    # Set results from first run as init for next run

    init_new <- results[nrow(results), ]
    init_new <- init_new[c("S1", "S2", "E1", "E2", "I1",
                           "I2", "R1", "R2", "D"), drop = FALSE]

    # Calculate new run length
    t_new <- (tf - i_start) * (i / ins) - (tf - i_start) * ( (i - 1) / ins)

    # Run with new inits
    run <- ssa.adaptivetau(init_new, t, RF, P, t_new)

    # Offset time by the final time of the past run
    run <- cbind(apply(run[, "time", drop = FALSE], 2, function(x) x +
                       results[nrow(results), "time"]), run[, -1])

    # Drop duplicated first row
    results <- rbind(results, run[-1, ])
  }
  return(results)
}
