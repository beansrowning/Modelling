# Multicore Batch Run function
# Attempting to handle arguments a bit better...
# 11 Jun 2017

# Depends
require("adaptivetau")
require("parallel")
require("doParallel")
require("foreach")

batch_run_mc <- function(...) {
  # Runs a specified number of adaptivetau simulations utilizing multithreaded
  # processing by assigning CPU cores to a parallel socket cluster.
  # It will not, therefore, run in a virtual machine. UNIX untested.
  # Performance increase over base function is close to sqrt(n)
  #
  # Args:
  #   batch : Number of desired ssa runs
  #   init.values : Initial model values
  #   transitions : Model transtions
  #   rfunc : Model rate function
  #   parameters : Model parameters / variable definitions
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
    assign("batch", call$batch, envir = .GlobalEnv)
  } else {
    assign("batch", 10000, envir = .GlobalEnv);
    warning("No batch count given, 10000 assumed")
  }
  if (!is.null(call$insertion) && call$insertion >= 0) {
    assign("insertion", call$insertion, envir = .GlobalEnv)
  } else {
    assign("insertion", 0, envir = .GlobalEnv)
    warning("No start time given, 0 assumed")
  }
  if (!is.null(call$i_number)) {
    assign("i_number", call$i_number, envir = .GlobalEnv)
  } else {
    assign("i_number", 0, envir = .GlobalEnv)
    warning("No infected count given, 0 assumed")
  }
  if (!is.null(call$occ)) {
    assign("occ", call$occ, envir = .GlobalEnv)
  } else {
    assign("occ", 2, envir = .GlobalEnv)
    warning("No insertion count given, 2 assumed")
  }
  if(!is.null(call$grp)) {
    assign("grp", call$grp, envir = .GlobalEnv)
  } else {
    stop("No infection group specified!")
  }

  if (!is.null(call$init.values)) {
    assign("init", call$init.values)
  } else {
    assign("init", init.values)
  }
  if (!is.null(call$transitions)) {
    assign("t", call$transitions)
  } else {
    assign("t", transitions)
  }
  if (!is.null(call$rfunc)) {
    assign("rf", call$rfunc)
  } else {
    assign("rf", RateF)
  }
  if (!is.null(call$parameters)) {
    assign("p", call$parameters)
  } else {
    assign("p", parameters)
  }
  if (!is.null(call$length)) {
    assign("tf", call$length)
  } else {
    assign("tf", 365)
  }

  fun_list <<- list(init, t, rf, p, tf)
  #---End Match Call / Parameter assignment---------------

  #---Define Parallel Socket CLuster----------------------
  core <- detectCores(logical = FALSE)
  cl <- makePSOCKcluster(core)
  registerDoParallel(cl)

  clusterExport(cl, c("mul_ins", "batch", "fun_list",
                      "grp", "insertion", "i_number", "occ"))

 #---Define per-thread simulation routine-----------------
  par_run <- function() {
    results <- mul_ins()
    results <- cbind(results, I = rowSums(results[, c("I1", "I2")]))
    results <- cbind(results, iter = i)
    results <- results[, c("time", "I", "iter"), drop = FALSE]
    return(results)
  }

  #---Initiate Parallel Execution------------------------
  sim_dat <- data.frame(time = NULL, I = NULL, iter = NULL)
  sim_dat <- foreach(i = 1:batch, .packages = "adaptivetau",
                      .combine = rbind) %dopar% {
      par_run()

    }

  #---Sanitize and return run data-----------------------
  sim_dat <- as.data.frame(sim_dat)
  return(sim_dat)

  #---Stop PSOCK cluster---------------------------------
  stopCluster(cl)
  registerDoSEQ()

  #---Garbage Collection---------------------------------
  rm(cl)
  rm(batch, envir = .GlobalEnv)
  rm(fun_list, envir = .GlobalEnv)
  rm(grp, envir = .GlobalEnv)
  rm(insertion, envir = .GlobalEnv)
  rm(i_number, envir = .GlobalEnv)
  rm(occ, envir = .GlobalEnv)
}

mul_ins <- function() {
  # - batch_run_mc subroutine interfacing with ssa.apaptivetau
  # - Allows for single or multiple insertions of infected persons
  #   at any time point.
  # - If multiple insertions are called, it will space them out equally
  #   over the specified run length. No current way of having other spacing.
  # - Appends ssa run matrices together, and adjusts for time offset.
  #
  # Args:
  #   None, inherits from batch_run_mc
  # Returns:
  #   results : matrix of conglomerated run data

  # __Inherits__
  init <- fun_list[[1]]
  t <- fun_list[[2]]
  RF <- fun_list[[3]]
  P <- fun_list[[4]]
  ins <- occ
  i_num <- i_number
  i_start <- insertion
  age <- grp
  tf <- fun_list[[5]]
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
