# Singlecore Batch Run function
# Bringing it back up to spec
# 03 Jul 2017

# Depends
require("adaptivetau")

batch_run_sc <- function(...) {
  # Runs a specified number of adaptivetau simulations
  # - Is not optimized for multithreaded systems
  # - Performance loss is minimal for small batch sizes / systems
  #   with few cores or limited resources (my Pentium G3258)
  # - For large batches or in systems with higher core count,
  #   the multicore batch run is preferrable.
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

  #---Run Simulations-------------------------------------
  sim_dat <- data.frame(time = NULL, I = NULL, iter = NULL)
  for (i in 1:batch) {
      results <- mul_ins()
      results <- cbind(results, I = rowSums(results[, c("I1", "I2")]), iter = i)
      sim_dat <- rbind(sim_dat, results[, c("time", "I", "iter"), drop = FALSE])
    }

  #---Sanitize and return run data-----------------------
  sim_dat <- as.data.frame(sim_dat)
  return(sim_dat)
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
