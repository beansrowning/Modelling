# Singlecore Batch Run function
# Bringing it back up to spec
# 03 Jul 2017

# Depends
require("adaptivetau")
# mul_ins() defined in multicore.R

batch_run_sc <- function(envir, ...) {
  # Runs a specified number of adaptivetau simulations
  # - Is not optimized for multithreaded systems
  # - Performance loss is minimal for small batch sizes / systems
  #   with few cores or limited resources (my Pentium G3258)
  # - For large batches or in systems with higher core count,
  #   the multicore batch run is preferrable.
  #
  # Args:
  #   envir : Environment where the model parameters can be found
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

  stopifnot(!is.null(call$grp))

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
  grp <- call$grp
  init <- get("init.values", envir = .envir)
  t <- get("transitions", envir = .envir)
  rf <- get("RateF", envir = .envir)
  p <- get("parameters", envir = .envir)
  if (!is.null(call$length)) {
    assign("tf", call$length)
  } else {
    assign("tf", 365)
  }

  fun_list <- list(init, t, rf, p, tf)
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
