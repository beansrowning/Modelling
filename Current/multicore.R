# Multicore batchplot for Benchmark
# 01/06/2017

library(adaptivetau)
library(parallel)
library(doParallel)
library(foreach)

batch_plot_mc <- function(batch = 10000, fun_list = list(init.values, transitions,
  RateF, parameters, 365), grp = NULL, insertion = 0, i_number = NULL, occ = 2) {
  # Runs a specified number of adaptivetau simulations utilizing multithreaded
  # processing by assigning CPU cores to a parallel socket cluster.
  # It will not, therefore, run in a virtual machine. UNIX untested.
  # Performance increase over base function is close to sqrt(t) on 4 cores
  #
  # Args:
  #   batch : number of desired ssa runs
  #   fun_list : list of parameters read into ssa.adaptivetau
  #   FUN grp : 'y' or 'a' to indicate which age group to insert into
  #   insertion : Time point of first insertion (in days)
  #   i_number : Number of infected persons to insert each time
  #   occ : How manytotal insertion events should occur
  # Returns:
  #   run : left over matrix of last run data in batch for silly reasons
  #   plot_dat : data frame of time and infected counts for each run
  #   graph : ggplot2 graph data for additional editing or saving

  # Set up PSOCK Cluster
  core <- detectCores(logical = FALSE)
  cl <- makePSOCKcluster(core)

  registerDoParallel(cl)

  if (is.null(grp) == TRUE) {
    stop("No infection group specified!")
  }
  if (is.null(occ) == TRUE) {
    stop("Number of insertions not specified!")
  }
  if (is.null(insertion) == TRUE || insertion < 0) {
    stop("Something is wrong with your start time, partner.")
  }

  mul_ins <- function(init = fun_list[[1]], t = fun_list[[2]],
    RF = fun_list[[3]], P = fun_list[[4]], ins = occ, i_num = i_number,
    i_start = insertion, age = grp, tf = fun_list[[5]]) {

    inf_grp <- ifelse(age == "a", "I2", "I1")

    # run given time delay
    if (i_start > 0) {

      results <- ssa.adaptivetau(init, t, RF, P, i_start)

      for (i in 1:occ) {

        results[nrow(results), inf_grp] <- results[nrow(results), inf_grp] +
          i_num  #add infected

        # accounting for time 0 starts

        init_new <- results[nrow(results), ]
        init_new <- init_new[c("S1","S2","E1","E2","I1","I2","R1","R2","D"),
                             drop = FALSE]

        t_new <- ((tf - i_start) * (i / occ) - (tf - i_start) * ((i - 1) / occ))

        # more accounting for time 0 starts
        run <- ssa.adaptivetau(init_new, t, RF, P, t_new)

        #offset time by the final time of the past run
        run <- cbind(apply(run[, "time", drop = FALSE], 2, function(x) x +
          results[nrow(results), "time"]), run[, -1])

        results <- rbind(results, run[-1, ])  #drop the first row
      }
    }

    # run if no delay
    if (i_start == 0) {
      t_first <- tf * (1 / occ)
      init_new <- init
      init_new[inf_grp] <- init[inf_grp] + i_num
      results <<- ssa.adaptivetau(init_new, t, RF, P, t_first)
      # insertion loops
      for (i in 1:(occ - 1)) {

        results[nrow(results), inf_grp] <- results[nrow(results), inf_grp] +
          i_num  #add infected
        # accounting for time 0 starts

        init_new <- results[nrow(results), ]
        init_new <- init_new[c("S1","S2","E1","E2","I1","I2","R1","R2","D"),
                             drop = FALSE]

        t_new <- ((tf - i_start) * (i / occ) - (tf - i_start) * ((i - 1) / occ))

        # more accounting for time 0 starts
        run <- ssa.adaptivetau(init_new, t, RF, P, t_new)

        # offset time by the final time of the past run
        run <- cbind(apply(run[, "time", drop = FALSE], 2, function(x) x +
          results[nrow(results), "time"]), run[, -1])
        results <- rbind(results, run[-1, ])  #drop the first row
      }
    }
    return(results)
  }

  par_run <- function() {
    results <- mul_ins()
    results <- cbind(results, I = rowSums(results[, c("I1", "I2")]))
    results <- cbind(results, iter = i)
    results <- results[, c("time", "I", "iter"), drop = FALSE]
    return(results)
  }
  # batch runs
  plot_dat <- data.frame(time = NULL, I = NULL, iter = NULL)
  plot_dat <- foreach(i = 1:batch, .packages = "adaptivetau", .combine = rbind)
               %dopar% {
      par_run()

    }

  # store plot data globally
  plot_dat <- as.data.frame(plot_dat)
  assign("plot_dat2", plot_dat, envir = .GlobalEnv)
  plot_dat$t_2 <- round(plot_dat$time, 0)

  stopCluster(cl)  # Stop PSOCK cluster
  remove(cl)
  registerDoSEQ()
}