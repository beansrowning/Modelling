# Singlecore batchplot for Benchmark 
# 28/05/2017


library(ggplot2)
library(plyr)
library(parallel)
library(doParallel)
library(foreach)



batch_plot <- function(FUN = "mul_ins", batch = 100, 
                       fun_list = list(init.values, transitions, RateF, parameters, 365), 
                       grp = NULL, insertion = 0, i_number = NULL, occ = 2) {
  # Runs ssa.adaptivetau specified number of times and plots results with ggplot2
  # -Added Functionality to insert infected individuals at a given time
  # -"mul_ins" adds functionality to insert individuals at spaced intervals
  #  after the first insertion.  
  # Automatically plots data as geom_points on a cumulative graph 
  # Moving average functionality to be added 
  # Args: 
  #   FUN : Name of insertion function to utilize
  #            mul_ins: newer, inserts one infected person then adds more routinely 
  #            ins_1: older, depreciated single point insertion run 
  #   batch : number of desired ssa runs 
  #   fun_list : list of parameters read into ssa.adaptivetau through FUN 
  #   grp : "y" or "a" to indicate which age group to insert into
  #   insertion : Time point of first insertion (in days) 
  #   i_number : Number of infected persons to insert each time 
  #   occ : How many total insertion events should occur 
  # Returns:
  #   run : left over matrix of last run data in batch for silly reasons
  #   plot_dat : data frame of time and infected counts for each run
  #   graph : ggplot2 graph data for additional editing or saving 
  
  if (FUN == "ins_1") {
    #throw some errors
    if (is.null(grp) == TRUE) {
      stop("No group specified!")
    }
    if (is.null(insertion) == TRUE) {
      stop("No insertion time selected!")
    }
    if (is.null(i_number) == TRUE) {
      stop("No number of infected specified!")
    }
    
    ins_1 <- function(i = fun_list[[1]], t = fun_list[[2]], RF = fun_list[[3]],
                      P = fun_list[[4]], t_int = insertion, i_num = i_number,age = grp, tf = fun_list[[5]]){
      t_2 <- tf - t_int
      inf_grp <- ifelse(age == "a", "I2", "I1")
      run_1 <- ssa.adaptivetau(i, t, RF, P, t_int)
      run_1[nrow(run_1), inf_grp] = run_1[nrow(run_1), inf_grp] + i_num
      init.2 <- c(c(run_1[nrow(run_1), "S1"], run_1[nrow(run_1), "S2"]),
                  c(run_1[nrow(run_1), "E1"], run_1[nrow(run_1), "E2"]),
                  c(run_1[nrow(run_1), "I1"], run_1[nrow(run_1), "I2"]),
                  c(run_1[nrow(run_1), "R1"], run_1[nrow(run_1), "R2"]),
                  c(run_1[nrow(run_1), "D"]))
      run_2 <- ssa.adaptivetau(init.2, t, RF, P, t_2)
      run_2 <- cbind(apply(run_2[,"time", drop=FALSE], 2, function(x) x+run_1[nrow(run_1),"time"]),
                     run_2[, -1])
      run <- rbind(run_1,run_2[-1, ])
      run <- cbind(run, I = rowSums(run[, c("I1", "I2")]))
      run <<- run[, c("time", "I"), drop = FALSE]
    }
    
    #run a whole bunch of times and store into a df
    plot_dat <- data.frame(time = 0, I = 0, iter = 0)
    for(num in 1:batch){
      ins_1()
      run <- cbind(run, iter = num)
      plot_dat <- rbind(plot_dat, run)
    }
    plot_dat <- plot_dat[-1, ] #drop starting value (how do you do this better?)
    #trim and store into a new dataframe
    #plot_dat$t_2 <- round(plot_dat$time, 0)
    #plot_dat_2 <- unique(plot_dat)
    
    #Create summary measures from the runs
    #sum_dat <- ddply(plot_dat_2,.variables = "t_2",
    #summarize,
    #ave = mean(I),
    #lb = quantile(I,0.25),
    #ub = quantile(I,0.75))
    
    #plotting
    graph = ggplot(plot_dat)
    graph = graph + geom_point(aes(x=time, y=I), alpha=0.1, size=1)
    #graph = graph + geom_ribbon(data = sum_dat,
    #aes(x=t_2,ymin=lb,ymax=ub),
    #alpha = 0.25)
    #graph = graph + geom_line(data = sum_dat,
    #aes(x=t_2,y=ave),
    #size=0.5)
    graph = graph + labs(title= paste(batch,"SIR Iterations"),
                         x = "Time (days)",
                         y = "Infected (count)")
    graph = graph + theme_bw()
    plot(graph)
    assign("graph", graph, envir = .GlobalEnv) #for editing or saving
  }
  if (FUN == "mul_ins") {
    #throw some errors
    if (is.null(grp) == TRUE) {
      stop("No infection group specified!")
    }
    if (is.null(occ) == TRUE) {
      stop("Number of insertions not specified!")
    }
    if (is.null(insertion) == TRUE || insertion < 0) {
      stop("Something is wrong with your start time,partner.")
    }
    
    mul_ins <- function(init = fun_list[[1]], t = fun_list[[2]], RF = fun_list[[3]],
                        P = fun_list[[4]], ins = occ, i_num = i_number,
                        i_start = insertion, age = grp, tf = fun_list[[5]]) {
      inf_grp <- ifelse(age == "a", "I2", "I1")
      
      #run given time delay
      if (i_start > 0) {
        results <- ssa.adaptivetau(init, t, RF, P, i_start)
        throw_2 <<- "starttime > 0"
        for (i in 1:occ) {
          results[nrow(results), inf_grp] = results[nrow(results), inf_grp] + i_num #add infected
          # accounting for time 0 starts
          init_new <- c(c(results[nrow(results), "S1"], results[nrow(results), "S2"]),
                        c(results[nrow(results), "E1"], results[nrow(results), "E2"]),
                        c(results[nrow(results), "I1"], results[nrow(results), "I2"]),
                        c(results[nrow(results), "R1"], results[nrow(results), "R2"]),
                        c(results[nrow(results), "D"]))
          t_new <- ((tf-i_start)*(i/occ)-(tf-i_start)*((i-1)/occ))
          # more accounting for time 0 starts
          run <- ssa.adaptivetau(init_new, t, RF, P, t_new)
          run <- cbind(apply(run[, "time", drop=FALSE], 2, function(x) x+results[nrow(results), "time"]),
                       run[, -1]) #offset time by the final time of the past run
          results <- rbind(results, run[-1, ]) #drop the first row
        }
      }
      #run if no delay
      if (i_start == 0) {
        t_first <- tf*(1/occ)
        init_new <- init
        init_new[inf_grp] <- init[inf_grp] + i_num
        results <<- ssa.adaptivetau(init_new, t, RF, P, t_first)
        throw_1 <<- "starttime == 0"
        #insertion loops
        for (i in 1:(occ-1)) {
          results[nrow(results), inf_grp] <- results[nrow(results), inf_grp] + i_num #add infected
          #accounting for time 0 starts
          init_new <- c(c(results[nrow(results), "S1"], results[nrow(results), "S2"]),
                        c(results[nrow(results), "E1"], results[nrow(results), "E2"]),
                        c(results[nrow(results), "I1"], results[nrow(results), "I2"]),
                        c(results[nrow(results), "R1"], results[nrow(results), "R2"]),
                        c(results[nrow(results), "D"]))
          t_new <- ((tf-i_start)*(i/occ)-(tf-i_start)*((i-1)/occ))
          #more accounting for time 0 starts
          run <- ssa.adaptivetau(init_new,t,RF,P,t_new)
          run <- cbind(apply(run[, "time", drop=FALSE], 2, function(x) x+results[nrow(results),"time"]),
                       run[, -1]) #offset time by the final time of the past run
          results <- rbind(results,run[-1, ]) #drop the first row
        }
      }
      #store results of run globally
      assign("results", results, envir = .GlobalEnv)
    }
    
    #batch runs
    plot_dat <- data.frame(time = NULL, I = NULL, iter = NULL)
    for (i in 1:batch) {
      mul_ins()
      results <- cbind(results, I = rowSums(results[, c("I1", "I2")]))
      results <- cbind(results, iter = i)
      plot_dat <- rbind(plot_dat, results[ ,c("time", "I", "iter"), drop = FALSE])
    }
    
    #store plot data globally
    assign("plot_dat1", plot_dat, envir = .GlobalEnv)
    plot_dat$t_2 <- round(plot_dat$time, 0)
    #plot_dat_2 <- unique(plot_dat)
  }
}