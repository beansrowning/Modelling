# Multicore work
# 28/5/2017

library(ggplot2)
library(plyr)
library(parallel)
library(doParallel)
library(foreach)

batch_plot_mc <- function(batch = 1000, fun_list =
                            list(init.values, transitions, RateF, parameters,365), grp = NULL, insertion = 0, i_number = NULL, occ = 2){
  
  # Set up PSOCK Cluster 
  
  core <- detectCores(logical=FALSE)
  cl <- makePSOCKcluster(core)
  registerDoParallel(cl)
  
  if(is.null(grp) == TRUE){
    stop("No infection group specified!")
  }
  if(is.null(occ) == TRUE){
    stop("Number of insertions not specified!")
  }
  if(is.null(insertion) == TRUE || insertion < 0){
    stop("Something is wrong with your start time, partner.")
  }
  
  mul_ins <- function(init = fun_list[[1]], t = fun_list[[2]], RF = fun_list[[3]],
                      P = fun_list[[4]], ins = occ, i_num = i_number,i_start = insertion,age = grp, tf = fun_list[[5]]
  ){
    inf_grp <- ifelse(age == "a","I2","I1")
    
    #run given time delay
    if(i_start > 0){
      results <- ssa.adaptivetau(init,t,RF,P,i_start)
      throw_2 <<- "starttime > 0"
      for(i in 1:occ){
        results[nrow(results),inf_grp] = results[nrow(results),inf_grp] + i_num #add infected
        #accounting for time 0 starts
        init_new <- c(c(results[nrow(results),"S1"],results[nrow(results),"S2"]),
                      c(results[nrow(results),"E1"],results[nrow(results),"E2"]),
                      c(results[nrow(results),"I1"],results[nrow(results),"I2"]),
                      c(results[nrow(results),"R1"],results[nrow(results),"R2"]),
                      c(results[nrow(results),"D"]))
        t_new = ((tf-i_start)*(i/occ)-(tf-i_start)*((i-1)/occ))
        #more accounting for time 0 starts
        run = ssa.adaptivetau(init_new,t,RF,P,t_new)
        run = cbind(apply(run[,"time", drop=FALSE],2,function(x) x+results[nrow(results),"time"]),
                    run[,-1]) #offset time by the final time of the past run
        results <- rbind(results,run[-1,]) #drop the first row
      }
    }
    #run if no delay
    if(i_start == 0){
      t_first = tf*(1/occ)
      init_new <- init
      init_new[inf_grp] = init[inf_grp] + i_num
      results <<- ssa.adaptivetau(init_new,t,RF,P,t_first)
      throw_1 <<- "starttime == 0"
      #insertion loops
      for(i in 1:(occ-1)){
        results[nrow(results),inf_grp] = results[nrow(results),inf_grp] + i_num #add infected
        #accounting for time 0 starts
        init_new <- c(c(results[nrow(results),"S1"],results[nrow(results),"S2"]),
                      c(results[nrow(results),"E1"],results[nrow(results),"E2"]),
                      c(results[nrow(results),"I1"],results[nrow(results),"I2"]),
                      c(results[nrow(results),"R1"],results[nrow(results),"R2"]),
                      c(results[nrow(results),"D"]))
        t_new = ((tf-i_start)*(i/occ)-(tf-i_start)*((i-1)/occ))
        #more accounting for time 0 starts
        run = ssa.adaptivetau(init_new,t,RF,P,t_new)
        run = cbind(apply(run[,"time", drop=FALSE],2,function(x) x+results[nrow(results),"time"]),
                    run[,-1]) #offset time by the final time of the past run
        results <- rbind(results,run[-1,]) #drop the first row
      }
    }
    #store results of run globally
    assign("results",results,envir=.GlobalEnv)
  }
  par_run <- function() {
    results <- mul_ins()
    results <- cbind(results, I = rowSums(results[,c("I1","I2")]))
    results <- cbind(results,iter=i)
    results <- results[, c("time", "I", "iter"), drop = FALSE]
    return(results)
  }
  #batch runs
  plot_dat <- data.frame(time = NULL,I = NULL,iter = NULL)
  plot_dat <- foreach(i = 1:batch, .packages='adaptivetau', .combine=rbind) %dopar% {
    par_run()
    # plot_dat <- rbind(plot_dat,results[,c("time","I","iter"),drop=FALSE])
  }
  
  #store plot data globally
  plot_dat <- as.data.frame(plot_dat)
  assign("plot_dat2",plot_dat,envir=.GlobalEnv)
  plot_dat$t_2 <- round(plot_dat$time,0)
  
  stopCluster(cl) # Stop PSOCK cluster
}
#   #plot_dat_2 <- unique(plot_dat)
#   #Create summary measures from the runs
#   sum_dat <- ddply(plot_dat,.variables = "t_2",
#                    summarize,
#                    ave = mean(I),
#                    lb = quantile(I,0.25),
#                    ub = quantile(I,0.75))
# 
#   #graphing
#   graph = ggplot(plot_dat)
#   graph = graph + geom_point(aes(x=time, y=I), alpha=0.1, size=1)
#   #graph = graph + geom_ribbon(data = sum_dat,
#   #aes(x=t_2,ymin=lb,ymax=ub),
#   #alpha = 0.25)
#   #graph = graph + geom_line(data = sum_dat,
#   #aes(x=t_2,y=ave),
#   #size=0.5)
#   graph = graph + labs(title= paste(batch,"SIR Iterations"),
#                        x = "Time (days)",
#                        y = "Infected (count)")
#   graph = graph + theme_bw()
#   plot(graph)
#   assign("graph",graph,envir = .GlobalEnv) #for editing or saving
# }