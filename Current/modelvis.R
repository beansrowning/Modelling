#Model visualization functions
#working 3/13
#imports: googleVis,ggplot2,plyr(not really)

if(!require(googleVis)){
  install.packages("googleVis")
 }
if(!require(ggplot2)){
  install.packages("ggplot2")
 }
if(!require(plyr)){
  install.packages("plyr")
}
suppressPackageStartupMessages(library(googleVis))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(plyr))

#SIRplot
#generic post-run plot yielding interactive googleVis output.
#mat = resulting matrix from ssa run
#vars = what vars to plot (column names from matrix)
#y.axis = "lin" for linear, "log" for logarithmic
#x.range = given time range to view
#parameters = optional table with the parameters of the run beside the graph
SIRplot <- function(mat,vars = c("time", "S11", "I11", "R11"),y.axis = "lin", x.range = c(0,(mat[nrow(mat),"time"])),parameters = NULL){
    #determine row numbers for given x.range
    rag <- which(mat[,"time"] >= x.range[1] & mat[,"time"] <= x.range[2])
    x.range <- c(rag[1],rag[length(rag)])

    #subset plot variables from matrix and store in a df
    gvplot_dat <- as.data.frame(mat[x.range[1]:x.range[2],vars])

    #begin plotting
    if (y.axis == "lin") {
      if (is.null(parameters) == TRUE) { #no parameter table
        g_graph <- gvisLineChart(gvplot_dat,
                 options = list(
                   title = "Model Output",
                   hAxis="{title:'Time (days)', titleTextStyle:{color:'black'}}",
                   vAxis="{title:'Count', titleTextStyle:{color:'black'}, scaleType: 'y.axis'}",
                   width = 668,
                   height = 400
                 ))
        plot(g_graph)
      } else { #create parameter table and merge
        g_graph <- gvisLineChart(gvplot_dat,
                 options = list(
                   title = "Model Output",
                   hAxis="{title:'Time (days)', titleTextStyle:{color:'black'}}",
                   vAxis="{title:'Count', titleTextStyle:{color:'black'}, scaleType: 'y.axis'}",
                   width = 668,
                   height = 400
                 ))
        para_tab <- data.frame(Parameter = names(parameters), Value = parameters, stringsAsFactors = FALSE)
        g_table <- gvisTable(para_tab,options = list(
                   width = 200,
                   height = 400
                ))
        g_merge <- gvisMerge(g_graph,g_table, horizontal = TRUE,
                   tableOptions="bgcolor=\"#607D8B\" cellspacing=10" #coral blue number 5
                      )
        plot(g_merge)
    }
    }
    if (y.axis == "log") {
      if (is.null(parameters) == TRUE) { #no parameter table
        g_graph <- gvisLineChart(gvplot_dat,
                                 options = list(
                                   title = "Model Output",
                                   hAxis="{title:'Time (days)', titleTextStyle:{color:'black'}}",
                                   vAxis="{title:'Count', titleTextStyle:{color:'black'}, scaleType: 'log'}",
                                   width = 668,
                                   height = 400
                                 ))
        plot(g_graph)
      } else { #create parameter table and merge
        g_graph <- gvisLineChart(gvplot_dat,
                                 options = list(
                                   title = "Model Output",
                                   hAxis="{title:'Time (days)', titleTextStyle:{color:'black'}}",
                                   vAxis="{title:'Count', titleTextStyle:{color:'black'}, scaleType: 'log'}",
                                   width = 668,
                                   height = 400
                                 ))
        para_tab <- data.frame(Parameter = names(parameters), Value = parameters, stringsAsFactors = FALSE)
        g_table <- gvisTable(para_tab,options = list(
          width = 200,
          height = 400
        ))
        g_merge <- gvisMerge(g_graph,g_table, horizontal = TRUE,
                             tableOptions="bgcolor=\"#607D8B\" cellspacing=10" #coral blue number 5
        )
        plot(g_merge)
      }
    }
}

#Batch Plot
#runs specified number of batches and gives ggplot2 output
#default function: SIR_run (insertion of infected individuals at a given time-point)
batch_plot <- function(FUN = "1_ins", batch = 100, fun_list = list(init.values, transitions, RateF, parameters,365), grp = NULL, insertion = NULL, i_number = NULL, occ = 1){
  if(FUN == "1_ins"){
    #throw some errors
    if(is.null(grp) == TRUE){
      stop("No group specified!")
    }
    if(is.null(insertion) == TRUE){
      stop("No insertion time selected!")
    }
    if(is.null(i_number) == TRUE){
      stop("No number of infected specified!")
    }

    #pretty much the same wrapper I wrote in the old one
    1_ins <- function(i = fun_list[[1]], t = fun_list[[2]], RF = fun_list[[3]], 
    P = fun_list[[4]], t_int = insertion, i_num = i_number,age = grp, tf = fun_list[[5]]){
          t_2 <- tf - t_int
          inf_grp <- ifelse(age == "a","I2","I1")
          run_1 <- ssa.adaptivetau(i, t, RF, P, t_int)
          run_1[nrow(run_1),inf_grp] = run_1[nrow(run_1),inf_grp] + i_num
          init.2 = c(c(run_1[nrow(run_1),"S1"],run_1[nrow(run_1),"S2"]),
                     c(run_1[nrow(run_1),"E1"],run_1[nrow(run_1),"E2"]),
                     c(run_1[nrow(run_1),"I1"],run_1[nrow(run_1),"I2"]),
                     c(run_1[nrow(run_1),"R1"],run_1[nrow(run_1),"R2"]),
                     c(run_1[nrow(run_1),"D"]))
          run_2 <- ssa.adaptivetau(init.2, t, RF, P, t_2)
          run_2 <- cbind(apply(run_2[,"time", drop=FALSE],2,function(x) x+run_1[nrow(run_1),"time"]),
                          run_2[,-1])
          run <- rbind(run_1,run_2[-1,])
          run <- cbind(run, I = rowSums(run[,c("I1","I2")]))
          run <<- run[,c("time","I"), drop = FALSE]
          }

    #run a whole bunch of times and store into a df
    plot_dat <- data.frame(time = 1, I = 1, iter = 1)
    for(num in 1:batch){
      SIR_r()
      run <- cbind(run, iter = num)
      plot_dat <- rbind(plot_dat,run)
    }

    #trim and store into a new dataframe
    plot_dat$t_2 <- round(plot_dat$time,0)
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
    assign("graph",graph,envir = .GlobalEnv) #for editing or saving
  }
  if(FUN == "mul_ins"){
    #throw some errors
    if(is.null(grp) == TRUE){
        stop("No group specified!")
    }
   if(is.null(occ) == TRUE{
       stop("No rate of insertion specified!")
    }
   if(is.null(ins.start) == TRUE){
        stop("No start time specified!")
    }
    
    #multi-insertion function 
    mul_ins <- function(i = fun_list[[1]], t = fun_list[[2]], RF = fun_list[[3]], 
        P = fun_list[[4]], ins = occ, i_num = i_number,i_start = ins.start,age = grp, tf = fun_list[[5]]
        ){
        #local inits
        inf_grp <- ifelse(age == "a","I2","I1")
        res_df = data.frame(time = NULL,I = NULL,iter = NULL)
        #define first run, given time delay
        if(i_start > 0){
        results = as.data.frame(ssa.adaptivetau(i,t,RF,P,i_start))
        results = cbind(results,iter = 1)
        }
        #loop insertion runs
        for(i in 1:occ){
            if(i == 1 && i_start == 0){ #if no delay
                t_first = tf*(1/occ)
                i[inf_grp] = i[inf_grp] + i_num
                results = as.data.frame(ssa.adaptivetau(i,t,RF,P,t_first))
                results = cbind(results,iter = 1)
                res_df <<- rbind(res_df,results[,c("time","I","iter"),drop = FALSE])
                
            else{
               init_new = c(c(results[nrow(results),"S1"],results[nrow(results),"S2"]),
                          c(results[nrow(results),"E1"],results[nrow(results),"E2"]),
                          c(results[nrow(results),"I1"],results[nrow(results),"I2"]),
                          c(results[nrow(results),"R1"],results[nrow(results),"R2"]),
                          c(results[nrow(results),"D"]))
                init_new[inf_grp] = init_new[inf_grp] + i_num
                t_new = (tf-i_start*(i/occ))+results[nrow(results),"time"]
                run = as.data.frame(ssa.adaptivetau(init_new,t,RF,P,t_new))
                run = cbind(apply(run[,"time", drop=FALSE],2,function(x) x+results[nrow(results),"time"]),
                run[,-1])
                run = cbind(run,iter = i)
                results = rbind(results,run)
                res_df <<- results[,c("time","I","inter"),drop = FALSE]
                } 
    
            }
    }}
  else{
    stop("I haven't coded for that option yet, you dunce!")
  }
}
