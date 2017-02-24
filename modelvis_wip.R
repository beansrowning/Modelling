#model visualization work in progress
#not working, figure out data structures 23/2

SIRplot <- function(mat,vars = c("time", "S11", "I11", "R11"),y.axis = "lin", x.range = c(0,(mat[nrow(mat),"time"])),parameters = NULL){
    #determine row numbers for given x range 
    rag <- c(which(mat[,"time"] == x.range[1]), 
             which(mat[,"time"] == x.range[2])
             )
    
    #store plot variables in a df
    gvplot_dat <- data.frame()
    dim(gvplot_dat) <- c(rag[2],0)
    for (i in 1:length(vars)) {
      gvplot_dat <- cbind(gvplot_dat, mat[rag[1]:rag[2],vars[i]])
      next()
    }
  
    #begin plotting 
    if (is.null(parameters) == TRUE) { #no parameter table
        g_graph <- gvisLineChart(gvplot_dat,
                 options = list(
                   title = "Model Output",
                   hAxis="{title:'colnames(gvplot_dat)[1]', titleTextStyle:{color:'black'}}",
                   vAxis="{title:'Count', titleTextStyle:{color:'black'}, scaleType: 'y.axis'}",
                   width = 668,
                   height = 400
                 ))
        plot(g_graph)
    } else { #create parameter table and merge 
        g_graph <- gvisLineChart(gvplot_dat,
                 options = list(
                   title = "Model Output",
                   hAxis="{title:'colnames(gvplot_dat)[1]', titleTextStyle:{color:'black'}}",
                   vAxis="{title:'Count', titleTextStyle:{color:'black'}, scaleType: 'y.axis'}",
                   width = 668,
                   height = 400
                 ))
        paratab <- data.frame(Parameter = names(parameters), Value = parameters, stringsAsFactors = FALSE)
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
