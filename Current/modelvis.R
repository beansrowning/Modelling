#model visualization work in progress
#working 25/2/2017

SIRplot <- function(mat,vars = c("time", "S11", "I11", "R11"),y.axis = "lin", x.range = c(0,(mat[nrow(mat),"time"])),parameters = NULL){
    #determine row numbers for given x.range
    rag <- which(mat[,"time"] >= x.range[1] & mat[,"time"] <= x.range[2])
    x.range <- c(rag[1],rag[length(rag)])
    
    #subset plot variables from matrix and store in a df
    gvplot_dat <- as.data.frame(runs[x.range[1]:x.range[2],vars])
    
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