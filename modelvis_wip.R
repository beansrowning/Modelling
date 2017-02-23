#model visualization work in progress

SIRplot <- function(mat,...,y.axis = "lin", x.range = c(0,(mat[length(mat),"time"])),parameters = NULL){
    #store plot variables into a list 
    plot_vars <- list(...) 
    #store plot variables in a df
    gvplot_dat <- data.frame(mat[x.range1[1]:x.range[2],plot_vars], stringsAsFactors = FALSE)
    #begin plotting 
    if (parameters = NULL){ #no parameter table
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
        g_merge <- gvisMerge(g_graph,g_table, horizontal = TRUE,
                   tableOptions="bgcolor=\"#607D8B\" cellspacing=10" #coral blue number 5
                      )
        plot(g_merge)
    }
}