#Basic Measles SEIR Model with gVis output
#20/2/2017 Sean Browning
#Depends: 'adaptivetau', 'googleVis'

library(adaptivetau)
library(googleVis)

#Defining initial values
init.values = c(
  S = 10^5,
  E = 0,
  I = 20,
  R = 0
  )    

#Defining Transitions 
transitions = list(
  c(S = -1, E = +1), #Infection
  c(E = -1, I = +1), #infectious 
  c(I = -1, R = +1)  #Recovery
)

#define parameters 
parameters = c(
  R0 = 16,
  infectious.period = 7,
  latent.period = 8
)

RateF <- function(x, p, t) {
  #Parameters
  beta <- p["R0"]/(p["infectious.period"])
  f <- 1/p["latent.period"]
  gamma <- 1/p["infectious.period"]
    #Total population
  S <- x["S"]
  E <- x["E"]
  I <- x["I"]
  R <- x["R"]
  N <- S + E + I + R
  #Rate Functions
  return(c(S * beta * (I/N), #infection
           E * f,            #Infectious
           I * gamma         #recovery rates
  ))
 }

#runs 
set.seed(100)
r=ssa.adaptivetau(init.values, transitions, RateF, parameters, tf=150)


#Plotting 
gvplot_dat <- data.frame(r[,c("time", "S", "I", "R"), drop = FALSE]) #store plot variables in a df
gvisgraph  <- gvisLineChart(gvplot_dat, 
                           options = list(
                             title = "Measles SEIR Model",
                             hAxis="{title:'Time', titleTextStyle:{color:'black'}}",
                             vAxis="{title:'Count', titleTextStyle:{color:'black'}, scaleType: 'lin'}",
                             width = 668,
                             height = 400
                           ))
para_tab <- data.frame(Parameter = names(parameters), Value = parameters, stringsAsFactors = FALSE) 
gvistable <- gvisTable(para_tab,options = list( 
                            width = 200,
                            height = 400
                           ))
plot <- gvisMerge(gvisgraph,gvistable, horizontal = TRUE, 
                        tableOptions="bgcolor=\"#CCCCCC\" cellspacing=10"
                           )
plot(plot)
