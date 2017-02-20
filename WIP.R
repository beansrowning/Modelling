#3 meta-population SIR model with 2 rates of infection 
#Young & Old divisions, variable vaccine start times, constant birth and death rate
#Constant infectious rate
#Sean Browning 20/2/2017
#Depends: 'adaptivetau', 'googleVis'

library(adaptivetau)
library(googleVis)

#Defining initial values
init.values = c(
  S1Y = 10^4, #Susceptible pop1
  S1O = 10^4, 
  S2Y = 10^4, #Susceptible pop2
  S2O = 10^4,
  S3Y = 10^4, #Susceptible pop3
  S3O = 10^4,
  I1Y = 10,   #Infected pop1
  I1O = 5,
  I2Y = 0,    #Infected pop2
  I2O = 0,    
  I3Y = 50,   #Infected pop3
  I3O = 10,    
  R1Y = 0,    #Recovered pop1
  R1O = 0,
  R2Y = 0,    #Recovered pop2
  R2O = 0,    
  R3Y = 0,
  R3O = 0,     #Recovered pop3
  D1 = 0,
  D2 = 1000,
  D3 = 2000
  )    

#Defining Transitions 
transitions = list(
  c(S1Y = -1, I1Y = +1), #Infection
  c(S1Y = -1, I1Y = +1), 
  c(S1Y = -1, I1Y = +1), 
  c(S1O = -1, I1O = +1), 
  c(S1O = -1, I1O = +1), 
  c(S1O = -1, I1O = +1), 
  c(S2Y = -1, I2Y = +1), 
  c(S2Y = -1, I2Y = +1), 
  c(S2Y = -1, I2Y = +1), 
  c(S2O = -1, I2O = +1), 
  c(S2O = -1, I2O = +1), 
  c(S2O = -1, I2O = +1), 
  c(S3Y = -1, I3Y = +1), 
  c(S3Y = -1, I3Y = +1), 
  c(S3Y = -1, I3Y = +1), 
  c(S3O = -1, I3O = +1), 
  c(S3O = -1, I3O = +1), 
  c(S3O = -1, I3O = +1),
  c(I1Y = -1, R1Y = +1), #Recovery 
  c(I1O = -1, R1O = +1), 
  c(I2Y = -1, R2Y = +1), 
  c(I2O = -1, R2O = +1), 
  c(I3Y = -1, R3Y = +1), 
  c(I3O = -1, R3O = +1), 
  c(S1Y = +1), #Birth
  c(S2Y = +1), 
  c(S3Y = +1), 
  c(S1Y = -1), #Deaths
  c(I1Y = -1), 
  c(R1Y = -1), 
  c(S1O = -1), 
  c(I1O = -1), 
  c(R1O = -1), 
  c(S2Y = -1), 
  c(I2Y = -1), 
  c(R2Y = -1), 
  c(S2O = -1), 
  c(I2O = -1), 
  c(R2O = -1), 
  c(S3Y = -1),  
  c(I3Y = -1),  
  c(R3Y = -1), 
  c(S3O = -1), 
  c(I3O = -1), 
  c(R3O = -1),  
  c(S1Y = -1, R1Y = +1), #Vaccinations
  c(S2Y = -1, R2Y = +1),
  c(S3Y = -1, R3Y = +1)
)
#rate function
RateF <- function(x, p, t) {
  return(c(x["S1Y"] * (x["I1Y"]+x["I1O"])*p$beta[1], #infection rates
           x["S1Y"] * (x["I2Y"]+x["I2O"])*p$beta[2],
           x["S1Y"] * (x["I3Y"]+x["I3O"])*p$beta[2],
           x["S1O"] * (x["I1Y"]+x["I1O"])*p$beta[1], 
           x["S1O"] * (x["I2Y"]+x["I2O"])*p$beta[2],
           x["S1O"] * (x["I3Y"]+x["I3O"])*p$beta[2],
           x["S2Y"] * (x["I2Y"]+x["I2O"])*p$beta[1],
           x["S2Y"] * (x["I1Y"]+x["I1O"])*p$beta[2],
           x["S2Y"] * (x["I3Y"]+x["I3O"])*p$beta[2],
           x["S2O"] * (x["I2Y"]+x["I2O"])*p$beta[1],
           x["S2O"] * (x["I1Y"]+x["I1O"])*p$beta[2],
           x["S2O"] * (x["I3Y"]+x["I3O"])*p$beta[2],
           x["S3Y"] * (x["I3Y"]+x["I3O"])*p$beta[1], 
           x["S3Y"] * (x["I1Y"]+x["I1O"])*p$beta[2],
           x["S3Y"] * (x["I2Y"]+x["I2O"])*p$beta[2],
           x["S3O"] * (x["I3Y"]+x["I3O"])*p$beta[1],
           x["S3O"] * (x["I1Y"]+x["I1O"])*p$beta[2],
           x["S3O"] * (x["I2Y"]+x["I2O"])*p$beta[2],
           x["I1Y"] * p$gamma,             #recovery rates
           x["I1O"] * p$gamma,
           x["I2Y"] * p$gamma,
           x["I2O"] * p$gamma,
           x["I3Y"] * p$gamma,
           x["I3O"] * p$gamma,
           x["S1O"] * p$alpha,             #birth rates
           x["S2O"] * p$alpha,
           x["S3O"] * p$alpha,
           x["S1Y"] * p$omega[1],             #death rates
           x["I1Y"] * p$omega[1],
           x["R1Y"] * p$omega[1],
           x["S1O"] * p$omega[2],            
           x["I1O"] * p$omega[2],
           x["R1O"] * p$omega[2],
           x["S2Y"] * p$omega[1],
           x["I2Y"] * p$omega[1],
           x["R2Y"] * p$omega[1],
           x["S2O"] * p$omega[2],
           x["I2O"] * p$omega[2],
           x["R2O"] * p$omega[2],
           x["S3Y"] * p$omega[1],
           x["I3Y"] * p$omega[1],
           x["R3Y"] * p$omega[1],
           x["S3O"] * p$omega[2],
           x["I3O"] * p$omega[2],
           x["R3O"] * p$omega[2],
           x["S1Y"] * p$V[1] * (t>x["D1"]),
           x["S2Y"] * p$V[2] * (t>x["D2"]),
           x["S3Y"] * p$V[3] * (t>x["D3"])
  ))
}

#define parameters 
parameters = list(beta=c(8e-8,9e-8), gamma=0.4e-3, alpha=3e-8, omega=c(1e-9,2e-9), V=c(1e-5,1e-3,1e-2))

#Run the model
set.seed(4)
r=ssa.adaptivetau(init.values, transitions, RateF, parameters, tf=5000)

#Adding 3 variables to sum all of the SIR groups
r <-cbind(r, S= rowSums(r[,c("S1Y", "S1O","S2Y", "S2O", "S3Y", "S3O")]), #
          I = rowSums(r[,c("I1Y", "I1O", "I2Y", "I2O", "I3Y", "I3O")]), #
          R = rowSums(r[,c("R1Y", "R1O", "R2Y", "R2O", "R3Y", "R3O")]) #
          ) 

#giving each run an id based on the parameters for the save file
runid <- paste(parameters$beta[1],parameters$beta[2],parameters$gamma,parameters$alpha,parameters$omega,parameters$V[1],parameters$V[2],parameters$V[3], sep = ".")

#plotting
plotvars <- c("time", "S", "I", "R") #change to plot other populations 
gvplot_dat <- as.data.frame(r[,plotvars, drop = FALSE]) #store plot variables in a df
gvisgraph <- gvisLineChart(gvplot_dat, 
                           options = list(
                             title = "Metapopulation SIR Model",
                             hAxis="{title:'Time', titleTextStyle:{color:'black'}}",
                             vAxis="{title:'Count', titleTextStyle:{color:'black'}, scaleType: 'lin'}",
                             width = 668,
                             height = 400
                           ))
#create a df with the starting parameters
paratable <- as.data.frame(cbind(parameter = c("beta[1]","beta[2]","gamma","alpha","omega[1]","omega[2]"),
                                 value = c(parameters$beta[1],parameters$beta[2],parameters$gamma,parameters$alpha,parameters$omega[1],parameters$omega[2])))
#create table of parameters
par_table <- gvisTable(paratable, options = list( 
  width = 200,
  height = 400
))
gvisplot1 <- gvisMerge(gvisgraph,par_table,horizontal = TRUE, #merge the graph and table side-by-side
                      tableOptions="bgcolor=\"#CCCCCC\" cellspacing=10")

vacdf <- data.frame(cbind(Population = c("1", "2", "3"), Start = c(init.values["D1"],init.values["D2"],init.values["D3"])
                           ))
vac_tab <- gvisTable(vacdf, options = list(
  width = 868,
  height = 125
))

plotplot <- gvisMerge(gvisplot1,vac_tab, horizontal = FALSE, #merge table with vaccine program start times on bottom
                      tableOptions="bgcolor=\"#CCCCCC\" cellspacing=10"
)
#save to file
#print(gvisplot, file = paste("SIR",runid,"html", sep = ".")) #check your wd

#plot (>ie 8 required for viewing, set firefox as default on remote desktop)
plot(plotplot)


