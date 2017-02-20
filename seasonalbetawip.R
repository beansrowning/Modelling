#Seasonal change in contact rate 
#20/2/2017 Sean Browning
#Utilize cosine function with a period of 365pi for a rough model 
#Change initial values to R0 and easy to understand values
#compare new vs old 
#depends: initial values, parameters, 'adaptivetau', 'googleVis', 'stats'

library(adaptivetau)
library(googleVis)
library(stats)

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

#define parameters 
parameters = list(beta=c(8e-8,9e-8), gamma=0.4e-3, alpha=3e-8, omega=c(1e-9,2e-9), V=c(1e-5,1e-3,1e-2))

new_RateF <- function(x, p, t) {
  b1 <- abs((p$beta[1]-(.5*p$beta[1]))*cos(t/(365*pi))+(.1*p$beta[1]))
  b2 <- abs((p$beta[2]-(.5*p$beta[2]))*cos(t/(365*pi))+(.1*p$beta[2]))
  
  return(c(x["S1Y"] * (x["I1Y"]+x["I1O"])*b1, #infection rates
           x["S1Y"] * (x["I2Y"]+x["I2O"])*b2,
           x["S1Y"] * (x["I3Y"]+x["I3O"])*b2,
           x["S1O"] * (x["I1Y"]+x["I1O"])*b1, 
           x["S1O"] * (x["I2Y"]+x["I2O"])*b2,
           x["S1O"] * (x["I3Y"]+x["I3O"])*b2,
           x["S2Y"] * (x["I2Y"]+x["I2O"])*b1,
           x["S2Y"] * (x["I1Y"]+x["I1O"])*b2,
           x["S2Y"] * (x["I3Y"]+x["I3O"])*b2,
           x["S2O"] * (x["I2Y"]+x["I2O"])*b1,
           x["S2O"] * (x["I1Y"]+x["I1O"])*b2,
           x["S2O"] * (x["I3Y"]+x["I3O"])*b2,
           x["S3Y"] * (x["I3Y"]+x["I3O"])*b1, 
           x["S3Y"] * (x["I1Y"]+x["I1O"])*b2,
           x["S3Y"] * (x["I2Y"]+x["I2O"])*b2,
           x["S3O"] * (x["I3Y"]+x["I3O"])*b1,
           x["S3O"] * (x["I1Y"]+x["I1O"])*b2,
           x["S3O"] * (x["I2Y"]+x["I2O"])*b2,
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

old_RateF <- function(x, p, t) {
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

#runs 
set.seed(100)
r_1=ssa.adaptivetau(init.values, transitions, new_RateF, parameters, tf=5000)
r_2=ssa.adaptivetau(init.values, transitions, old_RateF, parameters, tf=5000)

#cleaning 
    r_1 <- cbind(r_1, S= rowSums(r_1[,c("S1Y", "S1O","S2Y", "S2O", "S3Y", "S3O")]), #
    I = rowSums(r_1[,c("I1Y", "I1O", "I2Y", "I2O", "I3Y", "I3O")]), #
    R = rowSums(r_1[,c("R1Y", "R1O", "R2Y", "R2O", "R3Y", "R3O")]) #
    ) 
    r_2 <- cbind(r_2, S= rowSums(r_2[,c("S1Y", "S1O","S2Y", "S2O", "S3Y", "S3O")]), #
    I = rowSums(r_2[,c("I1Y", "I1O", "I2Y", "I2O", "I3Y", "I3O")]), #
    R = rowSums(r_2[,c("R1Y", "R1O", "R2Y", "R2O", "R3Y", "R3O")]) #
    ) 


#Plotting 
plotvars <- c("time", "S", "I", "R") #change to plot other populations 
gvplot_dat_1 <- as.data.frame(r_1[,plotvars, drop = FALSE]) #store plot variables in a df
gvisgraph_1 <- gvisLineChart(gvplot_dat_1, 
                           options = list(
                             title = "Seasonal beta SIR Model",
                             hAxis="{title:'Time', titleTextStyle:{color:'black'}}",
                             vAxis="{title:'Count', titleTextStyle:{color:'black'}, scaleType: 'lin'}",
                             width = 668,
                             height = 400
                           ))
gvplot_dat_2 <- as.data.frame(r_2[,plotvars, drop = FALSE]) #store plot variables in a df
gvisgraph_2 <- gvisLineChart(gvplot_dat_2, 
                           options = list(
                             title = "constant beta SIR Model",
                             hAxis="{title:'Time', titleTextStyle:{color:'black'}}",
                             vAxis="{title:'Count', titleTextStyle:{color:'black'}, scaleType: 'lin'}",
                             width = 668,
                             height = 400
                           ))
plotplot <- gvisMerge(gvisgraph_1,gvisgraph_2, horizontal = TRUE, 
                        tableOptions="bgcolor=\"#CCCCCC\" cellspacing=10")
                        
#view
plot(plotplot)
