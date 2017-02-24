#Seasonal change in contact rate
#20/2/2017 Sean Browning
#Utilize cosine function with a period of 365pi for a rough model
#Change initial values to R0 and easy to understand values
#compare new vs old
#Using EU demographic values
#depends: initial values, parameters, 'adaptivetau', 'googleVis'

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
  I2Y = 10,    #Infected pop2
  I2O = 5,
  I3Y = 10,   #Infected pop3
  I3O = 5,
  R1Y = 0,    #Recovered pop1
  R1O = 0,
  R2Y = 0,    #Recovered pop2
  R2O = 0,
  R3Y = 0,
  R3O = 0,     #Recovered pop3
  D1 = 0,
  D2 = 100,
  D3 = 500
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

#Parameters: R0, infectious period (days), crude birth rate (per 1000),
#crude under-5 mortality rate (per 1000), crude death rate (per 1000), vaccine coverage %
parameters = list(R0=c(14,16), infectious.period=7, birth.rate=10, death.rate=c(10,10), vaccination.rate=c(0.3,0.1,0.01))

new_RateF <- function(x, p, t) {
  #Parameters
  b1 <- p$R0[1]/(p$infectious.period)
  b2 <- p$R0[2]/(p$infectious.period)
  gamma <- 1/p$infectious.period
  alpha <- p$birth.rate/365
  omega <- p$death.rate/365
  V <- p$vaccination.rate
    #Population Values
  S1 <- x["S1Y"] + x["S1O"]
  S2 <- x["S2Y"] + x["S2O"]
  S3 <- x["S3Y"] + x["S3O"]
  I1 <- x["I1Y"] + x["I1O"]
  I2 <- x["I2Y"] + x["I2O"]
  I3 <- x["I3Y"] + x["I3O"]
  R1 <- x["R1Y"] + x["R1O"]
  R2 <- x["R2Y"] + x["R2O"]
  R3 <- x["R3Y"] + x["R3O"]
  N1 <- S1 + I1 + R1
  N2 <- S2 + I2 + R2
  N3 <- S3 + I3 + R3
  N <- N1 + N2 + N3
  beta1 <- (b1*cos(t/(365*pi))+(1)
  beta2 <- (b2*cos(t/(365*pi))+(1)

  return(c(x["S1Y"] * beta1 * (I1/(N1)), #infection rates
           x["S1Y"] * beta2 * (I2/(N1)),
           x["S1Y"] * beta2 * (I3/(N1)),
           x["S1O"] * beta1 * (I1/(N1)),
           x["S1O"] * beta2 * (I2/(N1)),
           x["S1O"] * beta2 * (I3/(N1)),
           x["S2Y"] * beta1 * (I2/(N2)),
           x["S2Y"] * beta2 * (I1/(N2)),
           x["S2Y"] * beta2 * (I3/(N2)),
           x["S2O"] * beta1 * (I2/(N2)),
           x["S2O"] * beta2 * (I1/(N2)),
           x["S2O"] * beta2 * (I3/(N2)),
           x["S3Y"] * beta1 * (I3/(N3)),
           x["S3Y"] * beta2 * (I1/(N3)),
           x["S3Y"] * beta2 * (I2/(N3)),
           x["S3O"] * beta1 * (I3/(N3)),
           x["S3O"] * beta2 * (I1/(N3)),
           x["S3O"] * beta2 * (I2/(N3)),
           x["I1Y"] * gamma,             #recovery rates
           x["I1O"] * gamma,
           x["I2Y"] * gamma,
           x["I2O"] * gamma,
           x["I3Y"] * gamma,
           x["I3O"] * gamma,
           (N1/1000) * alpha,             #birth rates per 1000 pop
           (N2/1000) * alpha, 
           (N3/1000) * alpha,
           (x["S1Y"]/1000) * omega[1],             #death rates
           (x["I1Y"]/1000) * omega[1],             #under-5:
           (x["R1Y"]/1000) * omega[1],             #per 1000 
           (x["S1O"]/1000) * omega[2],             #crude:
           (x["I1O"]/1000) * omega[2],             #per 1000 pop
           (x["R1O"]/1000) * omega[2],
           (x["S2Y"]/1000) * omega[1],
           (x["I2Y"]/1000) * omega[1],
           (x["R2Y"]/1000) * omega[1],
           (x["S2O"]/1000) * omega[2],
           (x["I2O"]/1000) * omega[2],
           (x["R2O"]/1000) * omega[2],
           (x["S3Y"]/1000) * omega[1],
           (x["I3Y"]/1000) * omega[1],
           (x["R3Y"]/1000) * omega[1],
           (x["S3O"]/1000) * omega[2],
           (x["I3O"]/1000) * omega[2],
           (x["R3O"]/1000) * omega[2],
           ((N1/1000) * alpha) * V[1] * (t>x["D1"]),
           ((N2/1000) * alpha) * V[2] * (t>x["D2"]),
           ((N3/1000) * alpha) * V[3] * (t>x["D3"])
  ))
  }

old_RateF <- function(x, p, t) {
  #Parameters
  beta1 <- p$R0[1]/(p$infectious.period)
  beta2 <- p$R0[2]/(p$infectious.period)
  gamma <- 1/p$infectious.period
  alpha <- p$birth.rate/365
  omega <- p$death.rate/365
  V <- p$vaccination.rate
    #Population Values
  S1 <- x["S1Y"] + x["S1O"]
  S2 <- x["S2Y"] + x["S2O"]
  S3 <- x["S3Y"] + x["S3O"]
  I1 <- x["I1Y"] + x["I1O"]
  I2 <- x["I2Y"] + x["I2O"]
  I3 <- x["I3Y"] + x["I3O"]
  R1 <- x["R1Y"] + x["R1O"]
  R2 <- x["R2Y"] + x["R2O"]
  R3 <- x["R3Y"] + x["R3O"]
  N1 <- S1 + I1 + R1
  N2 <- S2 + I2 + R2
  N3 <- S3 + I3 + R3
  N <- N1 + N2 + N3
  return(c(x["S1Y"] * beta1 * (I1/(N1)), #infection rates
           x["S1Y"] * beta2 * (I2/(N1)),
           x["S1Y"] * beta2 * (I3/(N1)),
           x["S1O"] * beta1 * (I1/(N1)),
           x["S1O"] * beta2 * (I2/(N1)),
           x["S1O"] * beta2 * (I3/(N1)),
           x["S2Y"] * beta1 * (I2/(N2)),
           x["S2Y"] * beta2 * (I1/(N2)),
           x["S2Y"] * beta2 * (I3/(N2)),
           x["S2O"] * beta1 * (I2/(N2)),
           x["S2O"] * beta2 * (I1/(N2)),
           x["S2O"] * beta2 * (I3/(N2)),
           x["S3Y"] * beta1 * (I3/(N3)),
           x["S3Y"] * beta2 * (I1/(N3)),
           x["S3Y"] * beta2 * (I2/(N3)),
           x["S3O"] * beta1 * (I3/(N3)),
           x["S3O"] * beta2 * (I1/(N3)),
           x["S3O"] * beta2 * (I2/(N3)),
           x["I1Y"] * gamma,             #recovery rates
           x["I1O"] * gamma,
           x["I2Y"] * gamma,
           x["I2O"] * gamma,
           x["I3Y"] * gamma,
           x["I3O"] * gamma,
           (N1/1000) * alpha,             #birth rates per 1000 pop
           (N2/1000) * alpha, 
           (N3/1000) * alpha,
           (x["S1Y"]/1000) * omega[1],             #death rates
           (x["I1Y"]/1000) * omega[1],             #under-5:
           (x["R1Y"]/1000) * omega[1],             #per 1000 
           (x["S1O"]/1000) * omega[2],             #crude:
           (x["I1O"]/1000) * omega[2],             #per 1000 pop
           (x["R1O"]/1000) * omega[2],
           (x["S2Y"]/1000) * omega[1],
           (x["I2Y"]/1000) * omega[1],
           (x["R2Y"]/1000) * omega[1],
           (x["S2O"]/1000) * omega[2],
           (x["I2O"]/1000) * omega[2],
           (x["R2O"]/1000) * omega[2],
           (x["S3Y"]/1000) * omega[1],
           (x["I3Y"]/1000) * omega[1],
           (x["R3Y"]/1000) * omega[1],
           (x["S3O"]/1000) * omega[2],
           (x["I3O"]/1000) * omega[2],
           (x["R3O"]/1000) * omega[2],
           ((N1/1000) * alpha) * V[1] * (t>x["D1"]),
           ((N2/1000) * alpha) * V[2] * (t>x["D2"]),
           ((N3/1000) * alpha) * V[3] * (t>x["D3"])
  ))
  ))
 }

#runs
r_1=ssa.adaptivetau(init.values, transitions, new_RateF, parameters, tf=1000)
r_2=ssa.adaptivetau(init.values, transitions, old_RateF, parameters, tf=1000)

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
                             vAxis="{title:'Count', titleTextStyle:{color:'black'}, scaleType: 'log'}",
                             width = 668,
                             height = 400
                           ))
gvplot_dat_2 <- as.data.frame(r_2[,plotvars, drop = FALSE]) #store plot variables in a df
gvisgraph_2 <- gvisLineChart(gvplot_dat_2,
                           options = list(
                             title = "constant beta SIR Model",
                             hAxis="{title:'Time', titleTextStyle:{color:'black'}}",
                             vAxis="{title:'Count', titleTextStyle:{color:'black'}, scaleType: 'log'}",
                             width = 668,
                             height = 400
                           ))
plotplot <- gvisMerge(gvisgraph_1,gvisgraph_2, horizontal = TRUE,
                        tableOptions="bgcolor=\"#CCCCCC\" cellspacing=10")

#view
plot(plotplot)
