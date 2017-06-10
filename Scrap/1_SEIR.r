#Single Population Measles SEIR Model with gVis output
#21/2/2017 Sean Browning
#Added birth, death, and vaccination rates
#Uniform Mixing
#Depends: 'adaptivetau', 'googleVis'

library(adaptivetau)
library(googleVis)

#Defining input
init.values = c(
  S = c(10^4,10^5),
  E = c(0,0),
  I = c(20,2),
  R = c(0,0),
  D = 100 #begining of vaccination
  )

transitions = list(
  #young
  c(S1 = -1, E1 = +1), #Infection
  c(E1 = -1, I1 = +1), #infectious
  c(I1 = -1, R1 = +1),  #Recovery
  c(S1 = +1), #Vaccinations
  c(R1 = +1), #Deaths
  c(S1 = -1),
  c(E1 = -1),
  c(I1 = -1),
  c(R1 = -1),
  #old
  c(S2 = -1, E2 = +1), #Infection
  c(E2 = -1, I2 = +1), #infectious
  c(I2 = -1, R2 = +1),  #Recovery
  c(S2 = -1),
  c(E2 = -1),
  c(I2 = -1),
  c(R2 = -1)
)

parameters = c(
  R0 = 16,
  infectious.period = 7, #days
  latent.period = 8, #days
  vacc.pro = 0.9, #proportion
  birth.rate = 10, #per 1000, anum
  death.rate = c(10,10) #per 1000, anum
)

RateF <- function(x, p, t) {
  #local parameters
  beta <- p["R0"]/(p["infectious.period"])
  f <- 1/p["latent.period"]
  gamma <- 1/p["infectious.period"]
  alpha <- p["birth.rate"]/365
  omega <- c(p["death.rate1"]/365,
             p["death.rate2"]/365)
  v <- ifelse(t<x["D"], p["vacc.pro"], 0)
  #local population values
  S1 <- x["S1"]
  E1 <- x["E1"]
  I1 <- x["I1"]
  R1 <- x["R1"]
  S2 <- x["S2"]
  E2 <- x["E2"]
  I2 <- x["I2"]
  R2 <- x["R2"]
  S <- S1 + S2
  E <- E1 + E2
  I <- I1 + I2
  R <- R1 + R2
  N1 <- S1 + E1 + I1 + R1
  N2 <- S2 + E2 + I2 + R2
  Nt <- N1 + N2
  #Rate Functions
  return(c(S11 * beta[1] * (I/Nt), #pop1young
           E1 * f,
           I1 * gamma,
           (Nt/1000) * alpha * (1-v) ,
           (Nt/1000) * alpha * v,
           (S1/1000) * omega[1],
           (E1/1000) * omega[1],
           (I1/1000) * omega[1],
           (R1/1000) * omega[1],
           S2 * beta * (I/Nt), #old
           E2 * f,
           I2 * gamma,
           (S2/1000) * omega[2],
           (E2/1000) * omega[2],
           (I2/1000) * omega[2],
           (R2/1000) * omega[2]
  ))
 }

#runs
set.seed(100)
runs=ssa.adaptivetau(init.values, transitions, RateF, parameters, tf=50)


#Plotting
gvplot_dat <- data.frame(time = runs[,"time"], #store plot variables in a df
S = runs[,"S1"] + runs[,"S2"],
I = runs[,"I1"] + runs[,"I1"],
R = runs[,"R1"] + runs[,"R2"])
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
                        tableOptions="bgcolor=\"#607D8B\" cellspacing=10" #coral blue number 5
                           )
plot(plot)
