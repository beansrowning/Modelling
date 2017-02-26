#Three Population Measles SEIR Model
#25/2/2017 Sean Browning
#Added birth, death, and vaccination rates
#Two rates of infection
#offloaded visualization to a seprate function
#Depends: 'adaptivetau', 'googleVis', modelvis.R
library(adaptivetau)
library(googleVis)

#Defining input
init.values = c(
  S1 = c(10^4,10^5),
  E1 = c(0,0),
  I1 = c(10,2),
  R1 = c(0,0),
  S2 = c(10^4,10^5),
  E2 = c(0,0),
  I2 = c(10,2),
  R2 = c(0,0),
  S3 = c(10^4,10^5),
  E3 = c(0,0),
  I3 = c(10,2),
  R3 = c(0,0),
  D = c(30,100,0) #Begining of vaccination
  )

transitions = list(
  #pop1
  #young
  c(S11 = -1, E11 = +1), #Infection
  c(E11 = -1, I11 = +1), #infectious
  c(I11 = -1, R11 = +1),  #Recovery
  c(S11 = +1), #Vaccinations
  c(R11 = +1),
  c(S11 = -1, S12 = +1), #aging
  c(E11 = -1, E12 = +1),
  c(I11 = -1, I12 = +1),
  c(R11 = -1, R12 = +1),
  #old
  c(S12 = -1, E12 = +1), #Infection
  c(E12 = -1, I12 = +1), #infectious
  c(I12 = -1, R12 = +1),  #Recovery
  c(S12 = -1), #Deaths
  c(E12 = -1),
  c(I12 = -1),
  c(R12 = -1),
  c(I12 = +1) #migration event
  #pop2
  #young
  c(S21 = -1, E21 = +1), #Infection
  c(E21 = -1, I21 = +1), #infectious
  c(I21 = -1, R21 = +1), #Recovery
  c(S21 = +1), #Vaccinations
  c(R21 = +1),
  c(S21 = -1, S22 = +1), #aging
  c(E21 = -1, E22 = +1),
  c(I21 = -1, I22 = +1),
  c(R21 = -1, R22 = +1),
  #old
  c(S22 = -1, E22 = +1), #Infection
  c(E22 = -1, I22 = +1), #infectious
  c(I22 = -1, R22 = +1),  #Recovery
  c(S22 = -1), #Deaths
  c(E22 = -1),
  c(I22 = -1),
  c(R22 = -1),
  c(I22 = +1) #migration event
  #pop3
  #young
  c(S31 = -1, E31 = +1), #Infection
  c(E31 = -1, I31 = +1), #infectious
  c(I31 = -1, R31 = +1), #Recovery
  c(S31 = +1), #Vaccinations
  c(R31 = +1),
  c(S31 = -1, S32 = +1), #aging
  c(E31 = -1, E32 = +1),
  c(I31 = -1, I32 = +1),
  c(R31 = -1, R32 = +1),
  #old
  c(S32 = -1, E32 = +1), #Infection
  c(E32 = -1, I32 = +1), #infectious
  c(I32 = -1, R32 = +1),  #Recovery
  c(S32 = -1), #Deaths
  c(E32 = -1),
  c(I32 = -1),
  c(R32 = -1),
  c(I32 = +2) #migration event
)

parameters = c(
  R0 = c(16,8),
  infectious.period = 7, #days
  latent.period = 8, #days
  vacc.pro = c(0.6,0.6,0.6), #proportion
  birth.rate = 10, #per 1000, anum
  young.size = 5, #years in the young division
  death.rate = 10, #per 1000, anum
  migration.events = 2 #per anum
)

RateF <- function(x, p, t) {
  #local parameters
  beta <- c(p["R01"]/(p["infectious.period"]),
            p["R02"]/(p["infectious.period"])
        )
  f <- 1/p["latent.period"]
  gamma <- 1/p["infectious.period"]
  alpha <- p["birth.rate"]/365
  omega <- p["death.rate"]/365
  delta <- 1/(p["young.size"]*365)
  v1 <- ifelse(t<x["D1"], p["vacc.pro1"], 0)
  v2 <- ifelse(t<x["D2"], p["vacc.pro2"], 0)
  v3 <- ifelse(t<x["D3"], p["vacc.pro2"], 0)
  migration <- p["migration.events"]/365
  #local population values
  S1 <- x["S11"] + x["S12"]
  E1 <- x["E11"] + x["E12"]
  I1 <- x["I11"] + x["I12"]
  R1 <- x["R11"] + x["I12"]
  N1.y <- x["S11"] + x["E11"] + x["I11"] + x["R11"]
  N1.o <- x["S12"] + x["E12"] + x["I12"] + x["R12"]
  N1.t <- N1.y + N1.o
  S2 <- x["S21"] + x["S22"]
  E2 <- x["E21"] + x["E22"]
  I2 <- x["I21"] + x["I22"]
  R2 <- x["R21"] + x["I22"]
  N2.y <- x["S21"] + x["E21"] + x["I21"] + x["R21"]
  N2.o <- x["S22"] + x["E22"] + x["I22"] + x["R22"]
  N2.t <- N2.y + N2.o
  S3 <- x["S31"] + x["S32"]
  E3 <- x["E31"] + x["E32"]
  I3 <- x["I31"] + x["I32"]
  R3 <- x["R31"] + x["I32"]
  N3.y <- x["S31"] + x["E31"] + x["I31"] + x["R31"]
  N3.o <- x["S32"] + x["E32"] + x["I32"] + x["R32"]
  N3.t <- N3.y + N3.o
  N.t <- N1.t + N2.t + N3.t
  #Rate Functions
  return(c(x["S11"] * beta[1] * (I1/N.t) +  #young
           x["S11"] * beta[2] * (I2/N.t) +
           x["S11"] * beta[2] * (I3/N.t),
           x["E11"] * f,
           x["I11"] * gamma,
           (N1.t/1000) * alpha * (1-v1) ,
           (N1.t/1000) * alpha * v1,
           x["S11"] * delta,
           x["E11"] * delta,
           x["I11"] * delta,
           x["R11"] * delta,
           x["S12"] * beta[1] * (I1/N.t) +  #old
           x["S12"] * beta[2] * (I2/N.t) +
           x["S12"] * beta[2] * (I3/N.t),
           x["E12"] * f,
           x["I12"] * gamma,
           (x["S12"]/1000) * omega,
           (x["E12"]/1000) * omega,
           (x["I12"]/1000) * omega,
           (x["R12"]/1000) * omega,
           migration,
           #pop2
           x["S21"] * beta[1] * (I2/N.t) +  #young
           x["S21"] * beta[2] * (I1/N.t) +
           x["S21"] * beta[2] * (I3/N.t),
           x["E21"] * f,
           x["I21"] * gamma,
           (N2.t/1000) * alpha * (1-v2) ,
           (N2.t/1000) * alpha * v2,
           x["S21"] * delta,
           x["E21"] * delta,
           x["I21"] * delta,
           x["R21"] * delta,
           x["S22"] * beta[1] * (I2/N.t) +  #old
           x["S22"] * beta[2] * (I1/N.t) +
           x["S22"] * beta[2] * (I3/N.t),
           x["E22"] * f,
           x["I22"] * gamma,
           (x["S22"]/1000) * omega,
           (x["E22"]/1000) * omega,
           (x["I22"]/1000) * omega,
           (x["R22"]/1000) * omega,
           migration,
           #pop3
           x["S31"] * beta[1] * (I3/N.t) +  #young
           x["S31"] * beta[2] * (I1/N.t) +
           x["S31"] * beta[2] * (I2/N.t),
           x["E31"] * f,
           x["I31"] * gamma,
           (N3.t/1000) * alpha * (1-v3) ,
           (N3.t/1000) * alpha * v3,
           x["S31"] * delta,
           x["E31"] * delta,
           x["I31"] * delta,
           x["R31"] * delta,
           x["S32"] * beta[1] * (I3/N.t) +  #old
           x["S32"] * beta[2] * (I1/N.t) +
           x["S32"] * beta[2] * (I2/N.t),
           x["E32"] * f,
           x["I32"] * gamma,
           (x["S32"]/1000) * omega,
           (x["E32"]/1000) * omega,
           (x["I32"]/1000) * omega,
           (x["R32"]/1000) * omega,
           migration
  ))
 }

#runs
set.seed(100)
runs=ssa.adaptivetau(init.values, transitions, RateF, parameters, tf=3600)


#Create summary measures for plotting
runs <- cbind(runs,
S = rowSums(runs[,c("S11","S12","S21","S22","S31","S32")]),
I = rowSums(runs[,c("I11","I12","I21","I22","I31","I32")]),
R = rowSums(runs[,c("R11","R12","R21","R22","R31","R32")]),
S1 = rowSums(runs[,c("S11","S12")]),
I1 = rowSums(runs[,c("I11","I12")]),
R1 = rowSums(runs[,c("R11","R12")]),
S2 = rowSums(runs[,c("S21","S22")]),
I2 = rowSums(runs[,c("I21","I22")]),
R2 = rowSums(runs[,c("R21","R22")]),
S3 = rowSums(runs[,c("S31","S32")]),
I3 = rowSums(runs[,c("I31","I32")]),
R3 = rowSums(runs[,c("R31","R32")])
)
#plot overall SIR
SIRplot(runs, vars = c("time", "S", "I", "R"), parameters = parameters)
