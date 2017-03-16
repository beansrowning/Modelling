#WHO EURO 1 population measles model
#28/2/2017 Sean Browning
#Kyrgyzstan data (95.6% vac in 1-14, 95.6% in 15-inf)
#Depends: 'adaptivetau', 'googleVis', modelvis.R

if(!require(adaptivetau)){
  install.packages("adaptivetau")
}
suppressPackageStartupMessages(library(adaptivetau))

#Source model visualization
script.dir <- dirname(sys.frame(1)$ofile); source(paste0(script.dir,"/modelvis.R"))

#Defining input
init.values = c(
  S = c(81400,180700),
  E = c(0,0),
  I = c(0,0),
  R = c(1768600,3926300),
  D = 0 #begining of vaccination programme
  )

transitions = ssa.maketrans(c("S1","E1","I1","R1","S2","E2","I2","R2"),
  rbind(c("S1","E1","I1"),-1,c("E1","I1","R1"),+1),
  rbind(c("S1","R1"),+1),
  rbind(c("S1","E1","I1","R1"),-1,c("S2","E2","I2","R2"),+1),
  rbind(c("S2","E2","I2"),-1,c("E2","I2","R2"),+1),
  rbind(c("S2","E2","I2","R2"),-1)
  )

parameters = c( #Kyrgyzstan
  R0 = 16,
  infectious.period = 7, #days
  latent.period = 8, #days
  vacc.pro = 0.95, #proportion vacc at birth
  young.size = 14, #years in the young compartment
  birth.rate = 25.9, #per 1000, anum
  death.rate = 5.4 #per 1000, anum
)

RateF <- function(x, p, t) {
  #local parameters
  beta <- p["R0"]/(p["infectious.period"])
  f <- 1/p["latent.period"]
  gamma <- 1/p["infectious.period"]
  alpha <- p["birth.rate"]/365
  omega <- p["death.rate"]/365
  v <- ifelse(t>x["D"], p["vacc.pro"], 0)
  age.out <- 1/(p["young.size"]*365)
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
  return(c(S1 * beta[1] * (I/Nt), #young
           E1 * f,
           I1 * gamma,
           (Nt/1000) * alpha * (1-v) ,
           (Nt/1000) * alpha * v,
           S1 * age.out,
           E1 * age.out,
           I1 * age.out,
           R1 * age.out,
           S2 * beta * (I/Nt), #old
           E2 * f,
           I2 * gamma,
           (S2/1000) * omega,
           (E2/1000) * omega,
           (I2/1000) * omega,
           (R2/1000) * omega
  ))
 }

#runs
SIR_run <- function(i = init.values, t = transitions, RF = RateF, P = parameters, t_int, i_num,age = "a", tf = 365){
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
    run <<- rbind(run_1,run_2[-1,])
    }
SIR_run(t_int=10,i_num=1) #insert 1 person at time 10
#Summary Measures
run <- cbind(run,
S = rowSums(run[,c("S1","S2")]),
I = rowSums(run[,c("I1","I2")]),
R = rowSums(run[,c("R1","R2")])
)
#Plot
#plotting only the compartment that stores those cases introduced
SIRplot(run, vars = c("time","I"), parameters = parameters)  #"S", "I", "R"
