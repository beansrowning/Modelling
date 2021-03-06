# Benchmarking some code streamlining in the Multicore
# batch plot.

#Depends
require("adaptivetau")
require("microbenchmark")
require("ggplot2")
source("multicore.R")
source("multicore_2.R")

# Model parameters 

init.values <- c(
  S = c(81400, 180700),
  E = c(0, 0),
  I = c(0, 0),
  R = c(1768600, 3926300),
  D = 0 #begining of vaccination programme
  )

transitions <- ssa.maketrans(c("S1", "E1", "I1", "R1", "S2", "E2", "I2", "R2"),
  rbind(c("S1", "E1", "I1"), -1, c("E1", "I1", "R1"), +1),
  rbind(c("S1", "R1"), +1),
  rbind(c("S1", "E1", "I1", "R1"), -1, c("S2", "E2", "I2", "R2"), +1),
  rbind(c("S2", "E2", "I2"), -1, c("E2", "I2", "R2"), +1),
  rbind(c("S2", "E2", "I2", "R2"), -1)
  )

parameters <- c( #Kyrgyzstan
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
  beta <- p["R0"] / (p["infectious.period"])
  f <- 1 / p["latent.period"]
  gamma <- 1 / p["infectious.period"]
  alpha <- p["birth.rate"] / 365
  omega <- p["death.rate"] / 365
  v <- ifelse(t > x["D"], p["vacc.pro"], 0)
  age.out <- 1 / (p["young.size"] * 365)
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
  return(c(S1 * beta[1] * (I / Nt), #young
           E1 * f,
           I1 * gamma,
           (Nt / 1000) * alpha * (1 - v),
           (Nt / 1000) * alpha * v,
           S1 * age.out,
           E1 * age.out,
           I1 * age.out,
           R1 * age.out,
           S2 * beta * (I / Nt), #old
           E2 * f,
           I2 * gamma,
           (S2 / 1000) * omega,
           (E2 / 1000) * omega,
           (I2 / 1000) * omega,
           (R2 / 1000) * omega
  ))
 }
 
set.seed(1234)

bench <<- microbenchmark(streamlined = batch_plot_mc2(batch = 10000,
                                       grp = "a",
                                       insertion = 10,
                                       i_number = 20),
                        base = batch_plot_mc(batch = 10000,
                                      grp = "a",
                                      insertion = 10,
                                      i_number = 20), times = 100)


autoplot(bench)