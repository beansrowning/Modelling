# Sweden
# 2-4 : 1% seronegative
# 5-9 : 5.8%
# 10-19 : 4.7%
# 20-39 : 5.9%
# 40+ : 0.5%
# When weighted to 2015 population averages:
#   4.00% 0-19
#   2.34% 20+
# Current birth / death values reflect 2000-2005 estimates
# Consider 2010-2015 birth/death estimates:
# 9.5 CDR
# 12 CBR
swe <- new.env()

swe$init.values <- c(
  S = c(87805, 176865),
  E = c(0, 0),
  I = c(0, 0),
  R = c(2106625, 7392270)
  )

swe$transitions <- ssa.maketrans(c("S1", "E1", "I1", "R1", "S2", "E2", "I2", "R2"),
  rbind(c("S1", "E1", "I1"), -1, c("E1", "I1", "R1"), +1),
  rbind(c("S1", "R1", "I1", "I2"), +1),
  rbind(c("S1", "E1", "I1", "R1"), -1, c("S2", "E2", "I2", "R2"), +1),
  rbind(c("S2", "E2", "I2"), -1, c("E2", "I2", "R2"), +1),
  rbind(c("S2", "E2", "I2", "R2"), -1)
  )

swe$parameters <- c(
  R0 = 16,
  introduction.rate = 0, # (time to new case introduction)
  infectious.period = 7, # days
  latent.period = 8,     # days
  vacc.pro = 0.94,       # proportion vacc at birth
  young.size = 19,       # years in the young compartment
  birth.rate = 10.8,     # per 1000, anum
  death.rate = 10.5,       # per 1000, anum
  # Let's have some values that can be set by a wrapper function to
  # change how our model funtions, as well.
  introduction.rate = 0, # Some rate of new case introduction
  start.time = 0, # Time between start of sim and new case introduction
  end.time = 0, # Time between end of case introduction and end of sim
  grp.yng = 0, # Bool, will new cases be introduced in this compartment?
  grp.old = 0 # Bool, same as above
)

swe$RateF <- function(x, p, t) {
             # Local parameters
             beta <- p["R0"] / (p["infectious.period"])
             f <- 1 / p["latent.period"]
             gamma <- 1 / p["infectious.period"]
             alpha <- p["birth.rate"] / 365
             omega <- p["death.rate"] / 365
             v <- p["vacc.pro"]
             age.out <- 1 / (p["young.size"] * 365)
             startt <- ifelse(t < p["start.time"], 0, 1)
             endd <- ifelse(t < p["end.time"], 1, 0)
             yng <- p["grp.yng"]
             old <- p["grp.old"]
             int.rate <- p["introduction.rate"]
             # Local population values
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

             return(c(S1 * beta * (I / Nt), # Young values
                      E1 * f,
                      I1 * gamma,
                      (Nt / 1000) * alpha * (1 - v),
                      (Nt / 1000) * alpha * v,
                      # Defining when there will be new case importation :
                      startt * endd * yng * int.rate, # Young
                      startt * endd * old * int.rate, # Old
                      # Aging out
                      S1 * age.out,
                      E1 * age.out,
                      I1 * age.out,
                      R1 * age.out,
                      # Old
                      S2 * beta * (I / Nt),
                      E2 * f,
                      I2 * gamma,
                      # Death
                      (S2 / 1000) * omega,
                      (E2 / 1000) * omega,
                      (I2 / 1000) * omega,
                      (R2 / 1000) * omega
             ))
}




 # Measles Land
 # 90% Seroprevalence
 # 300K population
 measles_land <- new.env()
 measles_land$init.values <- c(
             S = c(4800, 43200),
             E = c(0, 0),
             I = c(0, 0),
             R = c(43200, 226800))

 measles_land$transitions <- ssa.maketrans(c("S1", "E1", "I1", "R1", "S2", "E2", "I2", "R2"),
 rbind(c("S1", "E1", "I1"), -1, c("E1", "I1", "R1"), +1),
 rbind(c("S1", "R1", "I1", "I2"), +1),
 rbind(c("S1", "E1", "I1", "R1"), -1, c("S2", "E2", "I2", "R2"), +1),
 rbind(c("S2", "E2", "I2"), -1, c("E2", "I2", "R2"), +1),
 rbind(c("S2", "E2", "I2", "R2"), -1)
 )

 measles_land$parameters <- c(
   R0 = 16,
   infectious.period = 7, # days
   latent.period = 8,     # days
   vacc.pro = 0.90,       # proportion vacc at birth
   young.size = 14,       # years in the young compartment
   birth.rate = 10.9,     # per 1000, anum
   death.rate = 10.9,       # per 1000, anum
   # Let's have some values that can be set by a wrapper function to
   # change how our model funtions, as well.
   introduction.rate = 0, # Some rate of new case introduction
   start.time = 0, # Time between start of sim and new case introduction
   end.time = 0, # Time between end of case introduction and end of sim
   grp.yng = 0, # Bool, will new cases be introduced in this compartment?
   grp.old = 0 # Bool, same as above
 )
 measles_land$RateF <- function(x, p, t) {
              # Local parameters
              beta <- p["R0"] / (p["infectious.period"])
              f <- 1 / p["latent.period"]
              gamma <- 1 / p["infectious.period"]
              alpha <- p["birth.rate"] / 365
              omega <- p["death.rate"] / 365
              v <- p["vacc.pro"]
              age.out <- 1 / (p["young.size"] * 365)
              startt <- ifelse(t < p["start.time"], 0, 1)
              endd <- ifelse(t < p["end.time"], 1, 0)
              yng <- p["grp.yng"]
              old <- p["grp.old"]
              int.rate <- p["introduction.rate"]
              # Local population values
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

              return(c(S1 * beta * (I / Nt), # Young values
                       E1 * f,
                       I1 * gamma,
                       (Nt / 1000) * alpha * (1 - v),
                       (Nt / 1000) * alpha * v,
                       # Defining when there will be new case importation :
                       startt * endd * yng * int.rate, # Young
                       startt * endd * old * int.rate, # Old
                       # Aging out
                       S1 * age.out,
                       E1 * age.out,
                       I1 * age.out,
                       R1 * age.out,
                       # Old
                       S2 * beta * (I / Nt),
                       E2 * f,
                       I2 * gamma,
                       # Death
                       (S2 / 1000) * omega,
                       (E2 / 1000) * omega,
                       (I2 / 1000) * omega,
                       (R2 / 1000) * omega
              ))
}
