# All models assume highest Seroprevalence


# Sweden
# 2-4 : 1% seronegative
# 5-9 : 5.8%
# 10-19 : 4.7%
# 20-39 : 5.9%
# 40+ : 0.5%
# Assuming 5.9% 0-39, 0.5% 40+
# Consider 2010-2015 birth/death estimates:
# 9.5 CDR
# 12 CBR
swe <- new.env()

swe$init.values <- c(
  S = c(281363, 24973),
  E = c(0, 0),
  I = c(0, 0),
  R = c(4487507, 4969722)
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
  infectious.period = 7, # days
  latent.period = 8,     # days
  vacc.pro = 0.94,       # proportion vacc at birth
  young.size = 39,       # years in the young compartment
  birth.rate = 12,     # per 1000, anum
  death.rate = 9.5,       # per 1000, anum
  # Let's have some values that can be set by a wrapper function to
  # change how our model funtions, as well.
  introduction.rate = 0, # Some rate of new case introduction
  start.time = 0, # Time between start of sim and new case introduction
  end.time = 0, # Time between start of sim and end of case introduction period
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
                      (R2 / 1000) * omega))
}

# Latvia
# Relatively high seronegative population
# 2-4 yr : 19%
# 5-9 : 42.9%
# 10-19 : 39.8%
# 20-39 : 30.8%
# 40+ : 3.5%
# Assuming 42.9% in 0-39, 3.5% in 40+
# Measles eliminated
# 39 years in the young compartment
# 2015 Vital dynamics :
# CBR : 10.2
# CDR : 14.3

latvia <- new.env()

latvia$init.values <- c(
  S = c(397376, 37323),
  E = c(0, 0),
  I = c(0, 0),
  R = c(528909, 1029055)
  )

latvia$transitions <- ssa.maketrans(c("S1", "E1", "I1", "R1", "S2", "E2", "I2", "R2"),
  rbind(c("S1", "E1", "I1"), -1, c("E1", "I1", "R1"), +1),
  rbind(c("S1", "R1", "I1", "I2"), +1),
  rbind(c("S1", "E1", "I1", "R1"), -1, c("S2", "E2", "I2", "R2"), +1),
  rbind(c("S2", "E2", "I2"), -1, c("E2", "I2", "R2"), +1),
  rbind(c("S2", "E2", "I2", "R2"), -1)
  )

latvia$parameters <- c(
  R0 = 16,
  infectious.period = 7, # days
  latent.period = 8,     # days
  vacc.pro = 0.94,       # proportion vacc at birth
  young.size = 39,       # years in the young compartment
  birth.rate = 10.2,     # per 1000, anum
  death.rate = 14.3,       # per 1000, anum
  # Let's have some values that can be set by a wrapper function to
  # change how our model funtions, as well.
  introduction.rate = 0, # Some rate of new case introduction
  start.time = 0, # Time between start of sim and new case introduction
  end.time = 0, # Time between start of sim and end of case introduction period
  grp.yng = 0, # Bool, will new cases be introduced in this compartment?
  grp.old = 0 # Bool, same as above
)

latvia$RateF <- function(x, p, t) {
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
                      (R2 / 1000) * omega))
}

# Malta
# Intermediate Susceptibility
# Reported Seronegativity :
# 2-4 : 9.6
# 5-9 : 4.3
# 10-19 : 6.9
# 20-39 : 5.3
# 40+ : 3.1
# Assuming 9.6% for 0-39, and 3.1% for 40+
# Meales Eliminated by the WHO
# 2015 Vital Dynamics :
# CBR : 9.7
# CDR : 8.7

malta <- new.env()

malta$init.values <- c(
  S = c(20025, 6790),
  E = c(0, 0),
  I = c(0, 0),
  R = c(188572, 212229)
  )

malta$transitions <- ssa.maketrans(c("S1", "E1", "I1", "R1", "S2", "E2", "I2", "R2"),
  rbind(c("S1", "E1", "I1"), -1, c("E1", "I1", "R1"), +1),
  rbind(c("S1", "R1", "I1", "I2"), +1),
  rbind(c("S1", "E1", "I1", "R1"), -1, c("S2", "E2", "I2", "R2"), +1),
  rbind(c("S2", "E2", "I2"), -1, c("E2", "I2", "R2"), +1),
  rbind(c("S2", "E2", "I2", "R2"), -1)
  )

malta$parameters <- c(
  R0 = 16,
  infectious.period = 7, # days
  latent.period = 8,     # days
  vacc.pro = 0.94,       # proportion vacc at birth
  young.size = 39,       # years in the young compartment
  birth.rate = 9.7,     # per 1000, anum
  death.rate = 8.7,       # per 1000, anum
  # Let's have some values that can be set by a wrapper function to
  # change how our model funtions, as well.
  introduction.rate = 0, # Some rate of new case introduction
  start.time = 0, # Time between start of sim and new case introduction
  end.time = 0, # Time between start of sim and end of case introduction period
  grp.yng = 0, # Bool, will new cases be introduced in this compartment?
  grp.old = 0 # Bool, same as above
)

malta$RateF <- function(x, p, t) {
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
                      (R2 / 1000) * omega))
}
