# data

# Kyrgyzstan data (95.6% vac in 1-14, 95.6% in 15-inf)
kyrgyz <- new.env()
# Model parameters

kyrgyz$init.values <- c(
  S = c(81400, 180700),
  E = c(0, 0),
  I = c(0, 0),
  R = c(1768600, 3926300),
  D = 0 # Begining of vaccination programme
  )

kyrgyz$transitions <- ssa.maketrans(c("S1", "E1", "I1", "R1", "S2", "E2", "I2", "R2"),
  rbind(c("S1", "E1", "I1"), -1, c("E1", "I1", "R1"), +1),
  rbind(c("S1", "R1"), +1),
  rbind(c("S1", "E1", "I1", "R1"), -1, c("S2", "E2", "I2", "R2"), +1),
  rbind(c("S2", "E2", "I2"), -1, c("E2", "I2", "R2"), +1),
  rbind(c("S2", "E2", "I2", "R2"), -1)
  )

# Kyrgyzstan
kyrgyz$parameters <- c(
  R0 = 16, # TODO : Don't use R0
  infectious.period = 7, # days
  latent.period = 8,     # days
  vacc.pro = 0.95,       # proportion vacc at birth
  young.size = 14,       # years in the young compartment
  birth.rate = 25.9,     # per 1000, anum
  death.rate = 5.4       # per 1000, anum
)

kyrgyz$RateF <- function(x, p, t) {
  # Local parameters
  beta <- p["R0"] / (p["infectious.period"])
  f <- 1 / p["latent.period"]
  gamma <- 1 / p["infectious.period"]
  alpha <- p["birth.rate"] / 365
  omega <- p["death.rate"] / 365
  v <- ifelse(t > x["D"], p["vacc.pro"], 0)
  age.out <- 1 / (p["young.size"] * 365)

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

  # Rate Functions
  return(c(S1 * beta * (I / Nt), #young
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

# Sweden
# 2-4 : 1% seronegative
# 5-9 : 5.8%
# 10-19 : 4.7%
# 20-39 : 5.9%
# 40+ : 0.5%

swe <- new.env()

swe$init.values <- c(
  S = c(87805, 176865),
  E = c(0, 0),
  I = c(0, 0),
  R = c(2106625, 7392270),
  D = 0 # Begining of vaccination programme
  )

swe$transitions <- ssa.maketrans(c("S1", "E1", "I1", "R1", "S2", "E2", "I2", "R2"),
  rbind(c("S1", "E1", "I1"), -1, c("E1", "I1", "R1"), +1),
  rbind(c("S1", "R1"), +1),
  rbind(c("S1", "E1", "I1", "R1"), -1, c("S2", "E2", "I2", "R2"), +1),
  rbind(c("S2", "E2", "I2"), -1, c("E2", "I2", "R2"), +1),
  rbind(c("S2", "E2", "I2", "R2"), -1)
  )

swe$parameters <- c(
  R0 = 16,
  infectious.period = 7, # days
  latent.period = 8,     # days
  vacc.pro = 0.94,       # proportion vacc at birth
  young.size = 19,       # years in the young compartment
  birth.rate = 10.8,     # per 1000, anum
  death.rate = 10.5       # per 1000, anum
)

swe$RateF <- function(x, p, t) {
  # Local parameters
  beta <- p["R0"] / (p["infectious.period"])
  f <- 1 / p["latent.period"]
  gamma <- 1 / p["infectious.period"]
  alpha <- p["birth.rate"] / 365
  omega <- p["death.rate"] / 365
  v <- ifelse(t > x["D"], p["vacc.pro"], 0)
  age.out <- 1 / (p["young.size"] * 365)

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

  # Rate Functions
  return(c(S1 * beta * (I / Nt), #young
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
