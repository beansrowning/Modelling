# Working through this
# I'll attempt to reason out all the assumptions I've made and my process behind it.
# First start out with the packages we will need:
require(adaptivetau)
require(data.table)
require(Rcpp)
require(parallel)
require(doParallel)
require(foreach)
# Let's create a model with some initial values that would be reasonable
# And let's have that data all in an environment so it can be passed easily to
# various functions and saved after we're done.
# Let's name our hypothetical country, measles_land
measles_land <- new.env()

# It will be an SEIR model, and we will start with none infected.
# Let's just start with a pretty low population estimates by looking at
# countries within the EU and surrounding area

# Deciding base population size :
# --------------------------------
# From UN DESA, two of the smallest countries in the European area:
#   ~ 340,000 Iceland
#   ~ 570,000 Luxembourg
# Let's start small at 300,000 and go from there

# Deciding Young/Old compartment divisions :
# ------------------------------------------
# Also from UN DESA ~16% of the population of Europe is under 15
# Why under 15? Well, it comes directly from DESA, which is convient.
# It might also be aruged that 0-14 would be less likely to travel about
# and have different contact patterns than the older group.
# Let's go with that.

# Deciding S / I division (Seroprevalence) :
# ------------------------------------------
# The S/I division is more tricky.
# Firstly, the measles seroprevalence data I have access to is either old or
# based on quite small sample sizes. However, the data which is available shows
# that most countries have ~90% or greater seroprevalence.
# Let's assume a worst case of 90%

# With that, we should be able to generate a starting model population.
# -------------------------------------
# 300,000 * 16% = 48,000  under 15
# leaving ...     252,000 15 or older
# Of those 48,000 in the "Young" compartment:
#   4,800 (10%) would be susceptible
#   43,200 (90%) would be immune
# Of those 252,000 in the "Old" compartment:
#   25,200 (10%) would be susceptible
#   226,800 (90%) would be immune

# And now we can fill this out :
measles_land$init.values <- c(
            S = c(4800, 43200),
            E = c(0, 0),
            I = c(0, 0),
            R = c(43200, 226800))

# There should also be a list of transitions that go along with the SEIR model :
measles_land$transitions <- ssa.maketrans(c("S1", "E1", "I1", "R1", "S2", "E2", "I2", "R2"),
rbind(c("S1", "E1", "I1"), -1, c("E1", "I1", "R1"), +1),
rbind(c("S1", "R1", "I1", "I2"), +1),
rbind(c("S1", "E1", "I1", "R1"), -1, c("S2", "E2", "I2", "R2"), +1),
rbind(c("S2", "E2", "I2"), -1, c("E2", "I2", "R2"), +1),
rbind(c("S2", "E2", "I2", "R2"), -1)
)

# Now that that's settled, we can deal with the parameters for the model:
# ---------------------------------------

# Deciding R0 :
# -------------
# In lieu of using different mixing patterns, using R0 is much more straightforward
# The high end of R0 for measles is around 16.
# Let's assume the worst here.

# Deciding infectious period / latent period :
# ------------------------------------------
# I'll just use the textbook infectious period and latent period for measles:
# 7 days, and 8 days repectively.

# Effective Vaccination Rate :
# ----------------------------
# This is something we will want to change, but it's probably easier to assume it
# is the same as the seroprevalence value, 90%.

# Deciding Birth and Death Rate :
# -------------------------------
# From UN DESA, the European average Crude Death Rate in 2015 was 10.9 per 1000.
# It makes sense that we would want a country that increases in population.
# For now, however, let's just use that crude measure for both as a starting place.

# And now we can put that in a vector, also :
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

# Now comes the rate function.
# This will simply represent rate of our transistions
# in addition to having some logic to handle a few different modelling scenarios.
# It's good to just have this logic in from the beginning so we don't have to change
# anything when we want to look at different questions (how long before a country
# becomes at risk again, does this country have 0 measles transmission at 36 months
# following case importation, etc.)

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

# Now we have our model.
# Let's run it through a few diagnostics to make sure it's all up and running
# before we do anything too advanced:
# No new cases :
trial_run <- ssa.adaptivetau(measles_land$init.values, measles_land$transitions,
                            measles_land$RateF, measles_land$parameters, 365)
str(trial_run)
head(trial_run)
tail(trial_run)
Sys.sleep(10)
# with new case introduction :
# both age groups, starting at time 0, ending at 365, overrunning until 1000
# all of these assignments will be done by a function, but we can always do it
# by hand as well :
measles_land$parameters["introduction.rate"] <- 0.01 # ~1 per 100 days
measles_land$parameters["start.time"] <- 0
measles_land$parameters["end.time"] <- 365
measles_land$parameters["grp.yng"] <- 1
measles_land$parameters["grp.old"] <- 1
trial_run <- ssa.adaptivetau(measles_land$init.values, measles_land$transitions,
measles_land$RateF, measles_land$parameters, 1000)
str(trial_run)
head(trial_run)
tail(trial_run)
Sys.sleep(10)
# Now just with case introduction in the old compartment, waiting one year before
# new case introduction, and still going until 1000 :
measles_land$parameters["grp.yng"] <- 0
measles_land$parameters["start.time"] <- 365
measles_land$parameters["end.time"] <- 730
trial_run <- ssa.adaptivetau(measles_land$init.values, measles_land$transitions,
measles_land$RateF, measles_land$parameters, 1000)
str(trial_run)
head(trial_run)
tail(trial_run)
Sys.sleep(10)
# If all that went well, let's delete that data and reset those values :
measles_land$parameters["introduction.rate"] <- 0
measles_land$parameters["start.time"] <- 0
measles_land$parameters["end.time"] <- 0
measles_land$parameters["grp.yng"] <- 0
measles_land$parameters["grp.old"] <- 0
rm(trial_run)
Sys.sleep(10)
# Now to the question at hand : What is the smallest population size which can
# sustain no measles transmission following case importation at 12 mo, 24 mo, 36 mo.

# Let's start at 12.
# -------------------

# Now we will want to consider a grid space comprising different population sizes
# on one axis, and some other variable on the other, with the third, z axis, being
# the maximum outbreak that results from those given inputs.

# There are a few different options for what that "other variable" could be, but
# we might start with the effective vaccination rate in the model, as it is expected
# that this has some interplay with the relationship being explored. Next might
# be birth / death rate, which would require that these be different.

# A whole grid search routine without any hueristics is very time consuming, and we
# might find that the answer lies outside of the bounded search area we provide.
# To attempt to speed up that process, we will bound our population size parameter
# at 300,000 - 500,000, with a search resolution (or step) of 10,000.

# Since we were already considering 90% vaccination coverage to start with, let's
# bound that dimension from 0.90 to 1 (perfect coverage), with a search resolution
# of 0.01.

# There is a small issue, however: because we want to hold those previous population
# characteristics constant for now, we need a way to increment whole population size
# and automatically generate the initial values for the model and assign them accordingly
# The current grid search function I have written does not currently have that
# mechanism built in, so we will have to re-write it with that aim in mind.
# There are a few options here :

# 1. Incorperate this mechanic into the modelling subroutine of the function
# 2. Write a new subroutine to do this and call it before the modelling subroutine
#   in the loop.
# 3. Forgo looping over an additional function call, and calculate the values prior
#   to the loop, and then assign them with logic using the loop iterator.

# I'm not a computer science major, and I'm really tired of doing unittesting.
# I'm going to make an educated guess that a single function call to calculate those
# values is better than (x*y) additional function calls in a nested loop, though this
# would probably have a larger memory requirement (O(n) vs. O(1) I think).

# So let's just make a single function now to accomplish that and write in a call
# to that function at the beginning of a copy of that existing function.
# Let's also add some arguments to it that allow us to extend it to other situations
# we might want to look at down the road (different seroprevalence, different age
# compartments, etc.)

get_popvalues <- function(vec, young = 0.16, sero.p = c(0.9,0.9)) {
  # This routine creates the inital values for our SEIR model
  # by taking in the raw population number in vector format
  # and outputting a vector containing the values of the S and R
  # compartments of both populations for each value i the vector
  # Args :
  #   vec   : (Numeric Vector) of total population values being searched for
  #   young : (int) of the percentage comprising the young compartments of the model
  #   sero.p: (Numeric Vector) of the seroprevalence in the young and old compartments
  # Returns :
  #   popvalues : (Numeric Vector) containg all values of S and R to be assigned in the search

  #---Skipping some of the checks, don't break it-----------------
  stopifnot(is.vector(vec), length(young) == 1,
            is.vector(sero.p) && length(sero.p) == 2)
  #---Initialize--------------------------------------------------
  out <- list()
  #---...and make the list----------------------------------------
  for (i in 1:length(vec)) {
    out[[i]] <- list(S1 = vec[i] * young * (1 - sero.p[1]),
                     S2 = vec[i] * (1 - young) * (1 - sero.p[2]),
                     R1 = vec[i] * young * sero.p[1],
                     R2 = vec[i] * (1 - young) * sero.p[2])
  }
  #---Return to parent frame for access in a function-------------
  popvalues <<- out
}

# with that, we should be good to go. Doesn't hurt to test it at least once though.
get_popvalues(c(300000, 310000, 320000, 330000))
print(popvalues)
print(popvalues[[1]])
print(popvalues[[1]]$S2)
Sys.sleep(10)
# Wow. I can't believe I got that working the first go.
rm(popvalues)
# I took the liberty of fixing up the gridsearch function to work with this routine
# and now we can load that up and begin.
source("gridsearch1.R")

# Let's remember to set the value of the introduction rate and the start.Time
# we want for each search, else we're going to end up with a bunch of empty data!
measles_land$parameters["start.time"] <- 0

# Run 1
#-------
# Testing initial population size and effective vaccination rate on outbreak length
# Population sizes : 300,000 - 500,000 by 10,000
# Vaccination rates : 0.90 - 1 by 0.01
# Case introduction rate : 0.01 (approximately 1 per 100 days)
# Equal introduction likelihood in either group
# Total grid area: 21 x 11 = 231
print(paste0("Beinging Run 1 - ", date()))
measles_land$parameters["introduction.rate"] <- 0.01
measles_land$t1 <- system.time(measles_land$run_1 <- solutionSpace(measles_land,
                                    insbound = seq(300000, 500000, 10000),
                                    vaccbound = c(0.9, 0.91, 0.92, 0.93, 0.94,
                                                  0.95, 0.96, 0.97, 0.98, 0.99, 1),
                                    len = 365,
                                    # And just to be on the /really/ safe side...
                                    offset = 800))
print(paste0("Run 1 done - ", measles_land$t1))

# Run 2
#-------
# Testing initial population size and effective vaccination rate on outbreak length
# Population sizes : 300,000 - 500,000 by 10,000
# Vaccination rates : 0.90 - 1 by 0.01
# Case introduction rate : 0.05 (approximately 1 per 20 days)
# Equal introduction likelihood in either group
# Total grid area: 21 x 11 = 231
print(paste0("Beinging Run 2 - ", date()))
measles_land$parameters["introduction.rate"] <- 0.05
measles_land$t2 <- system.time(measles_land$run_2 <- solutionSpace(measles_land,
                                    insbound = seq(300000, 500000, 10000),
                                    vaccbound = c(0.9, 0.91, 0.92, 0.93, 0.94,
                                                  0.95, 0.96, 0.97, 0.98, 0.99, 1),
                                    len = 365,
                                    # And just to be on the /really/ safe side...
                                    offset = 800))
print(paste0("Run 2 done - ", measles_land$t2))

# Run 3
#-------
# Testing initial population size and effective vaccination rate on outbreak length
# Population sizes : 300,000 - 500,000 by 10,000
# Vaccination rates : 0.90 - 1 by 0.01
# Case introduction rate : 0.1 (approximately 1 per 10 days)
# Equal introduction likelihood in either group
# Total grid area: 21 x 11 = 231
print(paste0("Beinging Run 3 - ", date()))
measles_land$parameters["introduction.rate"] <- 0.1
measles_land$t3 <- system.time(measles_land$run_3 <- solutionSpace(measles_land,
                                    insbound = seq(300000, 500000, 10000),
                                    vaccbound = c(0.9, 0.91, 0.92, 0.93, 0.94,
                                                  0.95, 0.96, 0.97, 0.98, 0.99, 1),
                                    len = 365,
                                    # And just to be on the /really/ safe side...
                                    offset = 800))
print(paste0("Run 3 done - ", measles_land$t3))

# let's save our progress and be done for the night (or the morning as it were)
print(paste0("All Done! - ", date()))
save(measles_land, file="../../Data/gridsearch_part1.dat")
