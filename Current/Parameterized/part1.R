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
#Sys.sleep(10)
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
#Sys.sleep(10)
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
#Sys.sleep(10)
# If all that went well, let's delete that data and reset those values :
measles_land$parameters["introduction.rate"] <- 0
measles_land$parameters["start.time"] <- 0
measles_land$parameters["end.time"] <- 0
measles_land$parameters["grp.yng"] <- 0
measles_land$parameters["grp.old"] <- 0
rm(trial_run)
#Sys.sleep(10)
# Now to the question at hand : What is the smallest population size which can
# sustain no measles transmission following case importation at 12 mo, 24 mo, 36 mo.

# Let's start at 12. 
# (we can actually look at all of them by running the model longer)
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
  # and outputting a list containing the values of the S and R
  # compartments of both populations for each value i the vector
  # Args :
  #   vec   : (Numeric Vector) of total population values being searched for
  #   young : (int) the percentage comprising the young compartments of the model
  #   sero.p: (Numeric Vector) of the seroprevalence in the young and old compartments
  # Returns :
  #   out : (list) containg all values of S and R to be assigned in the search

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
  return(out)
}

# with that, we should be good to go. Doesn't hurt to test it at least once though.
popvalues <- get_popvalues(c(300000, 310000, 320000, 330000))
print(popvalues)
print(popvalues[[1]])
print(popvalues[[1]]$S2)
#Sys.sleep(10)
# Wow. I can't believe I got that working on the first go.
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
print(paste0("Begining Run 1 - ", date()))
measles_land$parameters["introduction.rate"] <- 0.01
measles_land$t1 <- system.time(measles_land$run_1 <- solutionSpace(measles_land,
                                    insbound = seq(300000, 500000, 10000),
                                    vaccbound = c(0.9, 0.91, 0.92, 0.93, 0.94,
                                                  0.95, 0.96, 0.97, 0.98, 0.99, 1),
                                    len = 365,
                                    # And just to be on the /really/ safe side...
                                    offset = 1200))
print(paste0("Run 1 done - ", measles_land$t1))

# as It's now going on the 9th hour waiting for this to finish, and I had
# issues with outbreaks overrunning the sim with the offset at 800, I think the
# other two models upping the introduction rate are overkill.

# What is probably better is to up the seoprevalence estimate in both groups
# by some small amount to see how that affects the output. Hopefully it will not
# take nearly as long

# I also had to update the gridsearch function to pass more arguments to the
# get_popvalues() function internally. That way we can change things like
# baseline seroprevalence from one call.

# Run 2
#-------
# Testing initial population size and effective vaccination rate on outbreak length
# Population sizes : 300,000 - 500,000 by 10,000
# Vaccination rates : 0.90 - 1 by 0.01
# Baseline Seroprevalence : 92%
# Case introduction rate : 0.01 (approximately 1 per 100 days)
# Equal introduction likelihood in either group
# Total grid area: 21 x 11 = 231
print(paste0("Begining Run 2 - ", date()))
measles_land$parameters["introduction.rate"] <- 0.01
measles_land$t2 <- system.time(measles_land$run_2 <- solutionSpace(measles_land,
                                    insbound = seq(300000, 500000, 10000),
                                    vaccbound = c(0.9, 0.91, 0.92, 0.93, 0.94,
                                                  0.95, 0.96, 0.97, 0.98, 0.99, 1),
                                    len = 365,
                                    # let's try 800 again
                                    offset = 800,
                                    sero.p = c(0.92, 0.92)))
print(paste0("Run 2 done - ", measles_land$t2))

# ...and as it turns out, that fails on the first trial - 300,000 with 0.90%
# Further analysis shows that ~1000 still had infected at the last time point
# (day 1165), indicating almost certain endemic spread occuring in 10% of runs.

# Let's move on up to 96% baseline seroprevalence and see how that works

# Run 3
#-------
# Testing initial population size and effective vaccination rate on outbreak length
# Population sizes : 300,000 - 500,000 by 10,000
# Vaccination rates : 0.90 - 1 by 0.01
# Baseline Seroprevalence : 96%
# Case introduction rate : 0.01 (approximately 1 per 100 days)
# Equal introduction likelihood in either group
# Total grid area: 21 x 11 = 231
print(paste0("Begining Run 3 - ", date()))
measles_land$parameters["introduction.rate"] <- 0.01
measles_land$t3 <- system.time(measles_land$run_3 <- solutionSpace(measles_land,
                                    insbound = seq(300000, 500000, 10000),
                                    vaccbound = c(0.9, 0.91, 0.92, 0.93, 0.94,
                                                  0.95, 0.96, 0.97, 0.98, 0.99, 1),
                                    len = 365,
                                    # let's try 800 again
                                    offset = 800,
                                    sero.p = c(0.96, 0.96)))
print(paste0("Run 3 done - ", measles_land$t3))

# let's save our progress and be done
print(paste0("All Done! - ", date()))
save(measles_land, file="../../Data/gridsearch_part1.dat")

# This is actually looking more reasoable...
# Now there's a greater question : is the max of the 10000 runs the correct value?
# or is an average / mode a better estimate? 
# perhaps we can just alter the `ed_sub()` function to append both. This shouldn't
# add too much computation time

# Re-run in progress. Then we can overlay both of the results and get a rough
# idea of what to expect looking forward. It certainly seems like there is
# potential for ~12 mo outbreaks given these parameters around the 330,000 area

# I currently have two grid searches running : 
# 1. A repeat of the last one which will also append the median (LG30)
# 2. The same gridspace at 95% seroprevalence instead (in the cluster)

# Pending the results of those searches, I can narrow in on an appropriate population size
# ...cluster run failed at 800 offset, increasing to 1200...

# Data from the first grid search is in, and we can see some very large
# large differences between maximum and median outbreak times. 
# how do we then define how likely a country is to experience an outbreak ?

# Maybe by counting the number of iterations that went above whatever threshold?
# Then again, the question posed was "will not experience and outbreak"

# So let's compromise and say that we accept the maximum in 10,000. But we might
# need to have some measure of the probability that this occurs (p value, etc).


# By the end of the night, I should have a dat file with the measles_land environment
# along with the following models :
# 1. Run 1 - 0.90% seroprevalence - Done (In dat file) 2000 depth
# 2. Run 2 - 0.92% seroprevalence - Done (In dat file) 2000 depth
# 3. Run 3 - 0.94% seroprevalence - Done (In dat file) 10000 depth
# 4. Run 4 - 0.95% seroprevalence - Done (In dat file) 10000 depth
# 5. Run 5 - 0.96% seroprevalence - Done (In dat file) 10000 depth


# We will also want to look at more real world countries in Europe to get an
# idea for how they might look under new case importation
# Let's start with Sweden since we already have the data for that

# Taking data from UN DESA in 2015: 
# 1,688,811 Below the age of 15
# 8,074,754 15+
# 
# we will want to vary two things : effective vaccination rate, and case introduction rate
# We can also look at weighting how likely an old/young case gets inserted into the model 
# Let's look at a smaller area this time.

# We will run this in the cluster, and have to edit our gridsearch function to 
# look back at the parameters we specify, rather than the ones we did earlier. 
# This is called `gridsearch2_mpi.R` in the Cluster directory.

# Run 1
# Insertion rates :   0.01-0.1 
# vaccinations rates: 0.9-1
# Total grid size : 10 * 11 = 110
# Search depth : 10,000 runs
# All other population values fixed
# Case introduction over the course of 1 year
# Offest by 2000 days beyond
source("../../Data/model_global.R")
solutions <- new.env()
print(paste0("Begining Run 1 - ", date()))
solutions$t1 <- system.time(solutions$run_1 <- solutionSpace(swe,
                                    insbound = c(0.01, 0.02, 0.03, 0.04, 0.05,
                                                 0.06, 0.07, 0.08, 0.09, 0.1),
                                    vaccbound = c(0.9, 0.91, 0.92, 0.93, 0.94,
                                                  0.95, 0.96, 0.97, 0.98, 0.99, 1),
                                    len = 365,
                                    offset = 2000))
print(paste0("Run 1 done - ", solutions$t1[3]))
save(solutions, file = "../../Data/gridsearch2.dat")
# Run 2
# Insertion rates :   0.01-0.1 
# vaccinations rates: 0.9-1
# Total grid size : 10 * 11 = 110
# Search depth : 10,000 runs
# All other population values fixed
# Old persons twice as likely to be introduced
# Case introduction over the course of 1 year
# Offest by 2000 days beyond
print(paste0("Begining Run 2 - ", date()))
solutions$t2 <- system.time(solutions$run_2 <- solutionSpace(swe,
                                    insbound = c(0.01, 0.02, 0.03, 0.04, 0.05,
                                                 0.06, 0.07, 0.08, 0.09, 0.1),
                                    vaccbound = c(0.9, 0.91, 0.92, 0.93, 0.94,
                                                  0.95, 0.96, 0.97, 0.98, 0.99, 1),
                                    len = 365,
                                    grp = c(0.5, 1),
                                    offset = 2000))
print(paste0("Run 2 done - ", solutions$t2[3]))
save(solutions, file = "../../Data/gridsearch2.dat")

# Run 3
# Insertion rates :   0.01-0.1 
# vaccinations rates: 0.9-1
# Total grid size : 10 * 11 = 110
# Search depth : 10,000 runs
# All other population values fixed
# young persons twice as likely to be introduced
# Case introduction over the course of 1 year
# Offest by 2000 days beyond
print(paste0("Begining Run 3 - ", date()))
solutions$t3 <- system.time(solutions$run_3 <- solutionSpace(swe,
                                    insbound = c(0.01, 0.02, 0.03, 0.04, 0.05,
                                                 0.06, 0.07, 0.08, 0.09, 0.1),
                                    vaccbound = c(0.9, 0.91, 0.92, 0.93, 0.94,
                                                  0.95, 0.96, 0.97, 0.98, 0.99, 1),
                                    len = 365,
                                    grp = c(1, 05),
                                    offset = 2000))
print(paste0("Run 2 done - ", solutions$t2[3]))
save(solutions, file = "../../Data/gridsearch2.dat")
print(paste0("All done - ", date()))

# And now we're back...with all the data that we needed up until this point as well!
# Let's recap what we did: 
# We took a somewhat open ended question: 
# what is the largest population size that can avoid a measles outbreak lasting 
# longer than x many months following new case introduction?
#
# and we approached that in two ways: 
# 1. Fixing our introduction rate, and grid searching population size and MMR vacc rate
# 2. Fixing our population size (to Sweden), and grid searching introduction rate and  MMR vacc rate
# 
# In both of these scenarios, we already did some sensitivity analyses by default:
# 1. We looked at how outbreak length would vary given different baseline seroprevalence estimates
# 2. We looked at how changing who is more likely to be introduced as a case (young or old) affected it
# 
# It's also of note that we made the young compartment larger in the Sweden Model
# (19 years v. 15 for the first one)
#
# What we didn't consider already was the following:
# 1. How might the results be different with different introduction rates / 
#    weighting who gets introduced more frequently / 
#    different seroprevalence for young and old / Increasing or decreasing population dynamics
# 2. DIfferent seroprevalence values / different R0 / Different CBR and CDR

# So we might as well do some of that now and see how/if that changes things
# but maybe we don't run it on all of runs we just did, yeah? Just a few. 
# Let's also split the grid in half so we can make them run a bit faster on a cluster
#
# For the first approach: 
# Let's test the sensitivity of the results if we did case introduction weighting 
# like we had in the second one.
# Since it was seen that 92% baseline seroprevalence had the highest outcomes 
# we will start with this one. If the results suggest significant differences
# we can always do other ones as well.

# Run 2 mod 1 pt 1
#-------
# Testing sensitivity to more of the older population being measles cases
# Population sizes : 300,000 - 390,000 by 10,000
# Vaccination rates : 0.90 - 1 by 0.01
# Baseline Seroprevalence : 92%
# Case introduction rate : 0.01 (approximately 1 per 100 days)
# Equal introduction likelihood in either group
# Total grid area: 10 x 11 = 110
print(paste0("Begining Run 1 - ", date()))
measles_land$parameters["introduction.rate"] <- 0.01
measles_land$t2_1 <- system.time(measles_land$run_2_1 <- solutionSpace(measles_land,
                                    count = 2000,
                                    insbound = seq(300000, 390000, 10000),
                                    vaccbound = c(0.9, 0.91, 0.92, 0.93, 0.94,
                                                  0.95, 0.96, 0.97, 0.98, 0.99, 1),
                                    len = 365,
                                    offset = 2000,
                                    grp = c(0.5, 1),
                                    sero.p = c(0.92, 0.92)))
print(paste0("Run 1 done - ", measles_land$t2_1[3]))
save(measles_land, file="../../Data/worker1.dat")

# Run 2 mod 1 pt 2
#-------
# Testing sensitivity to more of the older population being measles cases
# Population sizes : 400,000 - 500,000 by 10,000
# Vaccination rates : 0.90 - 1 by 0.01
# Baseline Seroprevalence : 92%
# Case introduction rate : 0.01 (approximately 1 per 100 days)
# Equal introduction likelihood in either group
# Total grid area: 11 x 11 = 121
print(paste0("Begining Run 1 - ", date()))
measles_land$parameters["introduction.rate"] <- 0.01
measles_land$t2_2 <- system.time(measles_land$run_2_2 <- solutionSpace(measles_land,
                                    count = 2000,
                                    insbound = seq(400000, 500000, 10000),
                                    vaccbound = c(0.9, 0.91, 0.92, 0.93, 0.94,
                                                  0.95, 0.96, 0.97, 0.98, 0.99, 1),
                                    len = 365,
                                    offset = 2000,
                                    grp = c(0.5, 1),
                                    sero.p = c(0.92, 0.92)))
print(paste0("Run 1 done - ", measles_land$t2_2[3]))
save(measles_land, file="../../Data/worker2.dat")


# Run 2 mod 2 pt 1
#-------
# Testing sensitivity to more of the younger population being measles cases
# Population sizes : 300,000 - 390,000 by 10,000
# Vaccination rates : 0.90 - 1 by 0.01
# Baseline Seroprevalence : 92%
# Case introduction rate : 0.01 (approximately 1 per 100 days)
# Equal introduction likelihood in either group
# Total grid area: 10 x 11 = 110
print(paste0("Begining Run 2 - ", date()))
measles_land$parameters["introduction.rate"] <- 0.01
measles_land$t2_21 <- system.time(measles_land$run_2_21 <- solutionSpace(measles_land,
                                    # Reduced search depth:
                                    count = 2000,
                                    insbound = seq(300000, 390000, 10000),
                                    vaccbound = c(0.9, 0.91, 0.92, 0.93, 0.94,
                                                  0.95, 0.96, 0.97, 0.98, 0.99, 1),
                                    len = 365,
                                    offset = 2000,
                                    grp = c(1, 0.5),
                                    sero.p = c(0.92, 0.92)))
print(paste0("Run 2 done - ", measles_land$t2_21[3]))
save(measles_land, file="../../Data/worker1.dat")

# Run 2 mod 2 pt 2
#-------
# Testing sensitivity to more of the younger population being measles cases
# Population sizes : 400,000 - 500,000 by 10,000
# Vaccination rates : 0.90 - 1 by 0.01
# Baseline Seroprevalence : 92%
# Case introduction rate : 0.01 (approximately 1 per 100 days)
# Equal introduction likelihood in either group
# Total grid area: 11 x 11 = 121
print(paste0("Begining Run 2 - ", date()))
measles_land$parameters["introduction.rate"] <- 0.01
measles_land$t2_22 <- system.time(measles_land$run_2_22 <- solutionSpace(measles_land,
                                    # Reduced search depth:
                                    count = 2000,
                                    insbound = seq(400000, 500000, 10000),
                                    vaccbound = c(0.9, 0.91, 0.92, 0.93, 0.94,
                                                  0.95, 0.96, 0.97, 0.98, 0.99, 1),
                                    len = 365,
                                    offset = 2000,
                                    grp = c(1, 0.5)
                                    sero.p = c(0.92, 0.92)))
print(paste0("Run 2 done - ", measles_land$t2_22[3]))
save(measles_land, file="../../Data/worker2.dat")

# Next, Let's look at how heterogeny of seroprevalence estimates of the two groups
# might make a difference. Let's do the same 92% seroprevalence, but toggle having
# One at 94% instead :

# Run 2 mod 3 pt 1
#-------
# Testing sensitivity to higher proportion of younger population seronegative
# Population sizes : 300,000 - 390,000 by 10,000
# Vaccination rates : 0.90 - 1 by 0.01
# Baseline Seroprevalence : 92%
# Case introduction rate : 0.01 (approximately 1 per 100 days)
# Equal introduction likelihood in either group
# Total grid area: 10 x 11 = 110
print(paste0("Begining Run 3 - ", date()))
measles_land$parameters["introduction.rate"] <- 0.01
measles_land$t2_31 <- system.time(measles_land$run_2_31 <- solutionSpace(measles_land,
                                    # Reducded search depth:
                                    count = 2000,
                                    insbound = seq(300000, 390000, 10000),
                                    vaccbound = c(0.9, 0.91, 0.92, 0.93, 0.94,
                                                  0.95, 0.96, 0.97, 0.98, 0.99, 1),
                                    len = 365,
                                    offset = 2000,
                                    grp = c(1, 1)
                                    sero.p = c(0.92, 0.94)))
print(paste0("Run 3 done - ", measles_land$t2_31[3]))
save(measles_land, file="../../Data/worker1.dat")

# Run 2 mod 3 pt 2
#-------
# Testing sensitivity to higher proportion of younger population seronegative
# Population sizes : 400,000 - 500,000 by 10,000
# Vaccination rates : 0.90 - 1 by 0.01
# Baseline Seroprevalence : 92%
# Case introduction rate : 0.01 (approximately 1 per 100 days)
# Equal introduction likelihood in either group
# Total grid area: 11 x 11 = 121
print(paste0("Begining Run 3 - ", date()))
measles_land$parameters["introduction.rate"] <- 0.01
measles_land$t2_32 <- system.time(measles_land$run_2_32 <- solutionSpace(measles_land,
                                    # Reducded search depth:
                                    count = 2000,
                                    insbound = seq(400000, 500000, 10000),
                                    vaccbound = c(0.9, 0.91, 0.92, 0.93, 0.94,
                                                  0.95, 0.96, 0.97, 0.98, 0.99, 1),
                                    len = 365,
                                    offset = 2000,
                                    grp = c(1, 1)
                                    sero.p = c(0.92, 0.94)))
print(paste0("Run 3 done - ", measles_land$t2_32[3]))
save(measles_land, file="../../Data/worker2.dat")

# Run 2 mod 4 pt 1
#-------
# Testing sensitivity to higher proportion of older population seronegative
# Population sizes : 300,000 - 390,000 by 10,000
# Vaccination rates : 0.90 - 1 by 0.01
# Baseline Seroprevalence : 92%
# Case introduction rate : 0.01 (approximately 1 per 100 days)
# Equal introduction likelihood in either group
# Total grid area: 10 x 11 = 110
print(paste0("Begining Run 4 - ", date()))
measles_land$parameters["introduction.rate"] <- 0.01
measles_land$t2_41 <- system.time(measles_land$run_2_41 <- solutionSpace(measles_land,
                                    # Reducded search depth:
                                    count = 2000,
                                    insbound = seq(300000, 390000, 10000),
                                    vaccbound = c(0.9, 0.91, 0.92, 0.93, 0.94,
                                                  0.95, 0.96, 0.97, 0.98, 0.99, 1),
                                    len = 365,
                                    offset = 2000,
                                    grp = c(1, 1)
                                    sero.p = c(0.94, 0.92)))
print(paste0("Run 4 done - ", measles_land$t2_41[3]))
save(measles_land, file="../../Data/worker1.dat")

# Run 2 mod 4 pt 2
#-------
# Testing sensitivity to higher proportion of older population seronegative
# Population sizes : 400,000 - 500,000 by 10,000
# Vaccination rates : 0.90 - 1 by 0.01
# Baseline Seroprevalence : 92%
# Case introduction rate : 0.01 (approximately 1 per 100 days)
# Equal introduction likelihood in either group
# Total grid area: 10 x 11 = 110
print(paste0("Begining Run 4 - ", date()))
measles_land$parameters["introduction.rate"] <- 0.01
measles_land$t2_42 <- system.time(measles_land$run_2_42 <- solutionSpace(measles_land,
                                    # Reducded search depth:
                                    count = 2000,
                                    insbound = seq(400000, 500000, 10000),
                                    vaccbound = c(0.9, 0.91, 0.92, 0.93, 0.94,
                                                  0.95, 0.96, 0.97, 0.98, 0.99, 1),
                                    len = 365,
                                    offset = 2000,
                                    grp = c(1, 1)
                                    sero.p = c(0.94, 0.92)))
print(paste0("Run 4 done - ", measles_land$t2_42[3]))
save(measles_land, file="../../Data/worker2.dat")

# So that works out to be 8 tasks total, split between two clusters
# which, even at a reduced search depth of 2000, is going to take a good while
# In the mean time, we should be thinking about maybe running the data on countries
# other than Sweden in the second approach, and then go back to sensitivity analyses.

# That last part of splitting up tasks manually was complicated even for two systems
# and short of writing more code to do this for me automatically, or creating a
# complex MPI-based gridsearch solution, this is the best I'm going to get. 

# Let's look at a nation like Latvia, which lies somewhere between Measles Land
# and Sweden. Like Sweden, it also considered Measles Eliminated by the WHO.
# Unlike Sweden, the estimates of seroprevalence I have from ESEN show a significant
# seroprevalence in each age group

# Run 1
# Insertion rates :   0.01-0.1 
# vaccinations rates: 0.9-1
# Total grid size : 10 * 11 = 110
# Search depth : 10,000 runs
# All other population values fixed
# Case introduction over the course of 1 year
# Offest by 2000 days beyond
source("../../Data/model_global.R")
solutions <- new.env()
print(paste0("Begining Run 1 - ", date()))
solutions$t1 <- system.time(solutions$run_1 <- solutionSpace(latvia,
                                    insbound = c(0.01, 0.02, 0.03, 0.04, 0.05,
                                                 0.06, 0.07, 0.08, 0.09, 0.1),
                                    vaccbound = c(0.9, 0.91, 0.92, 0.93, 0.94,
                                                  0.95, 0.96, 0.97, 0.98, 0.99, 1),
                                    len = 365,
                                    offset = 2000))
print(paste0("Run 1 done - ", solutions$t1[3]))
save(solutions, file = "../../Data/latvia.dat")
# Run 2
# Insertion rates :   0.01-0.1 
# vaccinations rates: 0.9-1
# Total grid size : 10 * 11 = 110
# Search depth : 10,000 runs
# All other population values fixed
# Old persons twice as likely to be introduced
# Case introduction over the course of 1 year
# Offest by 2000 days beyond
print(paste0("Begining Run 2 - ", date()))
solutions$t2 <- system.time(solutions$run_2 <- solutionSpace(latvia,
                                    insbound = c(0.01, 0.02, 0.03, 0.04, 0.05,
                                                 0.06, 0.07, 0.08, 0.09, 0.1),
                                    vaccbound = c(0.9, 0.91, 0.92, 0.93, 0.94,
                                                  0.95, 0.96, 0.97, 0.98, 0.99, 1),
                                    len = 365,
                                    grp = c(0.5, 1),
                                    offset = 2000))
print(paste0("Run 2 done - "), solutions$t2[3]))
save(solutions, file = "../../Data/latvia.dat")

# Run 3
# Insertion rates :   0.01-0.1 
# vaccinations rates: 0.9-1
# Total grid size : 10 * 11 = 110
# Search depth : 10,000 runs
# All other population values fixed
# young persons twice as likely to be introduced
# Case introduction over the course of 1 year
# Offest by 2000 days beyond
print(paste0("Begining Run 3 - ", date()))
solutions$t3 <- system.time(solutions$run_3 <- solutionSpace(latvia,
                                    insbound = c(0.01, 0.02, 0.03, 0.04, 0.05,
                                                 0.06, 0.07, 0.08, 0.09, 0.1),
                                    vaccbound = c(0.9, 0.91, 0.92, 0.93, 0.94,
                                                  0.95, 0.96, 0.97, 0.98, 0.99, 1),
                                    len = 365,
                                    grp = c(1, 0.5),
                                    offset = 2000))
print(paste0("Run 2 done - "), solutions$t2[3]))
save(solutions, file = "../../Data/latvia.dat")
print(paste0("All done - ", date()))

# After that, let's look at Malta, which is similar in size to Measles Land, but
# The ESEN 2 paper seemed to show was at an increased risk for an outbreak over
# Sweden.

# Run 1
# Insertion rates :   0.01-0.1 
# vaccinations rates: 0.9-1
# Total grid size : 10 * 11 = 110
# Search depth : 10,000 runs
# All other population values fixed
# Case introduction over the course of 1 year
# Offest by 2000 days beyond
source("../../Data/model_global.R")
source("gridsearch2.R")
sourceCpp("../src/Croots.cpp")
sourceCpp("../src/lenfind.cpp")
solutions <- new.env()
print(paste0("Begining Run 1 - ", date()))
solutions$t1 <- system.time(solutions$run_1 <- solutionSpace(malta,
                                    insbound = c(0.01, 0.02, 0.03, 0.04, 0.05,
                                                 0.06, 0.07, 0.08, 0.09, 0.1),
                                    vaccbound = c(0.9, 0.91, 0.92, 0.93, 0.94,
                                                  0.95, 0.96, 0.97, 0.98, 0.99, 1),
                                    len = 365,
                                    offset = 2000))
print(paste0("Run 1 done - ", solutions$t1[3]))
save(solutions, file = "../../Data/malta.dat")
# Run 2
# Insertion rates :   0.01-0.1 
# vaccinations rates: 0.9-1
# Total grid size : 10 * 11 = 110
# Search depth : 10,000 runs
# All other population values fixed
# Old persons twice as likely to be introduced
# Case introduction over the course of 1 year
# Offest by 2000 days beyond
print(paste0("Begining Run 2 - ", date()))
solutions$t2 <- system.time(solutions$run_2 <- solutionSpace(malta,
                                    insbound = c(0.01, 0.02, 0.03, 0.04, 0.05,
                                                 0.06, 0.07, 0.08, 0.09, 0.1),
                                    vaccbound = c(0.9, 0.91, 0.92, 0.93, 0.94,
                                                  0.95, 0.96, 0.97, 0.98, 0.99, 1),
                                    len = 365,
                                    grp = c(0.5, 1),
                                    offset = 2000))
print(paste0("Run 2 done - ", solutions$t2[3]))
save(solutions, file = "../../Data/malta.dat")

# Run 3
# Insertion rates :   0.01-0.1 
# vaccinations rates: 0.9-1
# Total grid size : 10 * 11 = 110
# Search depth : 10,000 runs
# All other population values fixed
# young persons twice as likely to be introduced
# Case introduction over the course of 1 year
# Offest by 2000 days beyond
print(paste0("Begining Run 3 - ", date()))
solutions$t3 <- system.time(solutions$run_3 <- solutionSpace(malta,
                                    insbound = c(0.01, 0.02, 0.03, 0.04, 0.05,
                                                 0.06, 0.07, 0.08, 0.09, 0.1),
                                    vaccbound = c(0.9, 0.91, 0.92, 0.93, 0.94,
                                                  0.95, 0.96, 0.97, 0.98, 0.99, 1),
                                    len = 365,
                                    grp = c(1, 0.5),
                                    offset = 2000))
print(paste0("Run 2 done - ", solutions$t2[3]))
save(solutions, file = "../../Data/malta.dat")
