# Trial.cpp R wrapper

require("Rcpp")
sourceCpp("./src/trial.cpp")
source("./trialfunc.R")
source("./data.r")

trial <- function(env, agegp) {
  time <- system.time(out <- trialrun(env, agegp))
  print("Completed.")
  assign("time", time, envir = .GlobalEnv)
  return(out)
}
