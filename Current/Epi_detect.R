# Finally, a functional Epidemic Detector
# 11 June 2017

# Depends
require("data.table")
require("Rcpp")

# Root finder (writen in C++)
# If Rtools is not installed, or admin access not available, load package
# (move "../Data/Croots" to your R library directory)
tryCatch(sourceCpp("./src/Croots.cpp"),
         error = function(e) {
           print("Croots couldn't load, trying package instead... ")
           tryCatch(require("Croots"),
                    error = function(e){
                      print("Library failed to load. Is it installed?")
                    })
         })

# Rules to define the roots of epidemic to satisfy all cases:
#   epidemic starts at first time with infected individiuals present, and ends
#   with the number of infected = 0.
#     - I_n > 0 AND I_(n-1) == 0  // Start
#     - I_n == 0 AND I_(n-1) > 0  // End
#   Exception: If epidemic starts at time = 0, cannot test I_(n-1), therefore:
#     - n = 0 AND I_n > 0         // Start at time = 0
#   This excludes the posibility of infected being left at the end of the
#   simulation, so always over-run it. It's also possible to throw a warning.
# Therefore:
#   Testing at X_n whether X_(n-1) > 0 will ensure that the only length
#   being caluclated is epidemic length, and not the time between epidemics.

Epi_detect <- function(result, verbose = FALSE) {
  # Improvement of the diff function for the purpose of determining
  # the length of epidemics. Calls a C++ function to determine
  # the roots of the epidemic before calculating the length.
  #
  # Args:
  #   result : A data frame, data.table, or matrix resulting from a batch run
  #            must contain time, infected counts, iteration number, and roots
  #   verbose : Logical. if TRUE, returns data frame of outbreak lengths and
  #             affixes results of the Croots function to the input
  # Returns:
  #   - Printed statement whether an epidemic was detected, in addition to the
  #     the number of detected epidemics, and the target iteration
  #   - Will also return maximum outbreak length.

  # Name for assignment if verbose
  dtname <- deparse(substitute(result))

  # Sanitize
  if (!is.data.table(result)) {
    result <- as.data.table(result)
  }
  #---init----------------------------------------------------------
  count <- 0
  outbreak_max <- 0
  tf <- result[nrow(result), "time"]
  iter_num <- vector()
  outbreak_time <- vector()
  proc <- vector()
  outbreaks <- data.frame(Length = NULL, Iteration = NULL)
  #---Error check for outbreaks that ran over simulation time------
  setkey(result, time)
  proc <- result[.(tf)][, I]
  if (any(proc != 0)) {
    stop("Outbreak ran over simulation at least once!")
  }
  setkey(result, iter, time) # re-sort to ensure proper analysis
  #---Call root finder function on the Infection counts-------------
  result[, roots := Croots(I)]
  #---The meat of 'er------------------------------------------------
  for (i in 1:result$iter[nrow(result)]) {
    setkey(result, iter, roots)
    mat <- result[.(i, TRUE)]
    for (j in 2:nrow(mat)) {
      if (mat$I[j - 1] > 0) {
        outbreak_time <- c(outbreak_time, (mat$time[j] - mat$time[j - 1]))
      }
    }
    if (any(outbreak_time >= 365)) {
      count <- count + 1
      iter_num <- c(iter_num, i)
    }
    outbreaks <- rbind(outbreaks, cbind(Length = outbreak_time, Iteration = i))
    outbreak_time <- NULL
    mat <- NULL
  }
  outbreak_max <- max(outbreaks$Length)
 #---Output-----------------------------------------------------------
 if (count == 0) {
   print("No Epidemics Detected!")
   print(paste0("Maximum outbreak time: ", outbreak_max))
 } else {
   print(paste0(count, " Epidemics detected in iteration(s): "))
   print(iter_num)
   print(paste0("Maximum outbreak time: ", outbreak_max))
 }
 if (verbose == TRUE) {
     assign("outbreaks", outbreaks, envir = parent.frame())
     print("Outbreak lengths saved as: 'outbreaks'")

     assign(dtname, result, envir = parent.frame())
     print("Roots saved to input file")

     if (count > 0) {
       setkey(result, iter, time)
       check <- result[iter %in% iter_num]
       assign("check", check, envir = parent.frame())
     }
 }
}
