# Workspace for smarter Epi_detect function
# Try #2, the old one was inaccurate and not useful.

# Depends
require("data.table")

# Rules to define the roots of epidemic to satisfy all cases:
#   epidemic starts at first time with infected individiuals present, and ends 
#   with the number of infected = 0. 
#     - I_n > 0 AND I_(n-1) == 0  // Start
#     - I_n == 0 AND I_(n-1) > 0  // End
#   Exception: If epidemic starts at time = 0, cannot test I_(n-1), therefore:
#     - n = 0 AND I_n > 0         // Start at time = 0
#   This excludes the posibility of infected being left at the end of the
#   simulation, so always over-run it. It's also possible to throw a warning. 


roots <- function(input) {
  # Function to find the beginning and end of an epidemic based upon 
  # the criteria mentioned above
  # Args:
  #   input : A numeric vector of the count of infected at each time point.
  # Returns:
  #   ret : A logical vector of the start and endpoints for an epidemic. 
  
  ret <- vector()
  for (i in 1:length(input)) {
    if (length(input[i - 1]) == 1) {
      if (input[i] > 0) {
          if (input[i - 1] == 0) {
            ret <- c(ret, TRUE)
            next
          } else {
            ret <- c(ret, FALSE)
            next
          }
      } else if (input[i] == 0) {
          if (input[i - 1] > 0) {
            ret <- c(ret, TRUE)
            next
          } else {
            ret <- c(ret, FALSE)
            next
          }
      } else {
          stop(paste0("Returned negative at: ", i, "!"))
      }
    } else if (length(input[i - 1]) < 1) {
      if (input[i] > 0) {
        ret <- c(ret, TRUE)
        next
      } else {
       ret <- c(ret, FALSE)
       next
      }
    } else {
      stop("Something is awry!")
    }
  next
  }
  return(ret)
}

rootsc <- function(input) {
  # A possible wrapper function to C code that would do the same thing
  # only do it a lot faster. 
  
  # Sanitize
  if (input < 0){
    stop("Invalid data")
  }
  # Call C code 
  out <- .C("roots", 
            n = as.integer(length(input)),
            x = as.double(input))
  return(out$y)
  
}
Epi_detect3 <- function(result, verbose = FALSE) {
  # Attempted improvement of the diff function for the purpose of 
  # determining the length of epidemics
  # Must be run after the roots function! 
  # Args:
  #   result : A data frame, data.table, or matrix resulting from a batch run
  #            must contain time, infected counts, iteration number, and roots
  #   verbose : Logical. if TRUE, returns data frame of outbreak lengths
  # Returns:
  #   - Printed statement whether an epidemic was detected, in addition to the 
  #     the number of detected epidemics, and the target iteration
  #   - Will also return maximum outbreak length.
  
  # Sanitize
  if (!is.data.table(result)) {
    result <- as.data.table(result)
  }

  # __init__
  result <- setkey(result, iter)
  count <- 0
  outbreak_max <- 0
  iter_num <- vector()
  outbreaks <- data.frame(Length = NULL, Iteration = NULL)
  outbreak_time <- vector()

  
  # The meat of 'er
  for (i in 1:result$iter[nrow(result)]) {
    
    mat <- result[J(i)]
    mat <- setkey(mat, roots)
    mat <- mat[J(TRUE)]
    
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
  }
  
  # Since the outbreak roots should be accurate, filtering should be uneccessary
  outbreak_max <- max(outbreaks$Length)
  
 if (count == 0) {
   print("No Epidemics Detected!")
   print(paste0("Maximum outbreak time: ", outbreak_max))
 } else {
   print(paste0(count, " Epidemics detected in iteration(s): "))
   print(iter_num)
   print(paste0("Maximum outbreak time: ", outbreak_max))
 }
 if (verbose == TRUE) {
     assign("outbreaks", outbreaks, envir = .GlobalEnv)
     print("Outbreak lengths saved as: 'outbreaks'")
 }
  
}