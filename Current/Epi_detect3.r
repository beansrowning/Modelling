# Epidemic detector (data.table variant)
# very, very fast. 
# 5 Jun 2017

# Depends 
require(data.table)


Epi_detect <- function(result, verbose = FALSE) {
  # Calculates the length of time between two periods of zero cases
  # If the length is longer than 365 days, it will return positive
  # Somewhat slow, but could possibly be streamlined or parallelized even
  #
  # Args:
  #   result : A data frame or matrix resulting from a batch run
  #            must contain time, number of infected, and run number
  #   verbose : Logical. if TRUE, returns raw vector of outbreak times
  # Returns:
  #   Printed statement whether an epidemic was detected or not in addition to
  #   the number of detected epidemics, and the iterations where they occurred.
  #   Will also return the maximum outbreak length

  # Sanitize
  if (!is.data.table(result)) {
    result <- as.data.table(result)
  }
  
  # __init__
  result <- setkey(result, iter)
  count <- 0
  iter_num <- vector()
  outbreaks <- vector()
  
  # The meat of 'er
  for (i in 1:result$iter[nrow(result)]) {
    
    mat <- result[J(i)]
    mat <- setkey(mat, I)
    mat <- mat[J(0)]
    mat <- mat$time
    outbreak_time <- diff(mat)
    
    outbreaks <- c(outbreak_time, outbreaks)
    if (any(outbreak_time >= 365)) {
      count <- count + 1
      iter_num <- c(iter_num, i)
    } else {
      next # probably not needed
    }
    
  }
  
  outbreak_max <- max(outbreaks)
  if (count == 0) {
    print("No Epidemics Detected!")
    print(paste0("Maximum outbreak time ", outbreak_max))
  } else {
    print(paste0(count, " Epidemics detected in iteration(s): "))
    print(iter_num)
    print(paste0("Maximum outbreak time ", outbreak_max))
  }
  if (verbose == TRUE) {
      assign("outbreaks", outbreaks, envir = .GlobalEnv)
      print("Outbreak lengths saved as 'outbreaks'")
  }

}
# TODO: Drop outbreaks if == 360.0000000000