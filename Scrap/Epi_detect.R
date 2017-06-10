# Epidemic detector functions (depreciated)
# 7 Jun 2017

# Depends
require("data.table")


Epi_detect <- function(result, verbose = FALSE) {
  # - Calculates the length of time between periods of zero cases
  # - If the length is longer than 365 days, it will return positive
  # - Speed improved with data.table subsetting methods
  #
  # Args:
  #   result : A data frame or matrix resulting from a batch run
  #            must contain time, number of infected, and run number
  #   verbose : Logical. if TRUE, returns raw vector of outbreak lengths
  # Returns:
  #   Printed statement whether an epidemic was detected or not in addition to
  #   the number of detected epidemics, and the iterations where they occurred.
  #   - Will also return the maximum outbreak length ...
  #     (which shouldn't be trusted as most will be false readings)

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

    outbreaks <- c(outbreaks, outbreak_time)
    if (any(outbreak_time >= 365)) {
      count <- count + 1
      iter_num <- c(iter_num, i)
    } else {
      next # probably not needed
    }

  }
  # Due to some issue with the precision doubles are being stored
  # at, time has to be rounded to 3 decimal places to subset properly.
  # I have no idea how to get around that.
  outbreaks <- round(outbreaks, 3)
  outbreaks <- outbreaks[!outbreaks %in% 360.5]
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

Epi_detect2 <- function(result, verbose = FALSE) {
  # - Calculates the length of time between periods of zero cases
  # - If the length is longer than 365 days, it will return positive
  # - Speed improved with data.table subsetting methods
  #
  # Args:
  #   result : A data frame or matrix resulting from a batch run
  #            must contain time, number of infected, and run number
  #   verbose : Logical. if TRUE, returns raw vector of outbreak lengths
  # Returns:
  #   Printed statement whether an epidemic was detected or not in addition to
  #   the number of detected epidemics, and the iterations where they occurred.
  #   - Will also return the maximum outbreak length ...
  #     (which shouldn't be trusted as most will be false readings)

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
    mat <- setkey(mat, roots)
    mat <- mat[J(TRUE)]
    mat <- mat$time
    outbreak_time <- diff(mat)

    outbreaks <- c(outbreaks, outbreak_time)
    if (any(outbreak_time >= 365)) {
      count <- count + 1
      iter_num <- c(iter_num, i)
    } else {
      next # probably not needed
    }

  }
  # Due to some issue with the precision doubles are being stored
  # at, time has to be rounded to 3 decimal places to subset properly.
  # I have no idea how to get around that.
  outbreaks <- round(outbreaks, 3)
  outbreaks <- outbreaks[!outbreaks %in% 360.5]
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
      assign("outbreaks2", outbreaks, envir = .GlobalEnv)
      print("Outbreak lengths saved as 'outbreaks'")
  }

}
