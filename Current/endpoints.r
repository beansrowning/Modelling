# Epidemic endpoint capturing function
# 8 June 2017

endpoints <- function(input) {
  ret <- vector()
  for (i in 1:length(input)) {

    if (input[i] == 0) {
      if (length(input[i - 1]) == 0) {

        if (input[i + 1] > 0) {
          ret <- c(ret, TRUE)
        } else {
          ret <- c(ret, FALSE)
        }

      } else if (length(input[i - 1]) == 1 & !is.na(input[i + 1])) {

        if ( (input[i - 1] > 0) & (input[i + 1] > 0)) {
          ret <- c(ret, TRUE)
        } else if (input[i - 1] > 0 & input[i + 1] == 0) {
          ret <- c(ret, TRUE)
        } else if (input[i - 1] == 0 & input[i + 1] > 0) {
          ret <- c(ret, TRUE)
        } else {
          ret <- c(ret, FALSE)
        }
      } else if (is.na(input[i + 1]) & input[i - 1] != 0) {
        ret <- c(ret, TRUE)
      } else {
      ret <- c(ret, FALSE)
      }

    } else if (input[i] > 0) {

      if (length(input[i - 1]) == 0 & input[i + 1] >= 0) {
        ret <- c(ret, TRUE)
      } else {
        ret <- c(ret, FALSE)
      }
    } else {
      stop("Invalid dimensions or input data!")
    }
  }
  return(ret)
}
