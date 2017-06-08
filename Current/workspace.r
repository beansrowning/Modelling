# Workspace for smarter Epi_detect function
# Done, just need to combine into a functional Epi_detect
# TODO: Combine functions

testtime <- 0:19
# Case 1: Two peaks, epidemic starts after time 0 and simulation
#         runs past end of epidemic
# Real epimic lengths: 2 x 8 days
testseq1 <- c(0, 1, 2, 3, 4, 3, 2, 1, 0, 1, 2, 3, 4, 5, 3, 1, 0, 0, 0, 0)
trueseq1 <- c(T, F, F, F, F, F, F, F, T, F, F, F, F, F, F, F, T, F, F, F)
# Case 2: One peak, epidemic starts after time 0 and simulation runs
#         past the end of the epidemic
# Real epidemic length: 1 x 10 days
testseq2 <- c(0, 0, 0, 3, 4, 5, 6, 10, 5, 3, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0)
trueseq2 <- c(F, F, T, F, F, F, F, F, F, F, F, F, T, F, F, F, F, F, F, F)
# Case 3: Two peaks, epidemic began at time 0 and simulation runs past
#         the end of the epidemic
# Real epidemic length: 2 x 8 days ?
testseq3 <- c(1, 1, 2, 3, 4, 3, 2, 1, 0, 0, 2, 3, 4, 5, 3, 1, 1, 0, 0, 0)
trueseq3 <- c(T, F, F, F, F, F, F, F, T, T, F, F, F, F, F, F, F, T, F, F)
# Case 4: One peak, epidemic began at time 0 and simulation does not run
#         past the end of the epidemic
# Real epidemic length: 1 x 20 days
testseq4 <- c(1, 1, 2, 3, 4, 5, 6, 10, 5, 3, 2, 2, 2, 2, 2, 2, 2, 1, 1, 0)
trueseq4 <- c(T, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, T)

# Rules to define the roots of epidemic to satisfy all cases:
# - if X_n = 0, AND (X_(n-1) = NA and X_(n+1) > 0)
# - if X_n = 0, AND (X_(n-1) > 0 and X_(n+1) > 0)
# - if X_n = 0, AND (X_(n-1) > 0 and X_(n+1) = 0)
# - if X_n = 0, AND (X_(n-1) = 0 and X_(n+1) > 0)
# - if X_n = 0, AND (X_(n-1) > 0 and X_(n+1) = NA)
# - if X_n > 0, AND (X_(n-1) = NA and (X_(n+1) > 0 | X_(n+1) = 0))


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

print("Test 1 (2 x 8 days) returned: ")
try1 <- endpoints(testseq1)
mat1 <- cbind(testtime, testseq1, try1)
mat1 <- mat1[mat1[, "try1"] %in% TRUE, "testtime"]
mat1 <- diff(mat1)
print(paste0("Epidemic: ", mat1, " days"))
print(identical(try1, trueseq1))

print("Test 2 (1 x 10 days) returned: ")
try2 <- endpoints(testseq2)
mat2 <- cbind(testtime, testseq2, try2)
mat2 <- mat2[mat2[, "try2"] %in% TRUE, "testtime"]
mat2 <- diff(mat2)
print(paste0("Epidemic: ", mat2, " days"))
print(identical(try2, trueseq2))

print("Test 3 (2 x 8 days) returned: ")
try3 <- endpoints(testseq3)
mat3 <- cbind(testtime, testseq3, try3)
mat3 <- mat3[mat3[, "try3"] %in% TRUE, "testtime"]
mat3 <- diff(mat3)
print(paste0("Epidemic: ", mat3, " days"))
print(identical(try3, trueseq3))

print("Test 4 (1 x 20 days) returned: ")
try4 <- endpoints(testseq4)
mat4 <- cbind(testtime, testseq4, try4)
mat4 <- mat4[mat4[, "try4"] %in% TRUE, "testtime"]
mat4 <- diff(mat4)
print(paste0("Epidemic: ", mat4, " days"))
print(identical(try4, trueseq4))
