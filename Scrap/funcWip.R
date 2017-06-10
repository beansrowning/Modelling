# Workspace for a function to detect if an epidemic has occured
# 05/02/2017

Epi_detect <-  function(result) {
   zeroes <- which(result[, "I"] == 0)
   new_mat <- result[zeroes, ]
   distance <- diff(new_mat[ ,"time"])
   if (any(distance >= 365)) {
       print("Epidemic Occurred")
   } else {
       print("No Epidemic Occurred")
   }

}

a <- matrix(c(1, 20, 30, 700, 760, 800, 4000, rep(0, 7)), nrow = 7, ncol = 2)
b <- matrix(c(1, 20, 30, 40, 50, 80, 120, rep(0, 7)), nrow = 7, ncol = 2)

colnames(a) <- c("time", "I")
colnames(b) <- c("time", "I")

Epi_detect(a)
# Epidemic Occurred
Epi_detect(b)
# No Epidemic Occurred

# TODO:
# ------
# expand to work within iterations
# tell what iteration, and how many iteration experienced epidemic
