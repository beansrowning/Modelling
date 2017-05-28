# Workspace for a function to detect if an epidemic has occured 
# 05/02/2017
# imports: tidyverse

if (!require(tidyverse)) {
  install.packages("tidyverse")
}
library(tidyverse)


Epi_detect <-  function(result) {
   zeroes <- which(result[, "I"] == 0)
   new_mat <- result[zeroes, ]
   distance <- diff(new_mat["time"])
   if (!any(distance >= 365)) {
       print("Epidemic Occured")
   } else {
       print("No Epidemic Occured")
   }
   
}

a <- matrix(time = c(1, 20, 30, 700, 760, 800, 4000), I = 0)
Epi_detect(a)