# Workspace for a function to detect if an epidemic has occured 
# 05/02/2017
# imports: ggplot2, magrittr (tidyverse)

if (!require(ggplot2)) {
 install.packages("ggplot2")
}
if (!require(magrittr)) {
  install.packages("magrittr")
}


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