get_popvalues <- function(vec, young = 0.16, sero.p = c(0.9,0.9)) {
  # This routine creates the inital values for our SEIR model
  # by taking in the raw population number in vector format 
  # and outputting a list containing the values of the S and R
  # compartments of both populations for each value i the vector
  # Args :
  #   vec   : (Numeric Vector) of total population values being searched for
  #   young : (int) of the percentage comprising the young compartments of the model
  #   sero.p: (Numeric Vector) of the seroprevalence in the young and old compartments
  # Returns :
  #   popvalues : (list) containg all values of S and R to be assigned in the search 
  
  #---Skipping some of the checks, don't break it-----------------
  stopifnot(is.vector(vec), length(young) == 1,
            is.vector(sero.p) && length(sero.p) == 2)
  #---Initialize--------------------------------------------------
  out <- list()
  #---...and make the list----------------------------------------
  for(i in 1:length(vec)) {
    out <- list(out, list(S1 = vec[i] * young * (1 - sero.p[1]),
                          S2 = vec[i] * (1 - young) * (1 - sero.p[2]),
                          R1 = vec[i] * young * sero.p[1],
                          R2 = vec[i] * (1 - young) * sero.p[2]))
  }
  #---Return to parent frame for access in a function-------------
  popvalues <<- out
}