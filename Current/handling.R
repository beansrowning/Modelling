# Data handling utilities

savesimdat <- function(obj) {
  # Saves environment containing sim data to disk in the Data folder
  # Args:
  #   obj : Name of the environment to save
  # Returns:
  #   - object saved to the /Data/ directory
  name <- deparse(substitute(obj))
  file <- paste0(name, ".dat")
  save(obj, file = paste0("../Data/", file), compress = "bzip2")
  print(paste0("File ", file, " saved in: Data/"))
}

loadsimdat <- function(obj) {
  # Loads previously saved sim data to the GlobalEnv
  # Args:
  #   obj : Name of the file to load without extension
  # Returns:
  #   - Environment loaded into current R session
  name <- deparse(substitute(obj))
  file <- paste0(name, ".dat")
  load(paste0("../Data/", file))
  assign(name, obj, envir = .GlobalEnv)
  print(paste0(file, " loaded into R sucessfully!"))
}
