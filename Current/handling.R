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

## Data managment functions from stackoverflow
.ls.objects <- function (pos = 1, pattern, order.by,
                        decreasing = FALSE, head = FALSE, n = 5) {
    napply <- function(names, fn) sapply(names, function(x)
                                         fn(get(x, pos = pos)))
    names <- ls(pos = pos, pattern = pattern)
    obj.class <- napply(names, function(x) as.character(class(x))[1])
    obj.mode <- napply(names, mode)
    obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
    obj.prettysize <- napply(names, function(x) {
                           capture.output(format(utils::object.size(x),
                                                 units = "auto"))
                           })
    obj.size <- napply(names, object.size)
    obj.dim <- t(napply(names, function(x)
                        as.numeric(dim(x))[1:2]))
    vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
    obj.dim[vec, 1] <- napply(names, length)[vec]
    out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
    names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
    if (!missing(order.by))
        out <- out[order(out[[order.by]], decreasing = decreasing), ]
    if (head)
        out <- head(out, n)
    out
}

lsos <- function(..., n=10) {
    .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}
