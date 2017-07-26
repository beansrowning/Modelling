require("adaptivetau")
require("parallel")
require("doParallel")


solutionSpace <- function(envir, insbound, occbound) {
  #---Initialize parameters--------------------------------------------
    .envir <- envir
    .vars <- c("init.values", "transitions", "parameters", "RateF")
    stopifnot(any(.vars %in% ls(.envir)), is.environment(.envir))

    init <- get("init.values", envir = .envir)
    t <- get("transitions", envir = .envir)
    rf <- get("RateF", envir = .envir)
    p <- get("parameters", envir = .envir)
    fun_list <- list(init, t, rf, p, tf)
  #---Initialize parallel backend-------------------------------------
  core <- detectCores(logical = FALSE)
  gettype <- ifelse(.Platform$OS.type == "windows", "PSOCK", "FORK")
  cl <- makeCluster(core, type = gettype)
  registerDoParallel(cl)
  clusterExport(cl, c("insbound", "occbound"), envir = environment())

  #---Define per-thread simulation routine-----------------------------
  # TODO : write function similar to mul_ins()
}
