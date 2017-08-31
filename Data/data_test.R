require(data.table)

data_test <- function(envir) {
  runs <- list("run_1", "run_2", "run_3")

  for (rn in runs) {

    cat("Checking for", rn, "...")

    if (rn %in% ls(envir)) {
      cat("TRUE", "\n")
    } else {
      cat("FALSE", "\n")
      warning(paste0(rn, "was not found!"))
    }

    cat("Checking for missing data...")
    dt <- get(rn, envir = envir)
    if (any(is.na(dt[, median])) || any(is.na(dt[, mean]))) {
      cat("FALSE", "\n")
      warning(paste0(rn, "contains missing data!"))
    } else {
      cat("TRUE", "\n")
    }
  }
}
