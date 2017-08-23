#OPEN MP TEST
require("parallel")
print(paste0("Logical Cores Detected :", detectCores(logical = TRUE))
print(paste0("Physical Cores Detected :", detectCores(logical = FALSE))