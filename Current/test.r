# Benchmarking epi_detect 

# Depends:
require(dplyr)
require(microbenchmark)
require(data.table)
source("Epi_detect.r")
source("Epi_detect2.r")

# __Dataset in__
input <- read.table("test.csv")
print(paste0(Sys.time()," - Done loading"))

# Benchmark

print(paste0(Sys.time()," - Start First variant."))
Epi_detect(input)

print(paste0(Sys.time()," - First First variant done."))

print(paste0(Sys.time()," - Start Second variant."))
Epi_detect_2(input)
print(paste0(Sys.time(), " - Second variant done."))