# Benchmark Test
# First run:
# user system elapsed
# 4897.32 1119.98 6019.81
# user system elapsed
# 13.48 3.22 95.13
set.seed(1234)

a <- system.time(batch_plot(batch = 10000, grp = "a", insertion = 10, i_number = 20))
print(a)

Sys.sleep(1)

b <- system.time(batch_plot_mc(batch = 10000, grp = "a", insertion = 10, i_number = 20))
print(b)
