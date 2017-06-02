# Benchmarking some code streamlining in the Multicore
# batch plot.

set.seed(1234)

a <- system.time(batch_plot_mc2(batch = 10000,
                            grp = "a",
                            insertion = 10,
                            i_number = 20))
print(a)

Sys.sleep(1)

b <- system.time(batch_plot_mc(batch = 10000,
                               grp = "a",
                               insertion = 10,
                               i_number = 20))
print(b)
