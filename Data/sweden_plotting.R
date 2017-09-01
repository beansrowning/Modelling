require(ggplot2)
require(data.table)
source("../Current/Parameterized/plot.R")
#---First Task--------------------
load("sweden_1.dat")
# First run
# ---------
# 3d Surface plot to see where to take cuts at
threedPlot(solutions$run_1, variables = c("max", "mean", "median"),
           xlab = "Case Introduction Rate",
           ylab = "MMR Vaccination Rate",
           zlab = "Outbreak Length",
           title = "Sweden Run 1 - Equal Case Introduction")
save(graph, file = "./output/Sweden/Base/1_3d.dat")
# Looking at changes in outbreak length along MMR rate

## lowest introduction rate
twodPlot(solutions$run_1, "vacc", c("mean", "median", "iqr", "max"),
         "ins", 0.01, logy = TRUE,
          xlab = "MMR Vaccination Rate",
          ylab = "Outbreak Length (days)",
          title = "Sweden Run 1 - Equal Case Introduction",
          sub = "0.01 introduction rate")
ggsave(filename="1_1.jpg", plot = graph, path = "./output/Sweden/Base")

## Highest introduction rate
twodPlot(solutions$run_1, "vacc", c("mean", "median", "iqr", "max"),
         "ins", 0.1, logy = TRUE,
          xlab = "MMR Vaccination Rate",
          ylab = "Outbreak Length (days)",
          title = "Sweden Run 1 - Equal Case Introduction",
          sub = "0.1 introduction rate")
ggsave(filename="1_2.jpg", plot = graph, path = "./output/Sweden/Base")

# Second run
# ----------
threedPlot(solutions$run_2, variables = c("max", "mean", "median"),
           xlab = "Case Introduction Rate",
           ylab = "MMR Vaccination Rate",
           zlab = "Outbreak Length",
           title = "Sweden Run 2 - Older Case Introduction")
save(graph, file = "./output/Sweden/Base/2_3d.dat")
# Looking at changes in outbreak length along MMR rate

## lowest introduction rate
twodPlot(solutions$run_2, "vacc", c("mean", "median", "iqr", "max"),
         "ins", 0.01, logy = TRUE,
          xlab = "MMR Vaccination Rate",
          ylab = "Outbreak Length (days)",
          title = "Sweden Run 2 - Older Case Introduction",
          sub = "0.01 introduction rate")
ggsave(filename="2_1.jpg", plot = graph, path = "./output/Sweden/Base")

## Highest introduction rate
twodPlot(solutions$run_2, "vacc", c("mean", "median", "iqr", "max"),
         "ins", 0.1, logy = TRUE,
          xlab = "MMR Vaccination Rate",
          ylab = "Outbreak Length (days)",
          title = "Sweden Run 2 - Older Case Introduction",
          sub = "0.1 introduction rate")
ggsave(filename="2_2.jpg", plot = graph, path = "./output/Sweden/Base")

# Third run
# ----------
threedPlot(solutions$run_3, variables = c("max", "mean", "median"),
           xlab = "Case Introduction Rate",
           ylab = "MMR Vaccination Rate",
           zlab = "Outbreak Length",
           title = "Sweden Run 3 - Younger Case Introduction")
save(graph, file = "./output/Sweden/Base/2_3d.dat")
# Looking at changes in outbreak length along MMR rate

## lowest introduction rate
twodPlot(solutions$run_3, "vacc", c("mean", "median", "iqr", "max"),
         "ins", 0.01, logy = TRUE,
          xlab = "MMR Vaccination Rate",
          ylab = "Outbreak Length (days)",
          title = "Sweden Run 3 - Younger Case Introduction",
          sub = "0.01 introduction rate")
ggsave(filename="3_1.jpg", plot = graph, path = "./output/Sweden/Base")

## Highest introduction rate
twodPlot(solutions$run_3, "vacc", c("mean", "median", "iqr", "max"),
         "ins", 0.1, logy = TRUE,
          xlab = "MMR Vaccination Rate",
          ylab = "Outbreak Length (days)",
          title = "Sweden Run 3 - Younger Case Introduction",
          sub = "0.1 introduction rate")
ggsave(filename="3_2.jpg", plot = graph, path = "./output/Sweden/Base")

rm("graph", "plot_dat", "solutions")

#---Second Task--------------------
load("sweden_2.dat")

# First run
# ---------
# 3d Surface plot to see where to take cuts at
threedPlot(solutions$run_1, variables = c("max", "mean", "median"),
           xlab = "Case Introduction Rate",
           ylab = "MMR Vaccination Rate",
           zlab = "Outbreak Length",
           title = "Sweden Run 1 - 12 month delay")
save(graph, file = "./output/Sweden/Base/4_3d.dat")
# Looking at changes in outbreak length along MMR rate
## lowest introduction rate
twodPlot(solutions$run_1, "vacc", c("mean", "median", "iqr", "max"),
         "ins", 0.01, logy = TRUE,
          xlab = "MMR Vaccination Rate",
          ylab = "Outbreak Length (days)",
          title = "Sweden Run 1 - 12 month delay",
          sub = "0.01 introduction rate")
ggsave(filename="4_1.jpg", plot = graph, path = "./output/Sweden/Base")

## Highest introduction rate
twodPlot(solutions$run_1, "vacc", c("mean", "median", "iqr", "max"),
         "ins", 0.1, logy = TRUE,
          xlab = "MMR Vaccination Rate",
          ylab = "Outbreak Length (days)",
          title = "Sweden Run 1 - 12 month delay",
          sub = "0.1 introduction rate")
ggsave(filename="4_2.jpg", plot = graph, path = "./output/Sweden/Base")

# Second run
# ----------
threedPlot(solutions$run_2, variables = c("max", "mean", "median"),
           xlab = "Case Introduction Rate",
           ylab = "MMR Vaccination Rate",
           zlab = "Outbreak Length",
           title = "Sweden Run 2 - 24 month delay")
save(graph, file = "./output/Sweden/Base/5_3d.dat")
# Looking at changes in outbreak length along MMR rate

## lowest introduction rate
twodPlot(solutions$run_2, "vacc", c("mean", "median", "iqr", "max"),
         "ins", 0.01, logy = TRUE,
          xlab = "MMR Vaccination Rate",
          ylab = "Outbreak Length (days)",
          title = "Sweden Run 2 - 24 month delay",
          sub = "0.01 introduction rate")
ggsave(filename="5_1.jpg", plot = graph, path = "./output/Sweden/Base")

## Highest introduction rate
twodPlot(solutions$run_2, "vacc", c("mean", "median", "iqr", "max"),
         "ins", 0.1, logy = TRUE,
          xlab = "MMR Vaccination Rate",
          ylab = "Outbreak Length (days)",
          title = "Sweden Run 2 - 24 month delay",
          sub = "0.1 introduction rate")
ggsave(filename="5_2.jpg", plot = graph, path = "./output/Sweden/Base")

# Third run
# ----------
threedPlot(solutions$run_3, variables = c("max", "mean", "median"),
           xlab = "Case Introduction Rate",
           ylab = "MMR Vaccination Rate",
           zlab = "Outbreak Length",
           title = "Sweden Run 3 - 36 month delay")
save(graph, file = "./output/Sweden/Base/6_3d.dat")
# Looking at changes in outbreak length along MMR rate

## lowest introduction rate
twodPlot(solutions$run_3, "vacc", c("mean", "median", "iqr", "max"),
         "ins", 0.01, logy = TRUE,
          xlab = "MMR Vaccination Rate",
          ylab = "Outbreak Length (days)",
          title = "Sweden Run 3 - 36 month delay",
          sub = "0.01 introduction rate")
ggsave(filename="6_1.jpg", plot = graph, path = "./output/Sweden/Base")

## Highest introduction rate
twodPlot(solutions$run_3, "vacc", c("mean", "median", "iqr", "max"),
         "ins", 0.1, logy = TRUE,
          xlab = "MMR Vaccination Rate",
          ylab = "Outbreak Length (days)",
          title = "Sweden Run 3 - 36 month delay",
          sub = "0.1 introduction rate")
ggsave(filename="6_2.jpg", plot = graph, path = "./output/Sweden/Base")

rm("graph", "plot_dat", "solutions")

#---First Task, Higher seronegativity--------------------
load("sweden_1_wc.dat")

# First run
# ---------
# 3d Surface plot to see where to take cuts at
threedPlot(solutions$run_1, variables = c("max", "mean", "median"),
           xlab = "Case Introduction Rate",
           ylab = "MMR Vaccination Rate",
           zlab = "Outbreak Length",
           title = "Sweden Run 1 - Equal Case Introduction")
save(graph, file = "./output/Sweden/Worst Case/1_3d.dat")
# Looking at changes in outbreak length along MMR rate
## lowest introduction rate
twodPlot(solutions$run_1, "vacc", c("mean", "median", "iqr", "max"),
         "ins", 0.01, logy = TRUE,
          xlab = "MMR Vaccination Rate",
          ylab = "Outbreak Length (days)",
          title = "Sweden Run 1 - Equal Case Introduction",
          sub = "0.01 introduction rate")
ggsave(filename="1_1.jpg", plot = graph, path = "./output/Sweden/Worst Case")

## Highest introduction rate
twodPlot(solutions$run_1, "vacc", c("mean", "median", "iqr", "max"),
         "ins", 0.1, logy = TRUE,
          xlab = "MMR Vaccination Rate",
          ylab = "Outbreak Length (days)",
          title = "Sweden Run 1 - Equal Case Introduction",
          sub = "0.1 introduction rate")
ggsave(filename="1_2.jpg", plot = graph, path = "./output/Sweden/Worst Case")

# Second run
# ----------
threedPlot(solutions$run_2, variables = c("max", "mean", "median"),
           xlab = "Case Introduction Rate",
           ylab = "MMR Vaccination Rate",
           zlab = "Outbreak Length",
           title = "Sweden Run 2 - Older Case Introduction")
save(graph, file = "./output/Sweden/Worst Case/2_3d.dat")
# Looking at changes in outbreak length along MMR rate

## lowest introduction rate
twodPlot(solutions$run_2, "vacc", c("mean", "median", "iqr", "max"),
         "ins", 0.01, logy = TRUE,
          xlab = "MMR Vaccination Rate",
          ylab = "Outbreak Length (days)",
          title = "Sweden Run 2 - Older Case Introduction",
          sub = "0.01 introduction rate")
ggsave(filename="2_1.jpg", plot = graph, path = "./output/Sweden/Worst Case")

## Highest introduction rate
twodPlot(solutions$run_2, "vacc", c("mean", "median", "iqr", "max"),
         "ins", 0.1, logy = TRUE,
          xlab = "MMR Vaccination Rate",
          ylab = "Outbreak Length (days)",
          title = "Sweden Run 2 - Older Case Introduction",
          sub = "0.1 introduction rate")
ggsave(filename="2_2.jpg", plot = graph, path = "./output/Sweden/Worst Case")

# Third run
# ----------
threedPlot(solutions$run_3, variables = c("max", "mean", "median"),
           xlab = "Case Introduction Rate",
           ylab = "MMR Vaccination Rate",
           zlab = "Outbreak Length",
           title = "Sweden Run 3 - Younger Case Introduction")
save(graph, file = "./output/Sweden/Worst Case/2_3d.dat")
# Looking at changes in outbreak length along MMR rate

## lowest introduction rate
twodPlot(solutions$run_3, "vacc", c("mean", "median", "iqr", "max"),
         "ins", 0.01, logy = TRUE,
          xlab = "MMR Vaccination Rate",
          ylab = "Outbreak Length (days)",
          title = "Sweden Run 3 - Younger Case Introduction",
          sub = "0.01 introduction rate")
ggsave(filename="3_1.jpg", plot = graph, path = "./output/Sweden/Worst Case")

## Highest introduction rate
twodPlot(solutions$run_3, "vacc", c("mean", "median", "iqr", "max"),
         "ins", 0.1, logy = TRUE,
          xlab = "MMR Vaccination Rate",
          ylab = "Outbreak Length (days)",
          title = "Sweden Run 3 - Younger Case Introduction",
          sub = "0.1 introduction rate")
ggsave(filename="3_2.jpg", plot = graph, path = "./output/Sweden/Worst Case")

rm("graph", "plot_dat", "solutions")

#---Second Task, Higher seronegativity--------------------
load("sweden_2_wc.dat")

# First run
# ---------
# 3d Surface plot to see where to take cuts at
threedPlot(solutions$run_1, variables = c("max", "mean", "median"),
           xlab = "Case Introduction Rate",
           ylab = "MMR Vaccination Rate",
           zlab = "Outbreak Length",
           title = "Sweden Run 1 - 12 month delay")
save(graph, file = "./output/Sweden/Worst Case/4_3d.dat")
# Looking at changes in outbreak length along MMR rate
## lowest introduction rate
twodPlot(solutions$run_1, "vacc", c("mean", "median", "iqr", "max"),
         "ins", 0.01, logy = TRUE,
          xlab = "MMR Vaccination Rate",
          ylab = "Outbreak Length (days)",
          title = "Sweden Run 1 - 12 month delay",
          sub = "0.01 introduction rate")
ggsave(filename="4_1.jpg", plot = graph, path = "./output/Sweden/Worst Case")

## Highest introduction rate
twodPlot(solutions$run_1, "vacc", c("mean", "median", "iqr", "max"),
         "ins", 0.1, logy = TRUE,
          xlab = "MMR Vaccination Rate",
          ylab = "Outbreak Length (days)",
          title = "Sweden Run 1 - 12 month delay",
          sub = "0.1 introduction rate")
ggsave(filename="4_2.jpg", plot = graph, path = "./output/Sweden/Worst Case")

# Second run
# ----------
threedPlot(solutions$run_2, variables = c("max", "mean", "median"),
           xlab = "Case Introduction Rate",
           ylab = "MMR Vaccination Rate",
           zlab = "Outbreak Length",
           title = "Sweden Run 2 - 24 month delay")
save(graph, file = "./output/Sweden/Worst Case/5_3d.dat")
# Looking at changes in outbreak length along MMR rate

## lowest introduction rate
twodPlot(solutions$run_2, "vacc", c("mean", "median", "iqr", "max"),
         "ins", 0.01, logy = TRUE,
          xlab = "MMR Vaccination Rate",
          ylab = "Outbreak Length (days)",
          title = "Sweden Run 2 - 24 month delay",
          sub = "0.01 introduction rate")
ggsave(filename="5_1.jpg", plot = graph, path = "./output/Sweden/Worst Case")

## Highest introduction rate
twodPlot(solutions$run_2, "vacc", c("mean", "median", "iqr", "max"),
         "ins", 0.1, logy = TRUE,
          xlab = "MMR Vaccination Rate",
          ylab = "Outbreak Length (days)",
          title = "Sweden Run 2 - 24 month delay",
          sub = "0.1 introduction rate")
ggsave(filename="5_2.jpg", plot = graph, path = "./output/Sweden/Worst Case")

# Third run
# ----------
threedPlot(solutions$run_3, variables = c("max", "mean", "median"),
           xlab = "Case Introduction Rate",
           ylab = "MMR Vaccination Rate",
           zlab = "Outbreak Length",
           title = "Sweden Run 3 - 36 month delay")
save(graph, file = "./output/Sweden/Worst Case/6_3d.dat")
# Looking at changes in outbreak length along MMR rate

## lowest introduction rate
twodPlot(solutions$run_3, "vacc", c("mean", "median", "iqr", "max"),
         "ins", 0.01, logy = TRUE,
          xlab = "MMR Vaccination Rate",
          ylab = "Outbreak Length (days)",
          title = "Sweden Run 3 - 36 month delay",
          sub = "0.01 introduction rate")
ggsave(filename="6_1.jpg", plot = graph, path = "./output/Sweden/Worst Case")

## Highest introduction rate
twodPlot(solutions$run_3, "vacc", c("mean", "median", "iqr", "max"),
         "ins", 0.1, logy = TRUE,
          xlab = "MMR Vaccination Rate",
          ylab = "Outbreak Length (days)",
          title = "Sweden Run 3 - 36 month delay",
          sub = "0.1 introduction rate")
ggsave(filename="6_2.jpg", plot = graph, path = "./output/Sweden/Worst Case")

rm("graph", "plot_dat", "solutions")
