require(ggplot2)
require(data.table)
source("../Current/Parameterized/plot.R")

#---First Task--------------------
load("latvia_1.dat")

# First run
# ---------
# 3d Surface plot to see where to take cuts at
threedPlot(solutions$run_1, variables = c("max", "mean", "median"),
           xlab = "Case Introduction Rate",
           ylab = "MMR Vaccination Rate",
           zlab = "Outbreak Length",
           title = "Latvia Run 1 - Equal Case Introduction")
save(graph, file = "./output/Latvia/1_3d.dat")
# Looking at changes in outbreak length along MMR rate

## lowest introduction rate
twodPlot(solutions$run_1, "vacc", c("mean", "median", "iqr", "max"),
         "ins", 0.01, logy = TRUE,
          xlab = "MMR Vaccination Rate",
          ylab = "Outbreak Length (days)",
          title = "Latvia Run 1 - Equal Case Introduction",
          sub = "0.01 introduction rate")
ggsave(filename="1_1.jpg", plot = graph, path = "./output/Latvia")

## Highest introduction rate
twodPlot(solutions$run_1, "vacc", c("mean", "median", "iqr", "max"),
         "ins", 0.1, logy = TRUE,
          xlab = "MMR Vaccination Rate",
          ylab = "Outbreak Length (days)",
          title = "Latvia Run 1 - Equal Case Introduction",
          sub = "0.1 introduction rate")
ggsave(filename="1_2.jpg", plot = graph, path = "./output/Latvia")

# Second run
# ----------
threedPlot(solutions$run_2, variables = c("max", "mean", "median"),
           xlab = "Case Introduction Rate",
           ylab = "MMR Vaccination Rate",
           zlab = "Outbreak Length",
           title = "Latvia Run 2 - Older Case Introduction")
save(graph, file = "./output/Latvia/2_3d.dat")
# Looking at changes in outbreak length along MMR rate

## lowest introduction rate
twodPlot(solutions$run_2, "vacc", c("mean", "median", "iqr", "max"),
         "ins", 0.01, logy = TRUE,
          xlab = "MMR Vaccination Rate",
          ylab = "Outbreak Length (days)",
          title = "Latvia Run 2 - Older Case Introduction",
          sub = "0.01 introduction rate")
ggsave(filename="2_1.jpg", plot = graph, path = "./output/Latvia")

## Highest introduction rate
twodPlot(solutions$run_2, "vacc", c("mean", "median", "iqr", "max"),
         "ins", 0.1, logy = TRUE,
          xlab = "MMR Vaccination Rate",
          ylab = "Outbreak Length (days)",
          title = "Latvia Run 2 - Older Case Introduction",
          sub = "0.1 introduction rate")
ggsave(filename="2_2.jpg", plot = graph, path = "./output/Latvia")

# Third run
# ----------
threedPlot(solutions$run_3, variables = c("max", "mean", "median"),
           xlab = "Case Introduction Rate",
           ylab = "MMR Vaccination Rate",
           zlab = "Outbreak Length",
           title = "Latvia Run 3 - Younger Case Introduction")
save(graph, file = "./output/Latvia/2_3d.dat")
# Looking at changes in outbreak length along MMR rate

## lowest introduction rate
twodPlot(solutions$run_3, "vacc", c("mean", "median", "iqr", "max"),
         "ins", 0.01, logy = TRUE,
          xlab = "MMR Vaccination Rate",
          ylab = "Outbreak Length (days)",
          title = "Latvia Run 3 - Younger Case Introduction",
          sub = "0.01 introduction rate")
ggsave(filename="3_1.jpg", plot = graph, path = "./output/Latvia")

## Highest introduction rate
twodPlot(solutions$run_3, "vacc", c("mean", "median", "iqr", "max"),
         "ins", 0.1, logy = TRUE,
          xlab = "MMR Vaccination Rate",
          ylab = "Outbreak Length (days)",
          title = "Latvia Run 3 - Younger Case Introduction",
          sub = "0.1 introduction rate")
ggsave(filename="3_2.jpg", plot = graph, path = "./output/Latvia")

rm("graph", "plot_dat", "solutions")

#---Second Task--------------------
load("latvia_2.dat")

# First run
# ---------
# 3d Surface plot to see where to take cuts at
threedPlot(solutions$run_1, variables = c("max", "mean", "median"),
           xlab = "Case Introduction Rate",
           ylab = "MMR Vaccination Rate",
           zlab = "Outbreak Length",
           title = "Latvia Run 1 - 12 month delay")
save(graph, file = "./output/Latvia/4_3d.dat")
# Looking at changes in outbreak length along MMR rate

## lowest introduction rate
twodPlot(solutions$run_1, "vacc", c("mean", "median", "iqr", "max"),
         "ins", 0.01, logy = TRUE,
          xlab = "MMR Vaccination Rate",
          ylab = "Outbreak Length (days)",
          title = "Latvia Run 1 - 12 month delay",
          sub = "0.01 introduction rate")
ggsave(filename="4_1.jpg", plot = graph, path = "./output/Latvia")

## Highest introduction rate
twodPlot(solutions$run_1, "vacc", c("mean", "median", "iqr", "max"),
         "ins", 0.1, logy = TRUE,
          xlab = "MMR Vaccination Rate",
          ylab = "Outbreak Length (days)",
          title = "Latvia Run 1 - 12 month delay",
          sub = "0.1 introduction rate")
ggsave(filename="4_2.jpg", plot = graph, path = "./output/Latvia")

# Second run
# ----------
threedPlot(solutions$run_2, variables = c("max", "mean", "median"),
           xlab = "Case Introduction Rate",
           ylab = "MMR Vaccination Rate",
           zlab = "Outbreak Length",
           title = "Latvia Run 2 - 24 month delay")
save(graph, file = "./output/Latvia/5_3d.dat")
# Looking at changes in outbreak length along MMR rate

## lowest introduction rate
twodPlot(solutions$run_2, "vacc", c("mean", "median", "iqr", "max"),
         "ins", 0.01, logy = TRUE,
          xlab = "MMR Vaccination Rate",
          ylab = "Outbreak Length (days)",
          title = "Latvia Run 2 - 24 month delay",
          sub = "0.01 introduction rate")
ggsave(filename="5_1.jpg", plot = graph, path = "./output/Latvia")

## Highest introduction rate
twodPlot(solutions$run_2, "vacc", c("mean", "median", "iqr", "max"),
         "ins", 0.1, logy = TRUE,
          xlab = "MMR Vaccination Rate",
          ylab = "Outbreak Length (days)",
          title = "Latvia Run 2 - 24 month delay",
          sub = "0.1 introduction rate")
ggsave(filename="5_2.jpg", plot = graph, path = "./output/Latvia")

# Third run
# ----------
threedPlot(solutions$run_3, variables = c("max", "mean", "median"),
           xlab = "Case Introduction Rate",
           ylab = "MMR Vaccination Rate",
           zlab = "Outbreak Length",
           title = "Latvia Run 3 - 36 month delay")
save(graph, file = "./output/Latvia/6_3d.dat")
# Looking at changes in outbreak length along MMR rate

## lowest introduction rate
twodPlot(solutions$run_3, "vacc", c("mean", "median", "iqr", "max"),
         "ins", 0.01, logy = TRUE,
          xlab = "MMR Vaccination Rate",
          ylab = "Outbreak Length (days)",
          title = "Latvia Run 3 - 36 month delay",
          sub = "0.01 introduction rate")
ggsave(filename="6_1.jpg", plot = graph, path = "./output/Latvia")

## Highest introduction rate
twodPlot(solutions$run_3, "vacc", c("mean", "median", "iqr", "max"),
         "ins", 0.1, logy = TRUE,
          xlab = "MMR Vaccination Rate",
          ylab = "Outbreak Length (days)",
          title = "Latvia Run 3 - 36 month delay",
          sub = "0.1 introduction rate")
ggsave(filename="6_2.jpg", plot = graph, path = "./output/Latvia")

rm("graph", "plot_dat", "solutions")
