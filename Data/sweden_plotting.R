require(ggplot2)
require(data.table)
source("../Current/Parameterized/plot.R")

#---First Task, lower vital dynamics--------------------
load("sweden_1.dat")

# First run
# ---------
# 3d Surface plot to see where to take cuts at
threedPlot(solutions$run_1, variables = c("max", "mean", "median"),
           xlab = "Case Introduction Rate",
           ylab = "MMR Vaccination Rate",
           zlab = "Outbreak Length",
           title = "Sweden Run 1 - Equal Case Introduction")
save(graph, file = "./output/Sweden/2000-2005/1_3d.dat")
# Looking at changes in outbreak length along MMR rate

## lowest introduction rate
twodPlot(solutions$run_1, "vacc", c("mean", "median", "iqr", "max"),
         "ins", 0.01, logy = TRUE,
          xlab = "MMR Vaccination Rate",
          ylab = "Outbreak Length (days)",
          title = "Sweden Run 1 - Equal Case Introduction",
          sub = "0.01 introduction rate")
ggsave(filename="1_1.jpg", plot = graph, path = "./output/Sweden/2000-2005")

## Highest introduction rate
twodPlot(solutions$run_1, "vacc", c("mean", "median", "iqr", "max"),
         "ins", 0.1, logy = TRUE,
          xlab = "MMR Vaccination Rate",
          ylab = "Outbreak Length (days)",
          title = "Sweden Run 1 - Equal Case Introduction",
          sub = "0.1 introduction rate")
ggsave(filename="1_2.jpg", plot = graph, path = "./output/Sweden/2000-2005")

# Second run
# ----------
threedPlot(solutions$run_2, variables = c("max", "mean", "median"),
           xlab = "Case Introduction Rate",
           ylab = "MMR Vaccination Rate",
           zlab = "Outbreak Length",
           title = "Sweden Run 2 - Older Case Introduction")
save(graph, file = "./output/Sweden/2000-2005/2_3d.dat")
# Looking at changes in outbreak length along MMR rate

## lowest introduction rate
twodPlot(solutions$run_2, "vacc", c("mean", "median", "iqr", "max"),
         "ins", 0.01, logy = TRUE,
          xlab = "MMR Vaccination Rate",
          ylab = "Outbreak Length (days)",
          title = "Sweden Run 2 - Older Case Introduction",
          sub = "0.01 introduction rate")
ggsave(filename="2_1.jpg", plot = graph, path = "./output/Sweden/2000-2005")

## Highest introduction rate
twodPlot(solutions$run_2, "vacc", c("mean", "median", "iqr", "max"),
         "ins", 0.1, logy = TRUE,
          xlab = "MMR Vaccination Rate",
          ylab = "Outbreak Length (days)",
          title = "Sweden Run 2 - Older Case Introduction",
          sub = "0.1 introduction rate")
ggsave(filename="2_2.jpg", plot = graph, path = "./output/Sweden/2000-2005")

# Third run
# ----------
threedPlot(solutions$run_3, variables = c("max", "mean", "median"),
           xlab = "Case Introduction Rate",
           ylab = "MMR Vaccination Rate",
           zlab = "Outbreak Length",
           title = "Sweden Run 3 - Younger Case Introduction")
save(graph, file = "./output/Sweden/2000-2005/2_3d.dat")
# Looking at changes in outbreak length along MMR rate

## lowest introduction rate
twodPlot(solutions$run_3, "vacc", c("mean", "median", "iqr", "max"),
         "ins", 0.01, logy = TRUE,
          xlab = "MMR Vaccination Rate",
          ylab = "Outbreak Length (days)",
          title = "Sweden Run 3 - Younger Case Introduction",
          sub = "0.01 introduction rate")
ggsave(filename="3_1.jpg", plot = graph, path = "./output/Sweden/2000-2005")

## Highest introduction rate
twodPlot(solutions$run_3, "vacc", c("mean", "median", "iqr", "max"),
         "ins", 0.1, logy = TRUE,
          xlab = "MMR Vaccination Rate",
          ylab = "Outbreak Length (days)",
          title = "Sweden Run 3 - Younger Case Introduction",
          sub = "0.1 introduction rate")
ggsave(filename="3_2.jpg", plot = graph, path = "./output/Sweden/2000-2005")

rm("graph", "plot_dat", "solutions")

#---Second Task, lower vital dynamics--------------------
load("sweden_2.dat")

# First run
# ---------
# 3d Surface plot to see where to take cuts at
threedPlot(solutions$run_1, variables = c("max", "mean", "median"),
           xlab = "Case Introduction Rate",
           ylab = "MMR Vaccination Rate",
           zlab = "Outbreak Length",
           title = "Sweden Run 1 - 12 month delay")
save(graph, file = "./output/Sweden/2000-2005/4_3d.dat")
# Looking at changes in outbreak length along MMR rate

## lowest introduction rate
twodPlot(solutions$run_1, "vacc", c("mean", "median", "iqr", "max"),
         "ins", 0.01, logy = TRUE,
          xlab = "MMR Vaccination Rate",
          ylab = "Outbreak Length (days)",
          title = "Sweden Run 1 - 12 month delay",
          sub = "0.01 introduction rate")
ggsave(filename="4_1.jpg", plot = graph, path = "./output/Sweden/2000-2005")

## Highest introduction rate
twodPlot(solutions$run_1, "vacc", c("mean", "median", "iqr", "max"),
         "ins", 0.1, logy = TRUE,
          xlab = "MMR Vaccination Rate",
          ylab = "Outbreak Length (days)",
          title = "Sweden Run 1 - 12 month delay",
          sub = "0.1 introduction rate")
ggsave(filename="4_2.jpg", plot = graph, path = "./output/Sweden/2000-2005")

# Second run
# ----------
threedPlot(solutions$run_2, variables = c("max", "mean", "median"),
           xlab = "Case Introduction Rate",
           ylab = "MMR Vaccination Rate",
           zlab = "Outbreak Length",
           title = "Sweden Run 2 - 24 month delay")
save(graph, file = "./output/Sweden/2000-2005/5_3d.dat")
# Looking at changes in outbreak length along MMR rate

## lowest introduction rate
twodPlot(solutions$run_2, "vacc", c("mean", "median", "iqr", "max"),
         "ins", 0.01, logy = TRUE,
          xlab = "MMR Vaccination Rate",
          ylab = "Outbreak Length (days)",
          title = "Sweden Run 2 - 24 month delay",
          sub = "0.01 introduction rate")
ggsave(filename="5_1.jpg", plot = graph, path = "./output/Sweden/2000-2005")

## Highest introduction rate
twodPlot(solutions$run_2, "vacc", c("mean", "median", "iqr", "max"),
         "ins", 0.1, logy = TRUE,
          xlab = "MMR Vaccination Rate",
          ylab = "Outbreak Length (days)",
          title = "Sweden Run 2 - 24 month delay",
          sub = "0.1 introduction rate")
ggsave(filename="5_2.jpg", plot = graph, path = "./output/Sweden/2000-2005")

# Third run
# ----------
threedPlot(solutions$run_3, variables = c("max", "mean", "median"),
           xlab = "Case Introduction Rate",
           ylab = "MMR Vaccination Rate",
           zlab = "Outbreak Length",
           title = "Sweden Run 3 - 36 month delay")
save(graph, file = "./output/Sweden/2000-2005/6_3d.dat")
# Looking at changes in outbreak length along MMR rate

## lowest introduction rate
twodPlot(solutions$run_3, "vacc", c("mean", "median", "iqr", "max"),
         "ins", 0.01, logy = TRUE,
          xlab = "MMR Vaccination Rate",
          ylab = "Outbreak Length (days)",
          title = "Sweden Run 3 - 36 month delay",
          sub = "0.01 introduction rate")
ggsave(filename="6_1.jpg", plot = graph, path = "./output/Sweden/2000-2005")

## Highest introduction rate
twodPlot(solutions$run_3, "vacc", c("mean", "median", "iqr", "max"),
         "ins", 0.1, logy = TRUE,
          xlab = "MMR Vaccination Rate",
          ylab = "Outbreak Length (days)",
          title = "Sweden Run 3 - 36 month delay",
          sub = "0.1 introduction rate")
ggsave(filename="6_2.jpg", plot = graph, path = "./output/Sweden/2000-2005")

rm("graph", "plot_dat", "solutions")

#---First Task, Higher vital dynamics--------------------
load("sweden_1_new.dat")

# First run
# ---------
# 3d Surface plot to see where to take cuts at
threedPlot(solutions$run_1, variables = c("max", "mean", "median"),
           xlab = "Case Introduction Rate",
           ylab = "MMR Vaccination Rate",
           zlab = "Outbreak Length",
           title = "Sweden Run 1 - Equal Case Introduction")
save(graph, file = "./output/Sweden/2010-2015/1_3d.dat")
# Looking at changes in outbreak length along MMR rate

## lowest introduction rate
twodPlot(solutions$run_1, "vacc", c("mean", "median", "iqr", "max"),
         "ins", 0.01, logy = TRUE,
          xlab = "MMR Vaccination Rate",
          ylab = "Outbreak Length (days)",
          title = "Sweden Run 1 - Equal Case Introduction",
          sub = "0.01 introduction rate")
ggsave(filename="1_1.jpg", plot = graph, path = "./output/Sweden/2010-2015")

## Highest introduction rate
twodPlot(solutions$run_1, "vacc", c("mean", "median", "iqr", "max"),
         "ins", 0.1, logy = TRUE,
          xlab = "MMR Vaccination Rate",
          ylab = "Outbreak Length (days)",
          title = "Sweden Run 1 - Equal Case Introduction",
          sub = "0.1 introduction rate")
ggsave(filename="1_2.jpg", plot = graph, path = "./output/Sweden/2010-2015")

# Second run
# ----------
threedPlot(solutions$run_2, variables = c("max", "mean", "median"),
           xlab = "Case Introduction Rate",
           ylab = "MMR Vaccination Rate",
           zlab = "Outbreak Length",
           title = "Sweden Run 2 - Older Case Introduction")
save(graph, file = "./output/Sweden/2010-2015/2_3d.dat")
# Looking at changes in outbreak length along MMR rate

## lowest introduction rate
twodPlot(solutions$run_2, "vacc", c("mean", "median", "iqr", "max"),
         "ins", 0.01, logy = TRUE,
          xlab = "MMR Vaccination Rate",
          ylab = "Outbreak Length (days)",
          title = "Sweden Run 2 - Older Case Introduction",
          sub = "0.01 introduction rate")
ggsave(filename="2_1.jpg", plot = graph, path = "./output/Sweden/2010-2015")

## Highest introduction rate
twodPlot(solutions$run_2, "vacc", c("mean", "median", "iqr", "max"),
         "ins", 0.1, logy = TRUE,
          xlab = "MMR Vaccination Rate",
          ylab = "Outbreak Length (days)",
          title = "Sweden Run 2 - Older Case Introduction",
          sub = "0.1 introduction rate")
ggsave(filename="2_2.jpg", plot = graph, path = "./output/Sweden/2010-2015")

# Third run
# ----------
threedPlot(solutions$run_3, variables = c("max", "mean", "median"),
           xlab = "Case Introduction Rate",
           ylab = "MMR Vaccination Rate",
           zlab = "Outbreak Length",
           title = "Sweden Run 3 - Younger Case Introduction")
save(graph, file = "./output/Sweden/2010-2015/2_3d.dat")
# Looking at changes in outbreak length along MMR rate

## lowest introduction rate
twodPlot(solutions$run_3, "vacc", c("mean", "median", "iqr", "max"),
         "ins", 0.01, logy = TRUE,
          xlab = "MMR Vaccination Rate",
          ylab = "Outbreak Length (days)",
          title = "Sweden Run 3 - Younger Case Introduction",
          sub = "0.01 introduction rate")
ggsave(filename="3_1.jpg", plot = graph, path = "./output/Sweden/2010-2015")

## Highest introduction rate
twodPlot(solutions$run_3, "vacc", c("mean", "median", "iqr", "max"),
         "ins", 0.1, logy = TRUE,
          xlab = "MMR Vaccination Rate",
          ylab = "Outbreak Length (days)",
          title = "Sweden Run 3 - Younger Case Introduction",
          sub = "0.1 introduction rate")
ggsave(filename="3_2.jpg", plot = graph, path = "./output/Sweden/2010-2015")

rm("graph", "plot_dat", "solutions")

#---Second Task, Higher vital dynamics--------------------
load("sweden_2_new.dat")

# First run
# ---------
# 3d Surface plot to see where to take cuts at
threedPlot(solutions$run_1, variables = c("max", "mean", "median"),
           xlab = "Case Introduction Rate",
           ylab = "MMR Vaccination Rate",
           zlab = "Outbreak Length",
           title = "Sweden Run 1 - 12 month delay")
save(graph, file = "./output/Sweden/2010-2015/4_3d.dat")
# Looking at changes in outbreak length along MMR rate

## lowest introduction rate
twodPlot(solutions$run_1, "vacc", c("mean", "median", "iqr", "max"),
         "ins", 0.01, logy = TRUE,
          xlab = "MMR Vaccination Rate",
          ylab = "Outbreak Length (days)",
          title = "Sweden Run 1 - 12 month delay",
          sub = "0.01 introduction rate")
ggsave(filename="4_1.jpg", plot = graph, path = "./output/Sweden/2010-2015")

## Highest introduction rate
twodPlot(solutions$run_1, "vacc", c("mean", "median", "iqr", "max"),
         "ins", 0.1, logy = TRUE,
          xlab = "MMR Vaccination Rate",
          ylab = "Outbreak Length (days)",
          title = "Sweden Run 1 - 12 month delay",
          sub = "0.1 introduction rate")
ggsave(filename="4_2.jpg", plot = graph, path = "./output/Sweden/2010-2015")

# Second run
# ----------
threedPlot(solutions$run_2, variables = c("max", "mean", "median"),
           xlab = "Case Introduction Rate",
           ylab = "MMR Vaccination Rate",
           zlab = "Outbreak Length",
           title = "Sweden Run 2 - 24 month delay")
save(graph, file = "./output/Sweden/2010-2015/5_3d.dat")
# Looking at changes in outbreak length along MMR rate

## lowest introduction rate
twodPlot(solutions$run_2, "vacc", c("mean", "median", "iqr", "max"),
         "ins", 0.01, logy = TRUE,
          xlab = "MMR Vaccination Rate",
          ylab = "Outbreak Length (days)",
          title = "Sweden Run 2 - 24 month delay",
          sub = "0.01 introduction rate")
ggsave(filename="5_1.jpg", plot = graph, path = "./output/Sweden/2010-2015")

## Highest introduction rate
twodPlot(solutions$run_2, "vacc", c("mean", "median", "iqr", "max"),
         "ins", 0.1, logy = TRUE,
          xlab = "MMR Vaccination Rate",
          ylab = "Outbreak Length (days)",
          title = "Sweden Run 2 - 24 month delay",
          sub = "0.1 introduction rate")
ggsave(filename="5_2.jpg", plot = graph, path = "./output/Sweden/2010-2015")

# Third run
# ----------
threedPlot(solutions$run_3, variables = c("max", "mean", "median"),
           xlab = "Case Introduction Rate",
           ylab = "MMR Vaccination Rate",
           zlab = "Outbreak Length",
           title = "Sweden Run 3 - 36 month delay")
save(graph, file = "./output/Sweden/2010-2015/6_3d.dat")
# Looking at changes in outbreak length along MMR rate

## lowest introduction rate
twodPlot(solutions$run_3, "vacc", c("mean", "median", "iqr", "max"),
         "ins", 0.01, logy = TRUE,
          xlab = "MMR Vaccination Rate",
          ylab = "Outbreak Length (days)",
          title = "Sweden Run 3 - 36 month delay",
          sub = "0.01 introduction rate")
ggsave(filename="6_1.jpg", plot = graph, path = "./output/Sweden/2010-2015")

## Highest introduction rate
twodPlot(solutions$run_3, "vacc", c("mean", "median", "iqr", "max"),
         "ins", 0.1, logy = TRUE,
          xlab = "MMR Vaccination Rate",
          ylab = "Outbreak Length (days)",
          title = "Sweden Run 3 - 36 month delay",
          sub = "0.1 introduction rate")
ggsave(filename="6_2.jpg", plot = graph, path = "./output/Sweden/2010-2015")

rm("graph", "plot_dat", "solutions")
