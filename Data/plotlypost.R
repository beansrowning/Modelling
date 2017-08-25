Sys.setenv("plotly_username"="beansrowning")
Sys.setenv("plotly_api_key"="JjiwmDmYu4pZIx40sedg")
require(plotly)
load("gridsearch2.dat")
source("../Current/Parameterized/plot.R")
swe <- new.env()
threedPlot(solutions$run_1,
            variables =c("max"),
            xlab = "Case Insertion Rate",
            ylab = "MMR Vaccination Rate",
            zlab = "Outbreak Length (days)",
            title = "Sweden - Grid Search Run 1")
swe$p1 <- plot
swe$pd1 <- plot_dat

threedPlot(solutions$run_2,
            variables =c("max"),
            xlab = "Case Insertion Rate",
            ylab = "MMR Vaccination Rate",
            zlab = "Outbreak Length (days)",
            title = "Sweden - Grid Search Run 2")
swe$p2 <- plot
swe$pd2 <- plot_dat

threedPlot(solutions$run_3,
            variables =c("max"),
            xlab = "Case Insertion Rate",
            ylab = "MMR Vaccination Rate",
            zlab = "Outbreak Length (days)",
            title = "Sweden - Grid Search Run 3")
swe$p3 <- plot
swe$pd3 <- plot_dat
rm("plot", "plot_dat")
save(swe, file="Sweden_run1.dat")
# api_create(swe$p1, filename = "Sweden Equal Introduction", fileopt = "overwrite")
# api_create(swe$p2, filename = "Swedent Older Introduction", fileopt = "overwrite")
# api_create(swe$p3, filename = "Sweden Younger Introduction", fileopt = "overwrite")
