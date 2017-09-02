# Plots and Charts

This folder contains the graphs of each nation and their respective runs.

The first numeral represents the run number, while the second represents the
case introduction rate being looked at. :
- X_1 : 0.01 case introduction rate
- X_2 : 0.1 case introduction rate

3d Surface plots are stored in dat files, which can be loaded into R with the
`load()` function, and viewed with the plot_ly package.

Comination plots were also made and follow the same naming structure as above.
They look solely at the mean outbreak length between runs. 

The plotting functions used to produce all the plots can be found in the
`Current/Parameterized` folder in the `plot.R` file

A Sample
![example](https://github.com/beansrowning/Modelling/blob/master/Data/output/Latvia/Base/6_1.jpg)
