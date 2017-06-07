# Modelling
A MSc. Epi summer project *(sometimes)* building stochastic models in R
By: Sean Browning, LSHTM 2017

## Scope
  * The project is a stochastic SEIR measles model built on the **adaptivetau**
    library and optimized with a parallel backend through **foreach** and its
    associated packages.
  * Visualizations were originally done with google visualizations using the R *googleVis*
    package, but this is now accomplished with *ggplot2*.

## Dependent Libraries
* Adaptivetau (model backend)
* data.table (for optimized subsetting)
* foreach (for batch model runs)
* doParallel (parallel computing backend)
* ggplot2 (visualizations)

## Directories
### Current
  Files being actively developed, currently:
  * Data : Initialized model data extrapolated from the public domain
  * multicore : Batch model run function
  * Epi_detect : Analyzes data for epidemics lasting longer than 365 days
### Scrap
  Depreciated files for reference and not much else
### Data
  Run data or data produced from function optimizing
