# Modelling

A MSc. Epi summer project _(sometimes)_ building stochastic models in R By: Sean Browning, LSHTM 2017

## Vision

A robust and intuitve model of measles transmission in Europe to answer fundamental questions about elimination conditions.

## Credo

1. Break it 'till it works
2. Optimize now, sleep later
3. Beautiful things work better

## Scope

- The project is a stochastic SEIR measles model built on the **adaptivetau** library and optimized with a parallel backend through **foreach** and its associated packages.
- Visualizations are accomplished with the _ggplot2_ package.

## Dependent Libraries

- Adaptivetau (model backend)
- data.table (for optimized subsetting)
- foreach (for batch model runs)
- doParallel (parallel computing backend)
- ggplot2 (visualizations)
- Rcpp (and R Tools / gcc)

## Installation

- Clone the repo or download just the 'Current' folder.
- Run '**init**.R' from R to load all data and functions
- If R fails to compile external code, install the package from the 'Data' folder instead

## Directories

### Current

Files being actively developed, currently:

- Data : Model data extrapolated from the public domain
- multicore : Batch model run function
- Epi_detect : Analyzes data for epidemics lasting longer than 365 days
- /Src : Any C++ or FORTRAN functions or subroutines called from R

  ### Scrap

   Depreciated files for reference and not much else

  ### Data

   Run data or data produced from function optimizing
