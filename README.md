# Modelling
An MSc. Epi summer project *(sometimes)* building stochastic models in R

##Dependent Libraries
* Adaptivetau
* googleVis (js embedded html visualization)

##Files
* Base_SEIR - **Working**

A single population SEIR model of Measles, googleVis output

* 1_SEIR - **Working**

SEIR model with birth, deaths, and vaccinations. Rates are approximated from
worldbank and WHO data on the EU from 2016.

* 3_SEIR - **Work in progress**

1_SEIR scaled up to 3 populations. Working out the rate function and transitions
to see if a single transition coupled with multiple rate functions is more optimal.

* alternateSIRpresentation

ggplot2 and matplot visualization code scrapped from an early model

* modelvis_wip - **Empty**

Allocated space for working on visualization code seprate from main file

* seasonalbetawip - **Broken**

An investigation in seasonal changes in transmission modeled using a cosine function. *(spoiler: that was a terrible idea)*  
Also began reworking the rate function to accept more tangible numbers (R0, birthrate, etc.) for ease of understanding.

* WIP - **Spaghetti**

Conglomerated 3 population SIR mess built from day one. Rate function needs work and the inputs are unwieldy.
