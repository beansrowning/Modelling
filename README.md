# Modelling
A MSc. Epi summer project *(sometimes)* building stochastic models in R
By: Sean Browning LSHTM 2017

##Dependent Libraries
* Adaptivetau
* googleVis (js embedded html visualization)

##Files
* Base_SEIR - **Working**

A single population SEIR model of Measles, googleVis output

* 1_SEIR - **Working**

SEIR model with birth, deaths, and vaccinations. Rates are approximated from
worldbank and WHO data on the EU from 2016.

* 3_SEIR - **Working**

1_SEIR scaled up to 3 populations. Rate function has been worked out,
ended up combining 3 infection transitions into a single one and adding the rate functions
up in a single line. Birth and Death rates are universal for the 3 populations, which needs to change.

* modelvis - **Working**

Visualization function through gVis required for all new models including 3_SEIR.
Will start a new branch for any other progress on it

* alternateSIRpresentation - **Depreciated**

ggplot2 and matplot visualization code scrapped from an early model

* seasonalbetawip - **Broken**

An investigation in seasonal changes in transmission modeled using a cosine function. *(spoiler: that was a terrible idea)*  
Also began reworking the rate function to accept more tangible numbers (R0, birthrate, etc.) for ease of understanding.

* Beginning - **Spaghetti**

Conglomerated 3 population SIR mess built from day one. Rate function needs work and the inputs are unwieldy.
I haven't quite scrapped it, but it served as more of a thought experiment and crash course in R.
