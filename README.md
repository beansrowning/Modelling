# Modelling
An MSc. Epi summer project *(sometimes)* building stochastic models in R

##Dependent Libraries
* Adaptivetau
* googleVis (html visualization output through json)

##Files
* Base_SEIR - **Working**
A single population SEIR model of Measles, googleVis output
* 1_SEIR - **Working**
SEIR model with birth, deaths, and vaccinations
* alternateSIRpresentation
ggplot2 and matplot visualization code scrapped from an early model
* modelvis_wip - **Empty**
Allocated space for working on visualization code seprate from main file
* seasonalbetawip - **Broken**
An investigation in seasonal changes in transmission modeled using a cosine function. *(spoiler: that was a terrible idea)*
Also began reworking the rate function to accept more tangible numbers (R0, birthrate, etc.) for ease of understanding.
* WIP - **Spaghetti**
Conglomerated 3 population SIR mess built from day one. Rate function needs work and the inputs are unwieldy.
