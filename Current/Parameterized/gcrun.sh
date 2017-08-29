#!/usr/bin/bash
# Run Malta and Latvia Simulations
# Then close the instance
export PATH=~/R/R-3.4.1/bin:$PATH
git pull
R --vanilla -f latvia.R
R --vanilla -f malta.R
git add ../../Data/malta2.dat
git add ../../Data/latvia2.dat
git commit -m"Malta & Latvia runs 2 done"
gcloud compute instances stop debian-1 --zone=europe-west1-b
