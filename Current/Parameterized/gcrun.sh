#!/usr/bin/bash
# Run Malta and Latvia Simulations
# Then close the instance
git pull
R --vanilla -f latvia.R
R --vanilla -f malta.R
git add ../../Data/malta.dat
git add ../../Data/latvia.dat
git commit -m"Google cloud compute all done"
gcloud compute instances stop debian-1 --zone=europe-west1-b