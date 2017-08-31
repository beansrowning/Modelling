#!/bin/bash
#$ -N Malta
#$ -cwd -V
#$ -M lsh1604217@student.lshtm.ac.uk
#$ -m e
#$ -l mem_free=1G,h_vmem=1G
#$ -q parallel.q
#$ -pe openmpi 44
#$ -R y
mpirun -np 44 R CMD BATCH malta.R 
