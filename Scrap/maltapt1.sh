#!/bin/bash
#$ -N Malta_1
#$ -cwd -V
#$ -M lsh1604217@student.lshtm.ac.uk
#$ -m e
#$ -l mem_free=1G,h_vmem=1G
#$ -q parallel.q
#$ -pe openmpi 64
#$ -R y
mpirun -np 64 R CMD BATCH maltapt1.R
