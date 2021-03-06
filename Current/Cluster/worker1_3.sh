#!/bin/bash
#$ -N Worker_1
#$ -cwd -V
#$ -M lsh1604217@student.lshtm.ac.uk
#$ -m e
#$ -l mem_free=1.5G,h_vmem=1.5G
#$ -q parallel.q
#$ -pe openmpi 64
#$ -R y
mpirun -np 64 R CMD BATCH sweden.R
mpirun -np 64 R CMD BATCH malta.R
mpirun -np 64 R CMD BATCH latvia.R
