#!/bin/bash
#$ -N Malta
#$ -cwd -V
#$ -M lsh1604217@student.lshtm.ac.uk
#$ -m e
#$ -l mem_free=1G,h_vmem=1G
#$ -q parallel.q
#$ -pe openmpi 64
#$ -R y
mpirun -np 64 R CMD BATCH malta.R
mpirun -np 64 R CMD BATCH maltapt1.R
mpirun -np 64 R CMD BATCH maltapt1_wc.R
mpirun -np 64 R CMD BATCH malta_wc.R
