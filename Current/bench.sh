#!/bin/bash
#$ -N MPI Solution Benchmark
#$ -cwd -V
#$ -M lsh1604217@student.lshtm.ac.uk
#$ -m e
#$ -l mem_free=1G,h_vmem=1G
#$ -q parallel.q
#$ -pe openmpi 33
#$ -R y
mpirun -np 1 R --vanilla -f gsbench.R 
