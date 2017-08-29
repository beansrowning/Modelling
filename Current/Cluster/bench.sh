#!/bin/bash
#$ -cwd -V
#$ -M lsh1604217@student.lshtm.ac.uk
#$ -m n
#$ -l mem_free=1G,h_vmem=1G
#$ -q parallel.q
#$ -pe smp 12
#$ -R y
R CMD BATCH ../gsbench.R