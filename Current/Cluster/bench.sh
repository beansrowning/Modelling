#!/bin/bash
#$ -cwd -V
#$ -l mem_free=1G,h_vmem=1G
#$ -q parallel.q
#$ -pe smp 8
#$ -R y
R --vanilla -f ../gsbench.R