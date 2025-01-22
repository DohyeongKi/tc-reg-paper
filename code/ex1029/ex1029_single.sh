#!/bin/bash
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=5
#SBATCH --nodes=1

R CMD BATCH --no-save ex1029_single.R out/ex1029_single_00.out