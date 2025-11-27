#!/bin/bash
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=10
#SBATCH --nodes=1

R CMD BATCH --no-save hprice_rep.R out/hprice_rep_01.out