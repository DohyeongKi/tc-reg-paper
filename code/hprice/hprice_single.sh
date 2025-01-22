#!/bin/bash
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=10
#SBATCH --nodes=1

R CMD BATCH --no-save hprice_single.R out/hprice_single_01.out