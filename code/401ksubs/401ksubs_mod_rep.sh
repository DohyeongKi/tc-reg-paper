#!/bin/bash
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=10
#SBATCH --nodes=1

R CMD BATCH --no-save 401ksubs_mod_rep.R out/401ksubs_mod_rep_01.out