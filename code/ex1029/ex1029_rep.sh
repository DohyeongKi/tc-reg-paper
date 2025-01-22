#!/bin/bash
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=10
#SBATCH --nodes=1

R CMD BATCH --no-save "--args 0" ex1029_rep.R out/ex1029_rep_20.out
R CMD BATCH --no-save "--args -1" ex1029_rep.R out/ex1029_rep_21.out
R CMD BATCH --no-save "--args -2" ex1029_rep.R out/ex1029_rep_22.out
R CMD BATCH --no-save "--args -3" ex1029_rep.R out/ex1029_rep_23.out
R CMD BATCH --no-save "--args -4" ex1029_rep.R out/ex1029_rep_24.out
R CMD BATCH --no-save "--args -5" ex1029_rep.R out/ex1029_rep_25.out
R CMD BATCH --no-save "--args -6" ex1029_rep.R out/ex1029_rep_26.out
R CMD BATCH --no-save "--args -7" ex1029_rep.R out/ex1029_rep_27.out
R CMD BATCH --no-save "--args -8" ex1029_rep.R out/ex1029_rep_28.out