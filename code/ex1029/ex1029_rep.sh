#!/bin/bash
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=10
#SBATCH --nodes=1

R CMD BATCH --no-save "--args 0" ex1029_rep.R out/ex1029_rep_00.out
R CMD BATCH --no-save "--args -1" ex1029_rep.R out/ex1029_rep_01.out
R CMD BATCH --no-save "--args -2" ex1029_rep.R out/ex1029_rep_02.out
R CMD BATCH --no-save "--args -3" ex1029_rep.R out/ex1029_rep_03.out
R CMD BATCH --no-save "--args -4" ex1029_rep.R out/ex1029_rep_04.out
R CMD BATCH --no-save "--args -5" ex1029_rep.R out/ex1029_rep_05.out
R CMD BATCH --no-save "--args -6" ex1029_rep.R out/ex1029_rep_06.out
R CMD BATCH --no-save "--args -7" ex1029_rep.R out/ex1029_rep_07.out
R CMD BATCH --no-save "--args -8" ex1029_rep.R out/ex1029_rep_08.out