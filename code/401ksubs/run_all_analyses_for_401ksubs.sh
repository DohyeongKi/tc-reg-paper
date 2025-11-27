#!/bin/bash
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=10
#SBATCH --nodes=1

########################################################################
# Experiment for the Retirement Saving Plan (401(k)) Data ##############
# Experiment without the regularized variants
R CMD BATCH --no-save 401ksubs_rep.R out/401ksubs_rep_01.out
# Experiment for the regularized variants
R CMD BATCH --no-save 401ksubs_mod_rep.R out/401ksubs_mod_rep_01.out
# Results are saved in `../../results/401ksubs`

# Generate Figure 6 in Section 9.3. Plot is saved in `../../plots/401ksubs`.
Rscript -e "rmarkdown::render('401ksubs_rank_plot.Rmd')"
########################################################################