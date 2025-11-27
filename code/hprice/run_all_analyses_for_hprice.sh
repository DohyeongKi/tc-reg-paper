#!/bin/bash
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=10
#SBATCH --nodes=1

########################################################################
# Experiment for the Housing Price Data ################################
# Experiment without the regularized variants
R CMD BATCH --no-save hprice_rep.R out/hprice_rep_01.out
# Experiment for the regularized variants
R CMD BATCH --no-save hprice_mod_rep.R out/hprice_mod_rep_01.out
# Experiment on an overfitting instance
R CMD BATCH --no-save hprice_single.R out/hprice_single_01.out
# Experiment results are saved in `../../results/hprice`

# Generate Figures 3 and 5 in Section 9.2
Rscript -e "rmarkdown::render('hprice_rank_plot.Rmd')"
# Generate Figure 4 in Section 9.2. Plots are saved in `../../plots/hprice`.
R CMD BATCH --no-save hprice_plot.R out/hprice_plot_01.out
########################################################################