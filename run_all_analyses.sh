#!/bin/bash
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=10
#SBATCH --nodes=1

########################################################################
# Experiment for the Earnings Data #####################################
cd code/ex1029
R CMD BATCH --no-save "--args 0" ex1029_rep.R out/ex1029_rep_00.out
R CMD BATCH --no-save "--args -1" ex1029_rep.R out/ex1029_rep_01.out
R CMD BATCH --no-save "--args -2" ex1029_rep.R out/ex1029_rep_02.out
R CMD BATCH --no-save "--args -3" ex1029_rep.R out/ex1029_rep_03.out
R CMD BATCH --no-save "--args -4" ex1029_rep.R out/ex1029_rep_04.out
R CMD BATCH --no-save "--args -5" ex1029_rep.R out/ex1029_rep_05.out
R CMD BATCH --no-save "--args -6" ex1029_rep.R out/ex1029_rep_06.out
R CMD BATCH --no-save "--args -7" ex1029_rep.R out/ex1029_rep_07.out
R CMD BATCH --no-save "--args -8" ex1029_rep.R out/ex1029_rep_08.out
# Experiment results are saved in `results/ex1029`

# Generate Figure 2 in Section 9.1 and Figures 7 and 8 in the 
# supplementary material. Plots are saved in `plots/ex1029`.
Rscript -e "rmarkdown::render('ex1029_rank_plot.Rmd')"
########################################################################

########################################################################
# Experiment for the Housing Price Data ################################
cd ../hprice
# Experiment without the regularized variants
R CMD BATCH --no-save hprice_rep.R out/hprice_rep_01.out
# Experiment for the regularized variants
R CMD BATCH --no-save hprice_mod_rep.R out/hprice_mod_rep_01.out
# Experiment on an overfitting instance
R CMD BATCH --no-save hprice_single.R out/hprice_single_01.out
# Experiment results are saved in `results/hprice`

# Generate Figures 3 and 5 in Section 9.2
Rscript -e "rmarkdown::render('hprice_rank_plot.Rmd')"
# Generate Figure 4 in Section 9.2. Plots are saved in `plots/hprice`.
R CMD BATCH --no-save hprice_plot.R out/hprice_plot_01.out
########################################################################

########################################################################
# Experiment for the Retirement Saving Plan (401(k)) Data ##############
cd ../401ksubs
# Experiment without the regularized variants
R CMD BATCH --no-save 401ksubs_rep.R out/401ksubs_rep_01.out
# Experiment for the regularized variants
R CMD BATCH --no-save 401ksubs_mod_rep.R out/401ksubs_mod_rep_01.out
# Results are saved in `results/401ksubs`

# Generate Figure 6 in Section 9.3. Plot is saved in `plots/401ksubs`.
Rscript -e "rmarkdown::render('401ksubs_rank_plot.Rmd')"
########################################################################