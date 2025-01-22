# Totally Concave Regression

This repository contains the code for the numerical experiments in the 
paper [Totally Concave Regression](https://arxiv.org/abs/2501.04360). 
All R scripts for the experiments are located in the folder `code`, 
along with the shell scripts used for running them on our server. The 
role of each R script is described below. Running these R scripts 
requires the R package `regmdc`. Please refer to this 
[page](https://github.com/DohyeongKi/regmdc) for how to install it. We 
used version 0.8.1 of the package for the experiments. The results and 
plots produced by the experiments are also stored in the folders 
`results` and `plots`. Furthermore, our implementation for axially 
concave regression can be found in the folder `axcon`.


## R Script Description by Dataset

### ex1029

- **`ex1029_eda.Rmd`**: Draws locally smoothed partial plots.
- **`ex1029_single.R`**: Fits each model to a single set of sub-samples.
- **`ex1029_reg_fit.Rmd`**: Draws the partial plots of the fits produced 
by `ex1029_single.R`.
- **`ex1029_rep.R`**: Conducts the main experiment for the dataset.
- **`ex1029_rank_plot.Rmd`**: Creates the plot of the best-performing 
ratios.

### hprice

- **`hprice_rep.R`** and **`hprice_mod_rep.R`**: Conduct the main 
experiment for the dataset.
- **`hprice_rank_plot.Rmd`**: Creates the plots of the empirical 
cumulative distributions of the ranks.
- **`hprice_single.R`**: Fits each model to a single training set.
- **`hprice_plot.R`**: Draws 3D plots for the fits produced by 
`hprice_single.R`.

### 401ksubs

- **`401ksubs_rep.R`** and **`401ksubs_mod_rep.R`**: Conduct the main 
experiment for the dataset.
- **`401ksubs_rank_plot.Rmd`**: Creates the plot of empirical cumulative 
distributions of the ranks.