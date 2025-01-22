rm(list = ls())
random_seed <- 2024L
set.seed(random_seed)

########################################################################
# Source, Library, and Data ############################################
source("../../axcon/axcon.R")
source("../../axcon/predict_axcon.R")

library(tidyverse)
library(regmdc)
library(caret)  # for cross-validation
library(foreach)  # for parallelization
library(doParallel)  # for parallelization
library(Sleuth3)  # the weekly-earnings data set
data(ex1029)

ex1029 <- ex1029 %>% 
  filter(Educ >= 8, Exper <= 40, Exper >= 1) %>% 
  filter(Race == 'NotBlack')
########################################################################

# Core Utilization #####################################################
num_cores <- 5L
registerDoParallel(num_cores)
########################################################################

########################################################################
# Sampling Ratio #######################################################
log2_sampling_ratio <- -2
sampling_ratio <- 2**log2_sampling_ratio
########################################################################

# The Number of Folds ##################################################
num_folds <- 10L
########################################################################

# Threshold ############################################################
threshold = 1e-4
########################################################################

########################################################################
# Data Cleaning ########################################################
sample <- sample(nrow(ex1029), size = nrow(ex1029) * sampling_ratio)
ex1029 <- ex1029[sample, ]

X_design <- ex1029 %>% 
  select(Educ, Exper) 
y <- log(ex1029[, 6])
########################################################################

########################################################################
# Axially Concave Regression ###########################################
axcon_model <- axcon(X_design, y, solver = "MOSEK")
########################################################################

########################################################################
# Totally Concave Regression ###########################################
tc_model <- regmdc(
  X_design, y, s = 2L, method = "tc", threshold = threshold
)
########################################################################

########################################################################
# Modified Version of Totally Concave Regression #######################
########################################################################
# Parameter searching
mod_tc_V_set <- c(1, 10, 100, 1000, 10000, 100000)
parameters <- expand.grid(V = mod_tc_V_set) 

# Perform k-fold cross-validation
k <- num_folds
folds <- createFolds(y, k = k, list = TRUE, returnTrain = FALSE)

mod_tc_cv_results <- foreach(para_index = 1L:nrow(parameters), .combine = 'rbind') %dopar% {
  V <- parameters[para_index, 1L]
  cv_pred_errors <- numeric(k)
  
  for (fold_index in (1L:k)) {
    X_cv_training <- X_design[-folds[[fold_index]], ]
    y_cv_training <- y[-folds[[fold_index]]]
    X_cv_test <- X_design[folds[[fold_index]], ]
    y_cv_test <- y[folds[[fold_index]]]
    
    tryCatch({
      # Build a model
      regmdc_model <- regmdc(
        X_cv_training, y_cv_training, s = 2L, method = "tcmars", 
        threshold = threshold, V,
        concave_covariates = c("Educ", "Exper"),
        variation_constrained_covariates = c("Educ", "Exper")
      )
      
      # Compute the fitted values at the cv-test data
      mod_tc_fit_cv_test <- predict_regmdc(regmdc_model, X_cv_test)
      
      # Compute the prediction error with the cv-test data
      cv_pred_errors[fold_index] <- mean((y_cv_test - mod_tc_fit_cv_test)**2)
    }, error = function(err) {
      cv_pred_errors[fold_index] <- NA
    })
  }
  
  c(V = V, cv_pred_error = mean(cv_pred_errors, na.rm = TRUE))
}

mod_tc_V_best <- mod_tc_cv_results[which.min(mod_tc_cv_results[, 2L]), 1L]

tryCatch({
  tryCatch({
    # Build a model
    mod_tc_model <- regmdc(
      X_design, y, s = 2L, method = "tcmars", threshold = threshold, 
      mod_tc_V_best,
      concave_covariates = c("Educ", "Exper"),
      variation_constrained_covariates = c("Educ", "Exper")
    )
  }, error = function(inner_err) {
    mod_tc_cv_results_sorted <- (
      mod_tc_cv_results[order(mod_tc_cv_results[, 2L], decreasing = FALSE), ]
    )
    .GlobalEnv$mod_tc_V_best <- mod_tc_cv_results_sorted[2L, 1L]
    
    .GlobalEnv$mod_tc_model <- regmdc(
      X_design, y, s = 2L, method = "tcmars", threshold = threshold, 
      .GlobalEnv$mod_tc_V_best,
      concave_covariates = c("Educ", "Exper"),
      variation_constrained_covariates = c("Educ", "Exper")
    )
  })
}, error = function(outer_err) {
  .GlobalEnv$mod_tc_model <- NA
})
########################################################################

########################################################################
# Output Results #######################################################
results <- list(
  data = ex1029,
  axcon_model = axcon_model,
  tc_model = tc_model,
  mod_tc_model = mod_tc_model,
  threshold = threshold,
  log2_sampling_ratio = log2_sampling_ratio,
  sampling_ratio = sampling_ratio,
  random_seed = random_seed
)

save(results, file = paste0("../../results/ex1029/ex1029_single_minus_log2_ratio", 
                            -log2_sampling_ratio, ".Rda"))
gc()
########################################################################
