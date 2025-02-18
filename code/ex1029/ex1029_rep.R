rm(list = ls())
args = commandArgs(trailingOnly=TRUE)
set.seed(2024L)

########################################################################
# Source, Library, and Data ############################################
source("../../axcon/axcon.R")
source("../../axcon/predict_axcon.R")

library(tidyverse)
library(regmdc)
library(foreach)  # for parallelization
library(doParallel)  # for parallelization
library(Sleuth3)  # the weekly-earnings data set
data(ex1029)

ex1029 <- ex1029 %>% 
  filter(Educ >= 8, Exper <= 40, Exper >= 1) %>% 
  filter(Race == 'NotBlack')

X <- ex1029 %>% 
  select(Educ, Exper)
y <- log(ex1029[, 6])
########################################################################

########################################################################
# Core Utilization #####################################################
num_cores <- 10L
#num_cores <- 2L
registerDoParallel(num_cores)
########################################################################

########################################################################
# The Number of Repetitions ############################################
num_rep <- 100L
#num_rep <- 5L
########################################################################

########################################################################
# Sampling Ratio #######################################################
log2_sampling_ratio <- as.numeric(args[1L])
sampling_ratio <- 2**log2_sampling_ratio
########################################################################

########################################################################
# The Ratio of Training Sets ###########################################
training_ratio <- 0.9
########################################################################

# Threshold ############################################################
threshold = 1e-4
########################################################################

# Optimization Problem Solver ##########################################
solver = "MOSEK"
#solver = NULL
########################################################################

########################################################################
estimation_results <- foreach(rep = 1L:num_rep, .combine = 'rbind') %dopar% {
  ######################################################################
  # Data Sub-sampling and Split ########################################
  set.seed(2024L + rep)
  
  training <- sample(length(y), size = length(y) * sampling_ratio * training_ratio)
  
  X_training <- X[training, ]
  y_training <- y[training]
  X_test <- X[-training, ]
  y_test <- y[-training]
  ######################################################################
  
  ######################################################################
  # Mincer's Model #####################################################
  df <- data.frame(y = y_training, X_training)
  
  # Build a model
  mincer_model <- lm(
    y ~ Educ + Exper + I(Exper^2), 
    data = df
  )
  
  # Compute the fitted values at the test set
  mincer_fit_test <- as.vector(predict(mincer_model, X_test))
  
  # Compute the prediction error with the test set
  mincer_pred_error <- mean((y_test - mincer_fit_test)**2)
  ######################################################################
  
  ######################################################################
  # Murphy and Welch's Model ###########################################
  # Build a model
  murphy_model <- lm(
    y ~ Educ + Exper + I(Exper^2) + I(Exper^3) + I(Exper^4), 
    data = df
  )
  
  # Compute the fitted values at the test set
  murphy_fit_test <- as.vector(predict(murphy_model, X_test))
  
  # Compute the prediction error with the test set
  murphy_pred_error <- mean((y_test - murphy_fit_test)**2)
  ######################################################################
  
  ######################################################################
  # Axially Concave Regression #########################################
  tryCatch({
    # Build a model
    axcon_model <- axcon(X_training, y_training, solver = solver)
    
    # Compute the fitted values at the test set
    axcon_fit_test <- as.vector(predict_axcon(axcon_model, X_test))
    
    # Compute the prediction error with the test set
    axcon_pred_error <- mean((y_test - axcon_fit_test)**2)
  }, error = function(err) {
    .GlobalEnv$axcon_pred_error <- NA
  })
  ######################################################################
  
  ######################################################################
  # Totally Concave Regression #########################################
  tryCatch({
    # Build a model
    regmdc_model <- regmdc(
      X_training, y_training, s = 2L, method = "tc", threshold = threshold
    )
    
    # Compute the fitted values at the test set
    tc_fit_test <- as.vector(predict_regmdc(regmdc_model, X_test))
    
    # Compute the prediction error with the test set
    tc_pred_error <- mean((y_test - tc_fit_test)**2)
  }, error = function(err) {
    .GlobalEnv$tc_pred_error <- NA
  })
  ######################################################################
  
  ######################################################################
  # Additive Concave Regression ########################################
  tryCatch({
    # Build a model
    additive_model <- regmdc(
      X_training, y_training, s = 1L, method = "tc", threshold = threshold
    )
    
    # Compute the fitted values at the test set
    additive_fit_test <- as.vector(predict_regmdc(additive_model, X_test))
    
    # Compute the prediction error with the test set
    additive_pred_error <- mean((y_test - additive_fit_test)**2)
  }, error = function(err) {
    .GlobalEnv$additive_pred_error <- NA
  })
  ######################################################################
  
  c(mincer = mincer_pred_error, 
    murphy = murphy_pred_error,
    axcon = axcon_pred_error,
    tc = tc_pred_error,
    additive = additive_pred_error)
}

estimation_results_no_na <- na.omit(estimation_results)

avg <- apply(estimation_results_no_na, MARGIN = 2L, FUN = mean)
se <- apply(estimation_results_no_na, MARGIN = 2L, FUN = function(x) {
  sqrt(var(x) / length(x))
})

quartiles <- do.call(cbind, lapply(seq(ncol(estimation_results)), function(col) {
  quantile(estimation_results_no_na[, col], prob = seq(0, 1, by = 0.25))
}))

colnames(quartiles) <- colnames(estimation_results)

summary_statistics <- rbind(avg, se, quartiles)

########################################################################
# Output Results #######################################################
results <- list(
  estimation_results = estimation_results,
  summary_statistics = summary_statistics,
  num_rep = num_rep,
  log2_sampling_ratio = log2_sampling_ratio,
  sampling_ratio = sampling_ratio,
  training_ratio = training_ratio,
  solver = solver
)

save(results, 
     file = paste0("../../results/ex1029/ex1029_rep_minus_log2_ratio", 
                   -log2_sampling_ratio, ".Rda"))
gc()
########################################################################
