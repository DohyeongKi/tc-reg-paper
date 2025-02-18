rm(list = ls())
set.seed(2024L)

########################################################################
# Source, Library, and Data ############################################
library(tidyverse)
library(regmdc)
library(caret)  # for cross-validation
library(foreach)  # for parallelization
library(doParallel)  # for parallelization
library(wooldridge)  # the k401ksubs data set
data("k401ksubs")

k401ksubs <- k401ksubs %>% 
  mutate(
    fsize1 = ifelse(fsize == 1, 1, 0), 
    fsize2 = ifelse(fsize == 2, 1, 0), 
    fsize3 = ifelse(fsize == 3, 1, 0), 
    fsize4 = ifelse(fsize == 4, 1, 0),
    fsize5 = ifelse(fsize >= 5, 1, 0)
  ) %>% 
  select(nettfa, inc, age, male, e401k, fsize1, fsize2, fsize3, fsize4, fsize5)
########################################################################

########################################################################
# Core Utilization #####################################################
num_cores <- 5L
#num_cores <- 2L
registerDoParallel(num_cores)
########################################################################

########################################################################
# The Number of Repetitions ############################################
num_rep <- 100L
#num_rep <- 5L
########################################################################

########################################################################
# The Ratio of Training Sets ###########################################
training_ratio <- 0.9
########################################################################

########################################################################
# The Number of Folds ##################################################
num_folds <- 10L
########################################################################

# The Number of Bins for Approximation #################################
num_bins <- 50L
########################################################################

# Threshold ############################################################
threshold = 1e-4
########################################################################
  
########################################################################
estimation_results <- foreach(rep = 1L:num_rep, .combine = 'rbind') %dopar% {
  ######################################################################
  # Data Split #########################################################
  set.seed(2024L + rep)
  
  training <- sample(nrow(k401ksubs), size = nrow(k401ksubs) * training_ratio)
  
  k401ksubs_training <- k401ksubs[training, ]
  k401ksubs_test <- k401ksubs[-training, ]
  ######################################################################
  
  ######################################################################
  # Quadratic Model ####################################################
  # Build a model
  quadratic_model <- lm(
    nettfa ~ (
      inc + I(inc**2) + age + I(age**2) + e401k 
      + fsize1 + fsize2 + fsize3 + fsize4
    ),
    data = k401ksubs_training
  )
  
  # Compute the fitted values at the test set
  quadratic_fit_test <- as.vector(predict(quadratic_model, k401ksubs_test))
  
  # Compute the prediction error with the test set
  quadratic_pred_error <- mean((k401ksubs_test$nettfa - quadratic_fit_test)**2)
  ######################################################################
  
  ######################################################################
  # Our Model 2 (Reg.) #################################################
  # Data ###############################################################
  X_training <- k401ksubs_training %>%
    select(inc, age, e401k, fsize1, fsize2, fsize3, fsize4)
  y_training <- k401ksubs_training$nettfa
  X_test <- k401ksubs_test %>%
    select(inc, age, e401k, fsize1, fsize2, fsize3, fsize4)
  y_test <- k401ksubs_test$nettfa
  ######################################################################
  ######################################################################
  # Parameter searching
  our2_mod_V_set <- c(1000, 5000, 10000)
  parameters <- expand.grid(V = our2_mod_V_set) 
  
  # Perform k-fold cross-validation
  k <- num_folds
  folds <- createFolds(y_training, k = k, list = TRUE, returnTrain = FALSE)
  
  our2_mod_cv_results <- matrix(, nrow = length(our2_mod_V_set), ncol = 2L)
  our2_mod_cv_results[, 1L] <- our2_mod_V_set
  
  for (para_index in (1L:nrow(parameters))) {
    V <- parameters[para_index, 1L]
    cv_pred_errors <- numeric(k)
    
    for (fold_index in (1L:k)) {
      X_cv_training <- X_training[-folds[[fold_index]], ]
      y_cv_training <- y_training[-folds[[fold_index]]]
      X_cv_test <- X_training[folds[[fold_index]], ]
      y_cv_test <- y_training[folds[[fold_index]]]
      
      tryCatch({
        # Build a model
        regmdc_model <- regmdc(
          X_cv_training, y_cv_training, s = 2L, method = "tcmars",
          threshold = threshold, V,
          number_of_bins = c(num_bins, NA, NA, NA, NA, NA, NA),
          convex_covariates = c("inc", "age", "e401k"),
          concave_covariates = "e401k",
          variation_constrained_covariates = c("inc", "age"),
          extra_linear_covariates = c("fsize1", "fsize2", "fsize3", "fsize4")
        )
        
        # Compute the fitted values at the cv-test data
        our_fit_cv_test <- predict_regmdc(regmdc_model, X_cv_test)
        
        # Compute the prediction error with the cv-test data
        cv_pred_errors[fold_index] <- mean((y_cv_test - our_fit_cv_test)**2)
      }, error = function(err) {
        cv_pred_errors[fold_index] <- NA
      })
    }
    
    our2_mod_cv_results[para_index, 2L] <- mean(cv_pred_errors, na.rm = TRUE)
  }
  
  our2_mod_V_best <- our2_mod_cv_results[which.min(our2_mod_cv_results[, 2L]), 1L]
  
  tryCatch({
    tryCatch({
      # Build a model
      our2_mod_model <- regmdc(
        X_training, y_training, s = 2L, method = "tcmars", threshold = threshold, 
        our2_mod_V_best,
        number_of_bins = c(num_bins, NA, NA, NA, NA, NA, NA),
        convex_covariates = c("inc", "age", "e401k"),
        concave_covariates = "e401k",
        variation_constrained_covariates = c("inc", "age"),
        extra_linear_covariates = c("fsize1", "fsize2", "fsize3", "fsize4")
      )
      
      # Compute the fitted values at the test set
      our2_mod_fit_test <- as.vector(predict_regmdc(our2_mod_model, X_test))
      
      # Compute the prediction error with the test set
      our2_mod_pred_error <- mean((y_test - our2_mod_fit_test)**2)
    }, error = function(inner_err) {
      our2_mod_cv_results_sorted <- (
        our2_mod_cv_results[order(our2_mod_cv_results[, 2L], 
                                  decreasing = FALSE), ]
      )
      .GlobalEnv$our2_mod_V_best <- our2_mod_cv_results_sorted[2L, 1L]
      
      our2_mod_model <- regmdc(
        X_training, y_training, s = 2L, method = "tcmars", threshold = threshold, 
        .GlobalEnv$our2_mod_V_best,
        number_of_bins = c(num_bins, NA, NA, NA, NA, NA, NA),
        convex_covariates = c("inc", "age", "e401k"),
        concave_covariates = "e401k",
        variation_constrained_covariates = c("inc", "age"),
        extra_linear_covariates = c("fsize1", "fsize2", "fsize3", "fsize4")
      )
      
      # Compute the fitted values at the test set
      our2_mod_fit_test <- as.vector(predict_regmdc(our2_mod_model, X_test))
      
      # Compute the prediction error with the test set
      .GlobalEnv$our2_mod_pred_error <- mean((y_test - our2_mod_fit_test)**2)
    })
  }, error = function(outer_err) {
    .GlobalEnv$our2_mod_pred_error <- NA
  })
  ######################################################################
  
  c(quadratic = quadratic_pred_error,
    our2_mod = our2_mod_pred_error,
    our2_mod_V_best = our2_mod_V_best)
}

V_best <- estimation_results[, -c(1L:2L), drop = FALSE]
estimation_results <- estimation_results[, c(1L:2L)]
colnames(V_best) <- colnames(estimation_results)[-1L]

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
  V_best = V_best,
  num_rep = num_rep,
  num_folds = num_folds,
  training_ratio = training_ratio,
  num_bins = num_bins,
  threshold = threshold
)

save(results, file = paste0("../../results/401ksubs/401ksubs_mod_rep.Rda"))
gc()
########################################################################