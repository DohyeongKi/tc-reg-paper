rm(list = ls())
set.seed(2024L)

########################################################################
# Source, Library, and Data ############################################
library(tidyverse)
library(regmdc)
library(caret)  # for cross-validation
library(foreach)  # for parallelization
library(doParallel)  # for parallelization
library(wooldridge)  # the housing price data set
data(hprice2)

hprice2 <- hprice2 %>% 
  mutate(
    log_price = log(price),
    log_nox = log(nox), 
    log_dist = log(dist)
  ) %>% 
  select(log_price, log_nox, log_dist, crime, rooms, stratio)
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
# The Ratio of Training Sets ###########################################
training_ratio <- 0.9
########################################################################

########################################################################
# The Number of Folds ##################################################
num_folds <- 10L
########################################################################

# Threshold ############################################################
threshold = 1e-4
########################################################################

########################################################################
estimation_results <- foreach(rep = 1L:num_rep, .combine = 'rbind') %dopar% {
  ######################################################################
  # Data Split #########################################################
  set.seed(2024L + rep)
  
  training <- sample(nrow(hprice2), size = nrow(hprice2) * training_ratio)
  
  hprice_training <- hprice2[training, ]
  hprice_test <- hprice2[-training, ]
  ######################################################################
  
  ######################################################################
  # Linear Model #######################################################
  # Build a textbook model
  linear_model <- lm(
    log_price ~ log_nox + log_dist + crime + rooms + stratio, 
    data = hprice_training
  )
  
  # Compute the fitted values at the test set
  linear_fit_test <- as.vector(predict(linear_model, hprice_test))
  
  # Compute the prediction error with the test set
  linear_pred_error <- mean((hprice_test$log_price - linear_fit_test)**2)
  ######################################################################
  
  ######################################################################
  # Our Model 2 ########################################################
  # Data ###############################################################
  X_training <- hprice_training %>%
    select(log_nox, log_dist, crime, rooms, stratio)
  y_training <- hprice_training$log_price
  X_test <- hprice_test %>%
    select(log_nox, log_dist, crime, rooms, stratio)
  y_test <- hprice_test$log_price
  ######################################################################
  # Parameter searching
  #our2_mod_V_set <- c(1, 10, 100, 1000, 10000)
  #our2_mod_V_set <- c(1, 5, 10, 50, 100)
  our2_mod_V_set <- c(1, 5, 10, 50)
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
          concave_covariates = "log_nox",
          convex_covariates = c("log_nox", "crime", "rooms"),
          variation_constrained_covariates = c("crime", "rooms"),
          extra_linear_covariates = c("log_dist", "stratio")
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
        concave_covariates = "log_nox",
        convex_covariates = c("log_nox", "crime", "rooms"),
        variation_constrained_covariates = c("crime", "rooms"),
        extra_linear_covariates = c("log_dist", "stratio")
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
        concave_covariates = "log_nox",
        convex_covariates = c("log_nox", "crime", "rooms"),
        variation_constrained_covariates = c("crime", "rooms"),
        extra_linear_covariates = c("log_dist", "stratio")
      )
      
      # Compute the fitted values at the test set
      our2_mod_fit_test <- as.vector(predict_regmdc(our2_mod_model, X_test))
      
      # Compute the prediction error with the test set
      .GlobalEnv$our2_mod_pred_error <- mean((y_test - our2_mod_fit_test)**2)
    })
  }, error = function(err) {
    .GlobalEnv$our2_mod_pred_error <- NA
  })
  ######################################################################
  
  ######################################################################
  # Additive Model #####################################################
  # Parameter searching
  #additive_mod_V_set <- c(1, 10, 100, 1000, 10000)
  #additive_mod_V_set <- c(1, 5, 10, 50, 100)
  additive_mod_V_set <- c(1, 5, 10, 50)
  parameters <- expand.grid(V = additive_mod_V_set) 
  
  # Perform k-fold cross-validation
  k <- num_folds
  folds <- createFolds(y_training, k = k, list = TRUE, returnTrain = FALSE)
  
  additive_mod_cv_results <- matrix(, nrow = length(additive_mod_V_set), ncol = 2L)
  additive_mod_cv_results[, 1L] <- additive_mod_V_set
  
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
          X_cv_training, y_cv_training, s = 1L, method = "tcmars", 
          threshold = threshold, V,
          convex_covariates = c("crime", "rooms"),
          variation_constrained_covariates = c("crime", "rooms"),
          extra_linear_covariates = c("log_nox", "log_dist", "stratio")
        )
        
        # Compute the fitted values at the cv-test data
        our_fit_cv_test <- predict_regmdc(regmdc_model, X_cv_test)
        
        # Compute the prediction error with the cv-test data
        cv_pred_errors[fold_index] <- mean((y_cv_test - our_fit_cv_test)**2)
      }, error = function(err) {
        cv_pred_errors[fold_index] <- NA
      })
    }
    
    additive_mod_cv_results[para_index, 2L] <- mean(cv_pred_errors, na.rm = TRUE)
  }
  
  additive_mod_V_best <- additive_mod_cv_results[which.min(additive_mod_cv_results[, 2L]), 1L]
  
  tryCatch({
    tryCatch({
      # Build a model
      additive_mod_model <- regmdc(
        X_training, y_training, s = 1L, method = "tcmars", threshold = threshold, 
        additive_mod_V_best,
        convex_covariates = c("crime", "rooms"),
        variation_constrained_covariates = c("crime", "rooms"),
        extra_linear_covariates = c("log_nox", "log_dist", "stratio")
      )
      
      # Compute the fitted values at the test set
      additive_mod_fit_test <- as.vector(predict_regmdc(additive_mod_model, X_test))
      
      # Compute the prediction error with the test set
      additive_mod_pred_error <- mean((y_test - additive_mod_fit_test)**2)
    }, error = function(inner_err) {
      additive_mod_cv_results_sorted <- (
        additive_mod_cv_results[order(additive_mod_cv_results[, 2L], 
                                      decreasing = FALSE), ]
      )
      .GlobalEnv$additive_mod_V_best <- additive_mod_cv_results_sorted[2L, 1L]
      
      additive_mod_model <- regmdc(
        X_training, y_training, s = 1L, method = "tcmars", threshold = threshold, 
        .GlobalEnv$additive_mod_V_best,
        convex_covariates = c("crime", "rooms"),
        variation_constrained_covariates = c("crime", "rooms"),
        extra_linear_covariates = c("log_nox", "log_dist", "stratio")
      )
      
      # Compute the fitted values at the test set
      additive_mod_fit_test <- as.vector(predict_regmdc(additive_mod_model, X_test))
      
      # Compute the prediction error with the test set
      .GlobalEnv$additive_mod_pred_error <- mean((y_test - additive_mod_fit_test)**2)
    })
  }, error = function(err) {
    .GlobalEnv$additive_mod_pred_error <- NA
  })
  ######################################################################
  
  c(linear = linear_pred_error,
    our2_mod = our2_mod_pred_error,
    additive_mod = additive_mod_pred_error,
    our2_mod_V_best = our2_mod_V_best,
    additive_mod_V_best = additive_mod_V_best)
}

V_best <- estimation_results[, -c(1L:3L)]
estimation_results <- estimation_results[, c(1L:3L)]
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
  threshold = threshold
)

save(results, file = paste0("../../results/hprice/hprice_mod_rep.Rda"))
gc()
########################################################################

