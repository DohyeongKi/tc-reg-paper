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
  # Interaction Model 1 ################################################
  # Build a model
  interaction1_model <- lm(
    nettfa ~ (
      inc + I(inc**2) + age + I(age**2) + inc:age + e401k 
      + fsize1 + fsize2 + fsize3 + fsize4
    ),
    data = k401ksubs_training
  )
  
  # Compute the fitted values at the test set
  interaction1_fit_test <- as.vector(predict(interaction1_model, k401ksubs_test))
  
  # Compute the prediction error with the test set
  interaction1_pred_error <- mean((k401ksubs_test$nettfa - interaction1_fit_test)**2)
  ######################################################################
  
  ######################################################################
  # Interaction Model 2 ################################################
  # Build a model
  interaction2_model <- lm(
    nettfa ~ (
      inc + I(inc**2) + age + I(age**2) + inc:age 
      + inc:I(age**2) + I(inc**2):age + e401k 
      + fsize1 + fsize2 + fsize3 + fsize4
    ),
    data = k401ksubs_training
  )
  
  # Compute the fitted values at the test set
  interaction2_fit_test <- as.vector(predict(interaction2_model, k401ksubs_test))
  
  # Compute the prediction error with the test set
  interaction2_pred_error <- mean((k401ksubs_test$nettfa - interaction2_fit_test)**2)
  ######################################################################
  
  ######################################################################
  # Interaction Model 3 ################################################
  # Build a model
  interaction3_model <- lm(
    nettfa ~ (
      inc + I(inc**2) + age + I(age**2) + inc:age + e401k
      + e401k:age + e401k:I(age**2) + e401k:inc + e401k:I(inc**2)
      + fsize1 + fsize2 + fsize3 + fsize4
    ),
    data = k401ksubs_training
  )
  
  # Compute the fitted values at the test set
  interaction3_fit_test <- as.vector(predict(interaction3_model, k401ksubs_test))
  
  # Compute the prediction error with the test set
  interaction3_pred_error <- mean((k401ksubs_test$nettfa - interaction3_fit_test)**2)
  ######################################################################
  
  ######################################################################
  # Interaction Model 4 ################################################
  # Build a model
  interaction4_model <- lm(
    nettfa ~ (
      inc + I(inc**2) + age + I(age**2) + inc:age 
      + inc:I(age**2) + I(inc**2):age
      + e401k + e401k:age + e401k:I(age**2) + e401k:inc + e401k:I(inc**2)
      + fsize1 + fsize2 + fsize3 + fsize4
    ),
    data = k401ksubs_training
  )
  
  # Compute the fitted values at the test set
  interaction4_fit_test <- as.vector(predict(interaction4_model, k401ksubs_test))
  
  # Compute the prediction error with the test set
  interaction4_pred_error <- mean((k401ksubs_test$nettfa - interaction4_fit_test)**2)
  ######################################################################
  
  ######################################################################
  # Our Model 1 ########################################################
  # Data ###############################################################
  X_training <- k401ksubs_training %>%
    select(inc, age, e401k, fsize1, fsize2, fsize3, fsize4)
  y_training <- k401ksubs_training$nettfa
  X_test <- k401ksubs_test %>%
    select(inc, age, e401k, fsize1, fsize2, fsize3, fsize4)
  y_test <- k401ksubs_test$nettfa
  ######################################################################
  tryCatch({
    # Build a model
    our1_model <- regmdc(
      X_training, y_training, s = 2L, method = "tc", threshold = threshold,
      number_of_bins = c(num_bins, NA, NA, NA, NA, NA, NA),
      is_totally_concave = FALSE,
      extra_linear_covariates = c("e401k", "fsize1", "fsize2", "fsize3", "fsize4")
    )
    
    # Compute the fitted values at the test set
    our1_fit_test <- as.vector(predict_regmdc(our1_model, X_test))
    
    # Compute the prediction error with the test set
    our1_pred_error <- mean((y_test - our1_fit_test)**2)
  }, error = function(err) {
    .GlobalEnv$our1_pred_error <- NA
  })
  ######################################################################
  
  ######################################################################
  # Our Model 2 ########################################################
  tryCatch({
    # Build a model
    our2_model <- regmdc(
      X_training, y_training, s = 2L, method = "tc", threshold = threshold,
      number_of_bins = c(num_bins, NA, NA, NA, NA, NA, NA),
      convex_covariates = c("inc", "age", "e401k"),
      concave_covariates = "e401k", 
      extra_linear_covariates = c("fsize1", "fsize2", "fsize3", "fsize4")
    )
    
    # Compute the fitted values at the test set
    our2_fit_test <- as.vector(predict_regmdc(our2_model, X_test))
    
    # Compute the prediction error with the test set
    our2_pred_error <- mean((y_test - our2_fit_test)**2)
  }, error = function(err) {
    .GlobalEnv$our2_pred_error <- NA
  })
  ######################################################################

  ######################################################################
  # Additive Model #####################################################
  tryCatch({
    # Build a model
    additive_model <- regmdc(
      X_training, y_training, s = 1L, method = "tc", threshold = threshold,
      number_of_bins = c(num_bins, NA, NA, NA, NA, NA, NA),
      is_totally_concave = FALSE,
      extra_linear_covariates = c("e401k", "fsize1", "fsize2", "fsize3", "fsize4")
    )
    
    # Compute the fitted values at the test set
    additive_fit_test <- as.vector(predict_regmdc(additive_model, X_test))
    
    # Compute the prediction error with the test set
    additive_pred_error <- mean((y_test - additive_fit_test)**2)
  }, error = function(err) {
    .GlobalEnv$additive_pred_error <- NA
  })
  ######################################################################
  
  c(quadratic = quadratic_pred_error,
    interaction1 = interaction1_pred_error,
    interaction2 = interaction2_pred_error,
    interaction3 = interaction3_pred_error,
    interaction4 = interaction4_pred_error,
    our1 = our1_pred_error,
    our2 = our2_pred_error,
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
  training_ratio = training_ratio,
  num_bins = num_bins,
  threshold = threshold
)

save(results, file = paste0("../../results/401ksubs/401ksubs_rep_check.Rda"))
gc()
########################################################################