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
  # Build a model
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
  # Quadratic Model ####################################################
  # Build a model
  quadratic_model <- lm(
    log_price ~ (
      log_nox + log_dist + crime + I(crime^2) + rooms + I(rooms^2) + stratio
    ), 
    data = hprice_training
  )
  
  # Compute the fitted values at the test set
  quadratic_fit_test <- as.vector(predict(quadratic_model, hprice_test))
  
  # Compute the prediction error with the test set
  quadratic_pred_error <- mean((hprice_test$log_price - quadratic_fit_test)**2)
  ######################################################################
  
  ######################################################################
  # Interaction Model 1 ################################################
  # Build a model
  interaction1_model <- lm(
    log_price ~ (
      log_nox + log_dist + crime + I(crime^2) + rooms + I(rooms^2) + stratio 
      + crime:rooms
    ), 
    data = hprice_training
  )
  
  # Compute the fitted values at the test set
  interaction1_fit_test <- as.vector(predict(interaction1_model, hprice_test))
  
  # Compute the prediction error with the test set
  interaction1_pred_error <- mean((hprice_test$log_price - interaction1_fit_test)**2)
  ######################################################################
  
  ######################################################################
  # Interaction Model 2 ################################################
  # Build a model
  interaction2_model <- lm(
    log_price ~ (
      log_nox + log_dist + crime + I(crime^2) + rooms + I(rooms^2) + stratio 
      + crime:rooms + crime:I(rooms^2) + I(crime^2):rooms
    ), 
    data = hprice_training
  )
  
  # Compute the fitted values at the test set
  interaction2_fit_test <- as.vector(predict(interaction2_model, hprice_test))
  
  # Compute the prediction error with the test set
  interaction2_pred_error <- mean((hprice_test$log_price - interaction2_fit_test)**2)
  ######################################################################
  
  ######################################################################
  # Interaction Model 3 ################################################
  # Build a model
  interaction3_model <- lm(
    log_price ~ (
      log_nox + log_dist + crime + I(crime^2) + rooms + I(rooms^2) + stratio 
      + log_nox:crime + log_nox:I(crime^2) + log_nox:rooms + log_nox:I(rooms^2)
      + crime:rooms
    ), 
    data = hprice_training
  )
  
  # Compute the fitted values at the test set
  interaction3_fit_test <- as.vector(predict(interaction3_model, hprice_test))
  
  # Compute the prediction error with the test set
  interaction3_pred_error <- mean((hprice_test$log_price - interaction3_fit_test)**2)
  ######################################################################
  
  ######################################################################
  # Interaction Model 4 ################################################
  # Build a model
  interaction4_model <- lm(
    log_price ~ (
      log_nox + log_dist + crime + I(crime^2) + rooms + I(rooms^2) + stratio 
      + log_nox:crime + log_nox:I(crime^2) + log_nox:rooms + log_nox:I(rooms^2)
      + crime:rooms + crime:I(rooms^2) + I(crime^2):rooms
    ), 
    data = hprice_training
  )
  
  # Compute the fitted values at the test set
  interaction4_fit_test <- as.vector(predict(interaction4_model, hprice_test))
  
  # Compute the prediction error with the test set
  interaction4_pred_error <- mean((hprice_test$log_price - interaction4_fit_test)**2)
  ######################################################################
  
  ######################################################################
  # Our Model 1 ########################################################
  # Data ###############################################################
  X_training <- hprice_training %>%
    select(log_nox, log_dist, crime, rooms, stratio)
  y_training <- hprice_training$log_price
  X_test <- hprice_test %>%
    select(log_nox, log_dist, crime, rooms, stratio)
  y_test <- hprice_test$log_price
  ######################################################################
  tryCatch({
    # Build a model
    our1_model <- regmdc(
      X_training, y_training, s = 2L, method = "tc", threshold = threshold,
      convex_covariates = c("crime", "rooms"),
      extra_linear_covariates = c("log_nox", "log_dist", "stratio")
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
      concave_covariates = c("log_nox"),
      convex_covariates = c("log_nox", "crime", "rooms"),
      extra_linear_covariates = c("log_dist", "stratio")
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
      convex_covariates = c("crime", "rooms"),
      extra_linear_covariates = c("log_nox", "log_dist", "stratio")
    )
    
    # Compute the fitted values at the test set
    additive_fit_test <- as.vector(predict_regmdc(additive_model, X_test))
    
    # Compute the prediction error with the test set
    additive_pred_error <- mean((y_test - additive_fit_test)**2)
  }, error = function(err) {
    .GlobalEnv$additive_pred_error <- NA
  })
  ######################################################################

  c(linear = linear_pred_error,
    quadratic = quadratic_pred_error,
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
  threshold = threshold
)

save(results, file = paste0("../../results/hprice/hprice_rep.Rda"))
gc()
########################################################################

