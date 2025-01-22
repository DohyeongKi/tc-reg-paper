rm(list = ls())
random_seed <- 2030L
set.seed(random_seed)

########################################################################
# Source, Library, and Data ############################################
library(tidyverse)
library(regmdc)
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

# Threshold ############################################################
threshold = 1e-4
########################################################################

########################################################################
# Data Split ###########################################################
training_ratio <- .9
training <- sample(nrow(hprice2), size = nrow(hprice2) * training_ratio)

hprice_training <- hprice2[training, ]
hprice_test <- hprice2[-training, ]
########################################################################

########################################################################
# Interaction Model 3 ##################################################
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
########################################################################

########################################################################
# Our Model 2 ##########################################################
# Data #################################################################
X_training <- hprice_training %>%
  select(log_nox, log_dist, crime, rooms, stratio)
y_training <- hprice_training$log_price
X_test <- hprice_test %>%
  select(log_nox, log_dist, crime, rooms, stratio)
y_test <- hprice_test$log_price
########################################################################
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
  .GlobalEnv$our2_model <- NA
  .GlobalEnv$our2_pred_error <- NA
})
########################################################################

########################################################################
# Output Results #######################################################
results <- list(
  data = hprice2,
  training_set = hprice_training,
  test_set = hprice_test,
  interaction3_model = interaction3_model,
  our2_model = our2_model,
  interaction3_pred_error = interaction3_pred_error,
  our2_pred_error = our2_pred_error,
  threshold = threshold,
  random_seed = random_seed
)

save(results, file = paste0("../../results/hprice/hprice_overfitting.Rda"))
gc()
########################################################################