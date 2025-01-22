predict_axcon <- function(axcon_model, X_pred) {
  X_design <- axcon_model$X_design
  is_scaled <- axcon_model$is_scaled
  rounding_digit <- axcon_model$rounding_digit
  max_vals <- axcon_model$max_vals
  min_vals <- axcon_model$min_vals
  unique_entries <- axcon_model$unique_entries
  fitted_values_lattice <- axcon_model$fitted_values_lattice

  # Error handling =============================================================
  if (is.vector(X_pred)) {
    if (length(X_pred) != ncol(X_design)) {
      stop('`length(X_pred)` must be equal to `ncol(X_design)`.')
    }

    X_pred <- matrix(X_pred, nrow = 1)
  }
  if (anyNA(X_pred)) {
    stop('`X_pred` must not include NA or NaN.')
  }
  if (!all(apply(X_pred, MARGIN = c(1L, 2L), FUN = is.numeric))) {
    stop('`X_pred` must be numeric.')
  }
  if (ncol(X_pred) != ncol(X_design)) {
    stop('`ncol(X_pred)` must be equal to `ncol(X_design)`.')
  }

  lapply((1L:nrow(X_pred)), function(row) {
    lapply((1L:ncol(X_pred)), function(col) {
      if (X_pred[row, col] < min_vals[col] ||
          X_pred[row, col] > max_vals[col]) {
        stop(paste0('The ', row, 'th data is out of the range.'))
      }
    })
  })

  # ============================================================================
  if (is_scaled) {
    X_pred = round(X_pred, digis = rounding_digit)
  } else {
    for (col in (1L:ncol(X_pred))) {
      X_pred[, col] <- round(
        ((X_pred[, col] - min_vals[col]) / (max_vals[col] - min_vals[col])),
        digits = rounding_digit
      )
    }
  }

  d <- ncol(X_design)
  cum_prod_ncols <- rep(1L, d)
  for (col in (1L:(d - 1L))) {
    cum_prod_ncols[d - col] <- (
      cum_prod_ncols[d + 1L - col] * length(unique_entries[[d + 1L - col]])
    )
  }
  total_ncols <- cum_prod_ncols[1L] * length(unique_entries[[1L]])

  predictions <- sapply((1L:nrow(X_pred)), function(row) {
    bin_index_and_relative_location <- lapply((1L:d), function(col) {
      X_pred_row_col <- X_pred[row, col]
      unique_entries_col <- unique_entries[[col]]

      # Conduct binary search to find the bin that contains the "row"th data
      low <- 1L
      high <- length(unique_entries_col)

      while (low + 1L < high) {
        mid <- as.integer((low + high) / 2L)
        if (X_pred_row_col >= unique_entries_col[mid]) {
          low <- mid
        } else {
          high <- mid
        }
      }

      relative_location <- (
        (X_pred_row_col - unique_entries_col[low])
        / (unique_entries_col[low + 1L] - unique_entries_col[low])
      )

      list(bin_index = low, relative_location = relative_location)
    })

    bin_index <- sapply((1L:d), function(row) {
      bin_index_and_relative_location[[row]]$bin_index
    })
    relative_location <- sapply((1L:d), function(row) {
      bin_index_and_relative_location[[row]]$relative_location
    })

    # Conduct multivariate interpolation
    delta <- expand.grid(rep(list(c(0L, 1L)), d)) # d-dim binary vectors
    prediction <- sum(sapply((1L:nrow(delta)), function(row_delta) {
      theta_index <- (
        sum(cum_prod_ncols * ((bin_index + delta[row_delta, ]) - 1L)) + 1L
      )
      theta_coeff <- (
        prod(relative_location * delta[row_delta, ]
             + (1.0 - relative_location) * (1.0 - delta[row_delta, ]))
      )

      theta_coeff * fitted_values_lattice[theta_index]
    }))
  })

  predictions
}
