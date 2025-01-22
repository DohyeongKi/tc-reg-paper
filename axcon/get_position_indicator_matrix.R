get_position_indicator_matrix <- function(X_design, rounding_digit) {
  d <- ncol(X_design)
  unique_entries <- lapply((1L:d), function(col){
    column_unique <- unique(c(0.0, X_design[, col], 1.0))
    sort(column_unique)
  })

  cum_prod_ncols <- rep(1L, d)
  for (col in (1L:(d - 1L))) {
    cum_prod_ncols[d - col] <- (
      cum_prod_ncols[d + 1L - col] * length(unique_entries[[d + 1L - col]])
    )
  }
  total_ncols <- cum_prod_ncols[1L] * length(unique_entries[[1L]])

  indicator_matrix <- do.call(rbind, lapply((1L:nrow(X_design)), function(row) {
    X_design_row <- X_design[row, ]

    entry_orders <- sapply((1L:d), function(col) {
      X_design_row_col <- as.numeric(X_design_row[col])
      which(
        (unique_entries[[col]] < X_design_row_col + 0.5 * (0.1**rounding_digit))
        & (unique_entries[[col]] > X_design_row_col - 0.5 * (0.1**rounding_digit))
      )[1L]  # it is actually unique, though
    })

    indicator_vector <- numeric(total_ncols)
    nonzero_index <- sum(cum_prod_ncols * (entry_orders - 1L)) + 1L
    indicator_vector[nonzero_index] <- 1.0

    indicator_vector
  }))

  list(indicator_matrix = indicator_matrix, unique_entries = unique_entries)
}
