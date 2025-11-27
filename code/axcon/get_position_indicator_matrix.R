#' Construct position indicator matrix
#'
#' Given a design matrix, this function constructs the position indicator 
#' matrix (\eqn{M} as defined in equation (104) of Section 10.2 of the 
#' supplementary material). In addition, the function also returns a list 
#' whose \eqn{k}-th element is the sorted vector of unique entries appearing in 
#' the \eqn{k}-th column of the design matrix (\eqn{\{u^{(k)}_0, \dots, 
#' u^{(k)}_{n_k}\}} in Section 10.2 of the supplementary material). 0 and 1 are 
#' additionally included.
#'
#' @param X_design A numeric design matrix. Each row corresponds to one 
#'   observation.
#' @param rounding_digit An integer specifying the number of decimal places
#'   to retain when rounding the scaled design matrix.
get_position_indicator_matrix <- function(X_design, rounding_digit) {
  d <- ncol(X_design)
  # Compute the sorted unique entries for each column of the design matrix 
  # (0 and 1 are additionally included)
  unique_entries <- lapply((1L:d), function(col){
    column_unique <- unique(c(0.0, X_design[, col], 1.0))
    sort(column_unique)
  })
  
  # Compute the number of columns of the position indicator matrix M
  cum_prod_ncols <- rep(1L, d)
  for (col in (1L:(d - 1L))) {
    cum_prod_ncols[d - col] <- (
      cum_prod_ncols[d + 1L - col] * length(unique_entries[[d + 1L - col]])
    )
  }
  total_ncols <- cum_prod_ncols[1L] * length(unique_entries[[1L]])
  
  # Construct the position indicator matrix M
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
