get_constraint_matrices <- function(unique_entries) {
  d <- length(unique_entries)

  constraint_matrices <- lapply((1L:d), function(col) {
    unique_entries_col <- unique_entries[[col]]
    D <- do.call(rbind, lapply(1L:(length(unique_entries_col) - 2L), function(row) {
      D_row <- numeric(length(unique_entries_col))
      # D_row[row] <- 1.0 / (unique_entries_col[row + 1L] - unique_entries_col[row])
      # D_row[row + 2L] <- 1.0 / (unique_entries_col[row + 2L] - unique_entries_col[row + 1L])
      # D_row[row + 1L] <- -(D_row[row] + D_row[row + 2L])

      D_row[row] <- (
        (unique_entries_col[row + 2L] - unique_entries_col[row]) 
        / (unique_entries_col[row + 1L] - unique_entries_col[row])
      )
      D_row[row + 2L] <- (
        (unique_entries_col[row + 2L] - unique_entries_col[row])
        / (unique_entries_col[row + 2L] - unique_entries_col[row + 1L])
      )
      D_row[row + 1L] <- -(D_row[row] + D_row[row + 2L])
      D_row
    }))

    constraint_matrix <- matrix(1.0, nrow = 1L, ncol = 1L)

    for (j in (1L:d)) {
      if (j == col) {
        constraint_matrix <- kronecker(constraint_matrix, D)
      } else {
        constraint_matrix <- kronecker(
          constraint_matrix, diag(nrow = length(unique_entries[[j]]))
        )
      }
    }

    constraint_matrix
  })

  constraint_matrices
}

