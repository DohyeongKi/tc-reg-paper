source('../../axcon/get_position_indicator_matrix.R')
source('../../axcon/get_constraint_matrices.R')
source('../../axcon/solve_optimization_problem.R')

#library(here)
#top_level_dir = here()
#axcon_dir = file.path(top_level_dir, "axcon")
#source(file.path(axcon_dir, "get_position_indicator_matrix.R"))
#source(file.path(axcon_dir, "get_constraint_matrices.R"))
#source(file.path(axcon_dir, "solve_optimization_problem.R"))

axcon <- function(X_design, y,
                  is_concave = TRUE, is_scaled = FALSE,
                  rounding_digit = 4L,
                  concave_covariates = NULL, convex_covariates = NULL, 
                  solver = NULL) {
  # Error handling =============================================================
  if (length(dim(X_design)) != 2L) {
    stop('`X_design` must be a matrix or a data frame.')
  }
  if (anyNA(X_design)) {
    stop('`X_design` must not include NA or NaN.')
  }
  if (!all(apply(X_design, MARGIN = c(1L, 2L), FUN = is.numeric))) {
    stop('`X_design` must be numeric.')
  }

  if (anyNA(y)) {
    stop('`y` must not include NA or NaN.')
  }
  if (!is.numeric(y)) {
    stop('`y` must be numeric.')
  }
  if (length(y) != nrow(X_design)) {
    stop('`length(y)` must be equal to `nrow(X_design)`.')
  }

  if (!is.logical(is_concave)) {
    stop('`is_concave` must be TRUE or FALSE.')
  }

  if (!is.logical(is_scaled)) {
    stop('`is_scaled` must be TRUE or FALSE.')
  }
  if (is_scaled) {
    if (min(X_design) < 0 || max(X_design) > 1) {
      stop('If `is_scaled` is TRUE, all the entries of `X_design` must be between 0 and 1.')
    }
  }

  if (!is.integer(rounding_digit) || length(rounding_digit) != 1L) {
    stop('`rounding_digit` must be an integer.')
  }
  if (rounding_digit < 1L) {
    stop('`rounding_digit` must be at least 1.')
  }

  # If `concave_covariates` is a string vector of column names, covert it into
  # an integer vector of column indices
  if (!is.null(concave_covariates)) {
    concave_covariates <- unique(concave_covariates)
    if (is.numeric(concave_covariates)) {
      if (!is.integer(concave_covariates)) {
        stop('`concave_covariates` must be an integer vector.')
      }

      concave_covariates <- sort(concave_covariates)
      if ((concave_covariates[1] <= 0)
          | (concave_covariates[length(concave_covariates)] > ncol(X_design))) {
        stop('Each integer in `concave_covariates` must be at least 1 and at most `ncol(X_design)`.')
      }
    } else {
      concave_covariates <- sapply(concave_covariates, function(col_name) {
        col_name_index <- which(colnames(X_design) == col_name)
        if (length(col_name_index) == 0) {
          stop(paste0('`X_design` does not have a column with the name "', col_name, '".'))
        } else {
          col_name_index
        }
      })
    }
  }

  # If `convex_covariates` is a string vector of column names, covert it into an
  # integer vector of column indices
  if (!is.null(convex_covariates)) {
    convex_covariates <- unique(convex_covariates)
    if (is.numeric(convex_covariates)) {
      if (!is.integer(convex_covariates)) {
        stop('`convex_covariates` must be an integer vector.')
      }

      convex_covariates <- sort(convex_covariates)
      if ((convex_covariates[1] <= 0)
          | (convex_covariates[length(convex_covariates)] > ncol(X_design))) {
        stop('Each integer in `convex_covariates` must be at least 1 and at most `ncol(X_design)`.')
      }
    } else {
      convex_covariates <- sapply(convex_covariates, function(col_name) {
        col_name_index <- which(colnames(X_design) == col_name)
        if (length(col_name_index) == 0) {
          stop(paste0('`X_design` does not have a column with the name "', col_name, '".'))
        } else {
          col_name_index
        }
      })
    }
  }

  if (is.null(concave_covariates) && is.null(convex_covariates)) {
    if (is_concave) {
      concave_covariates <- (1L:ncol(X_design))
      convex_covariates <- NULL
    } else {
      concave_covariates <- NULL
      convex_covariates <- (1L:ncol(X_design))
    }
  } else if (is.null(convex_covariates)) {
    convex_covariates <- (1L:ncol(X_design))[-concave_covariates]
    if (length(convex_covariates) == 0) {
      convex_covariates <- NULL
    }
  } else if (is.null(concave_covariates)){
    concave_covariates <- (1L:ncol(X_design))[-convex_covariates]
    if (length(concave_covariates) == 0) {
      concave_covariates <- NULL
    }
  } else {
    if (length(unique(c(concave_covariates, convex_covariates))) < ncol(X_design)) {
      stop('If neither `concave_covariates` nor `convex_covariates` is NULL, all covariates must be included in at least one of them.')
    }
  }

  # ============================================================================
  if (is_scaled) {
    max_vals <- rep(1.0, ncol(X_design))
    min_vals <- rep(0.0, ncol(X_design))

    X_design = round(X_design, digits = rounding_digit)
  } else {
    max_vals <- apply(X_design, MARGIN = 2L, max)
    min_vals <- apply(X_design, MARGIN = 2L, min)

    for (col in (1L:ncol(X_design))) {
      if (max_vals[col] == min_vals[col]) {
        stop(paste0('All the values of "', colnames(X_design)[col], '" are the same. Please remove that variable.'))
      } else {
        X_design[, col] <- round(
          ((X_design[, col] - min_vals[col]) / (max_vals[col] - min_vals[col])),
          digits = rounding_digit
        )
      }
    }
  }

  indicator_matrix_and_unique_entries <- get_position_indicator_matrix(
    X_design, rounding_digit
  )
  position_indicator_matrix <- indicator_matrix_and_unique_entries$indicator_matrix
  unique_entries <- indicator_matrix_and_unique_entries$unique_entries

  constraint_matrices <- get_constraint_matrices(unique_entries)

  fitted_values_lattice <- solve_optimization_problem(
    y, position_indicator_matrix, constraint_matrices,
    concave_covariates, convex_covariates, solver
  )
  fitted_values_design <- position_indicator_matrix %*% fitted_values_lattice

  axcon_model <- list(
    X_design = X_design,
    y = y,
    is_concave = is_concave,
    is_scaled = is_scaled,
    rounding_digit = rounding_digit,
    concave_covariates = concave_covariates,
    convex_covariates = convex_covariates,
    solver = solver,
    max_vals = max_vals,
    min_vals = min_vals,
    unique_entries = unique_entries,
    fitted_values_lattice = fitted_values_lattice,
    fitted_values_design = fitted_values_design
  )
}
