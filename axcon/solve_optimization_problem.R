library(CVXR)

solve_optimization_problem <- function(y, position_indicator_matrix,
                                       constraint_matrices,
                                       concave_covariates, convex_covariates,
                                       solver) {
  if (!is.null(solver)) {
    if (!(solver %in% CVXR::installed_solvers())) {
      stop('The given solver is not supported.')
    }
    
    if (solver == "MOSEK") {
      if (!require(Rmosek)) {
        stop('"MOSEK" is not installed.')
      }
    }
  }
  
  if (length(y) != nrow(position_indicator_matrix)) {
    stop('`nrow(y)` is not equal to `nrow(position_indicator_matrix)`.')
  }

  d <- length(constraint_matrices)

  theta <- Variable(ncol(position_indicator_matrix))
  objective <- Minimize(sum((y - position_indicator_matrix %*% theta)^2))

  constraints <- lapply(1L:d, function(col) {
    if (col %in% concave_covariates) {
      if (col %in% convex_covariates) {
        (constraint_matrices[[col]] %*% theta == 0)
      } else {
        (constraint_matrices[[col]] %*% theta <= 0)
      }
    } else {
      (constraint_matrices[[col]] %*% theta >= 0)
    }
  })
  problem <- Problem(objective, constraints = constraints)
  if (is.null(solver)) {
    solution <- solve(problem)
  } else {
    solution <- solve(problem, solver = solver)
  }

  if (solution$status != "optimal") {
    stop('`solution$status` is not "optimal".')
  }

  solution$getValue(theta)
}
