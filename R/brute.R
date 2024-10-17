library(parallel)
#' Solve the Knapsack Problem using Brute Force (with optional parallelization)
#'
#' This function solves the knapsack problem using brute-force search.
#' It optionally parallelizes the brute-force search if the argument \code{parallel} is set to TRUE.
#'
#' @param x A data frame with two columns: \code{v} for values and \code{w} for weights.
#' @param W A positive numeric value representing the maximum weight capacity of the knapsack.
#' @param parallel A logical value indicating whether to parallelize the search. Defaults to FALSE.
#'
#' @return A list with two elements:
#'   \item{value}{The maximum total value of the knapsack.}
#'   \item{elements}{The indices of the items in \code{x} that make up the optimal solution.}
#'
#' @export
brute_force_knapsack <- function(x, W, parallel = FALSE) {
  if (!is.data.frame(x) || !all(c("v", "w") %in% names(x))) {
    stop("Input must be a data frame with columns 'v' and 'w'.")
  }

  if (W <= 0) {
    stop("Knapsack capacity must be greater than 0.")
  }

  if (any(x$v < 0) || any(x$w < 0)) {
    stop("Values and weights must be non-negative.")
  }

  n <- nrow(x)
  best_value <- 0
  best_combination <- NULL
  num_combinations <- 2^n

  # Helper function to evaluate one combination
  evaluate_combination <- function(i) {
    selected <- as.logical(intToBits(i)[1:n])
    total_weight <- sum(x$w[selected])
    total_value <- sum(x$v[selected])

    if (total_weight <= W) {
      return(list(value = total_value, combination = which(selected)))
    } else {
      return(list(value = 0, combination = NULL))
    }
  }

  if (parallel) {
    # Detect the number of available cores
    num_cores <- detectCores()

    # Divide the workload into multiple cores using mclapply (for non-Windows)
    results <- mclapply(0:(num_combinations - 1), evaluate_combination, mc.cores = num_cores)
  } else {
    # Sequential brute-force search
    results <- lapply(0:(num_combinations - 1), evaluate_combination)
  }

  # Find the best solution
  for (result in results) {
    if (result$value > best_value) {
      best_value <- result$value
      best_combination <- result$combination
    }
  }

  return(list(value = best_value, elements = best_combination))
}

# Example usage with both parallel and sequential mode

#set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
#n <- 2000
#knapsack_objects <- data.frame(
  #w = sample(1:4000, size = n, replace = TRUE),
  #v = runif(n = n, 0, 10000)
#)

# Run in parallel mode
#cat("Running in parallel mode:\n")
#system.time({
  #result_parallel <- brute_force_knapsack(x = knapsack_objects[1:16, ], W = 2000, parallel = TRUE)
  #print(result_parallel)
#})

# Run in sequential mode
#cat("\nRunning in sequential mode:\n")
#system.time({
#result_sequential <- brute_force_knapsack(x = knapsack_objects[1:16, ], W = 2000, parallel = FALSE)
#print(result_sequential)
#})
