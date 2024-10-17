#' Solve the Knapsack Problem using Dynamic Programming
#'
#' This function solves the knapsack problem using dynamic programming.
#' It constructs a table to store the maximum value for each weight capacity.
#' The result is obtained by iterating over all items and weights.
#'
#' @param x A data frame with two columns: \code{v} for values and \code{w} for weights.
#'          Each row represents an item with a value and a weight.
#' @param W A positive numeric value representing the maximum weight capacity of the knapsack.
#'
#' @return A list with two elements:
#'   \item{value}{The maximum total value of the knapsack.}
#'   \item{elements}{The indices of the items in \code{x} that make up the optimal solution.}
#'
#' @examples
#' knapsack_objects <- data.frame(
#'   w = sample(1:4000, size = 8, replace = TRUE),
#'   v = runif(n = 8, 0, 10000)
#' )
#' dynamic_knapsack(knapsack_objects, W = 3500)
#'
#' @export
dynamic_knapsack <- function(x, W) {
  # Input validation: Check if x is a data frame with 'v' and 'w' columns
  if (!is.data.frame(x) || !all(c("v", "w") %in% names(x))) {
    stop("Input must be a data frame with 'v' and 'w' columns.")
  }

  # Check if W is a positive number
  if (W <= 0) {
    stop("Knapsack capacity must be greater than 0.")
  }

  # Ensure all values and weights are non-negative
  if (any(x$v < 0) || any(x$w < 0)) {
    stop("Values and weights must be non-negative.")
  }

  n <- nrow(x)  # Get the number of items

  # Create a table to store maximum values for each weight capacity
  dp <- matrix(0, nrow = n + 1, ncol = W + 1)

  # Fill the dp table
  for (i in 1:n) {
    for (w in 0:W) {
      if (x$w[i] <= w) {
        dp[i + 1, w] <- max(dp[i, w], dp[i, w - x$w[i]] + x$v[i])
      } else {
        dp[i + 1, w] <- dp[i, w]
      }
    }
  }

  # The maximum value will be in dp[n + 1, W] instead of dp[n + 1, W + 1]
  best_value <- dp[n + 1, W]

  # Now we need to find which items make up this maximum value
  best_combination <- integer(0)
  w <- W

  for (i in n:1) {
    if (dp[i + 1, w] != dp[i, w]) {  # If the values are different, the item was chosen
      best_combination <- c(best_combination, i)  # Store the item index
      w <- w - x$w[i]  # Reduce the remaining weight of the knapsack
    }
  }

  # Return a list containing the maximum value and the indices of the selected items
  return(list(value = best_value, elements = best_combination))
}

set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )

dynamic_knapsack(x = knapsack_objects[1:500,], W = 2000)

system.time({
  dynamic_knapsack(x = knapsack_objects[1:500,], W = 2000)
})
