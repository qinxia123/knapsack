#' Solve the Knapsack Problem using Greedy Heuristic
#'
#' This function solves the knapsack problem using a greedy approach.
#' It selects items based on their value-to-weight ratio until the capacity is full.
#'
#' @param x A data frame with two columns: \code{v} for values and \code{w} for weights.
#'          Each row represents an item with a value and a weight.
#' @param W A positive numeric value representing the maximum weight capacity of the knapsack.
#'
#' @return A list with two elements:
#'   \item{value}{The total value of the items selected for the knapsack.}
#'   \item{elements}{The indices of the items in \code{x} that have been selected as numeric values.}
#'
#' @examples
#' set.seed(42)
#' knapsack_objects <- data.frame(
#'   w = sample(1:4000, size = 1200, replace = TRUE),
#'   v = runif(1200, 0, 10000)
#' )
#' greedy_knapsack(x = knapsack_objects, W = 2000)
#'
#' @export
greedy_knapsack <- function(x, W) {
  # Input validation
  if (!is.data.frame(x) || !all(c("v", "w") %in% names(x))) {
    stop("Input must be a data frame with columns 'v' and 'w'.")
  }

  # Check if knapsack capacity is positive
  if (W <= 0) {
    stop("Knapsack capacity must be greater than 0.")
  }

  # Check if values and weights are non-negative
  if (any(x$v < 0) || any(x$w < 0) || any(is.na(x$w)) || any(is.na(x$v))) {
    stop("Values and weights must not contain NA and must be non-negative.")
  }

  # Calculate value-to-weight ratio
  x$unit_value <- x$v / x$w

  # Sort items by value-to-weight ratio in descending order
  x <- x[order(-x$unit_value), ]

  total_value <- 0  # Initialize total value
  total_weight <- 0  # Initialize total weight
  selected_items <- integer(0)  # Initialize selected items index

  # Select items
  for (i in 1:nrow(x)) {
    if (total_weight + x$w[i] <= W) {  # If adding the current item doesn't exceed capacity
      total_weight <- total_weight + x$w[i]  # Update total weight
      total_value <- total_value + x$v[i]  # Update total value
      selected_items <- c(selected_items, as.numeric(rownames(x)[i]))  # Record the selected item index
    } else {
      break  # If adding the current item exceeds capacity, stop selection
    }
  }

  return(list(value = round(total_value), elements = selected_items))  # Return total value and selected item indices
}

set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )

greedy_knapsack(x = knapsack_objects[1:2000,], W = 2000)

system.time({
  greedy_knapsack(x = knapsack_objects[1:2000,], W = 2000)
})

