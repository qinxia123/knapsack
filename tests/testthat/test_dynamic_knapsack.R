suppressWarnings(RNGversion(min(as.character(getRversion()),"3.5.3")))
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <- data.frame(
  w = sample(1:4000, size = n, replace = TRUE),
  v = runif(n = n, 0, 10000)
)

test_that("Correct object is returned", {
  expect_silent(dk <- dynamic_knapsack(x = knapsack_objects[1:8,], W = 3500))
  expect_named(dk, c("value", "elements"))
})

test_that("functions rejects erroneous input.", {
  expect_error(dynamic_knapsack("hej", 3500))
  expect_error(dynamic_knapsack(x = knapsack_objects[1:8,], W = -3500))
})

test_that("Function returns correct results.", {
  dk <- dynamic_knapsack(x = knapsack_objects[1:8,], W = 3500)
  expect_equal(round(dk$value), 16770)
  expect_true(all(round(dk$elements) %in% c(5, 8)))

  dk <- dynamic_knapsack(x = knapsack_objects[1:12,], W = 3500)
  expect_equal(round(dk$value), 16770)
  expect_true(all(round(dk$elements) %in% c(5, 8)))

  dk <- dynamic_knapsack(x = knapsack_objects[1:8,], W = 2000)
  expect_equal(round(dk$value), 15428)
  expect_true(all(round(dk$elements) %in% c(3, 8)))

  dk <- dynamic_knapsack(x = knapsack_objects[1:12,], W = 2000)
  expect_equal(round(dk$value), 15428)
  expect_true(all(round(dk$elements) %in% c(3, 8)))

  st <- system.time(dk <- dynamic_knapsack(x = knapsack_objects[1:16,], W = 2000))
  expect_true(as.numeric(st)[2] <= 1)  # The time complexity of dynamic programming should be O(nW)
})

# This is the newly added code to test larger datasets
test_that("Larger dataset results are correct.", {
  dk <- dynamic_knapsack(x = knapsack_objects[1:800,], W = 3500)
  expect_equal(round(dk$value), 192647, tolerance = 0.05)

  dk <- dynamic_knapsack(x = knapsack_objects[1:1200,], W = 3500)
  expect_equal(round(dk$value), 270290, tolerance = 0.05)
})
