context("dynamic_knapsack")

set.seed(42)
n <- 2000
knapsack_objects <- data.frame(
  w=sample(1:4000, size = n, replace = TRUE),
  v=runif(n = n, 0, 10000)
)

test_that("Correct object is returned", {
  expect_silent(dk <- knapack_dynamic(x = knapsack_objects[1:8,], W = 3500))
  expect_named(dk, c("value", "elements"))
})

test_that("functions rejects errounous input.", {
  expect_error(knapack_dynamic("hej", 3500))
  expect_error(greedy_knapsknapack_dynamicack(x = knapsack_objects[1:8,], W = -3500))
})

test_that("Function return correct results.", {
  dk <- greedy_knapsack(x = knapsack_objects[1:8,], W = 3500)
  expect_equal(round(dk$value), 15428)
  expect_true(all(round(dk$elements) %in% c(3, 8)))
  
})