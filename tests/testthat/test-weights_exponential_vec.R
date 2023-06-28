
library(checkmate)

expect_weights_exponential_matrix <- function(weights, alphas, n) {
  expect_matrix(
    x = weights, ncols = n, nrows = length(alphas), any.missing = FALSE
  )
  expect_numeric(x = as.numeric(weights), lower = 0, upper = 1)
  
  weights_expected <- t(vapply(
    X = alphas,
    FUN = function(alpha) {
      weights_exponential(
        alpha = alpha,
        n = n
      )
    },
    FUN.VALUE = numeric(length = n),
    USE.NAMES = FALSE
  ))
  
  if (n == 1) {
    weights_expected <- matrix(
      weights_expected,
      ncol = 1
    )
  }
  
  expect_equal(weights, weights_expected)
}

alphas <- c(seq(0, 1, by = 0.1), runif(2), 0, runif(5), 1, runif(10))

test_that("returns the same weights as `weights_exponential()`", {
  n <- 50
  
  weights <- weights_exponential_vec(alphas = alphas, n = n)
  expect_weights_exponential_matrix(weights = weights, alphas = alphas, n = n)
})

test_that("returns the same weights as `weights_exponential()` for large `n`", {
  n <- 10000
  
  weights <- weights_exponential_vec(alphas = alphas, n = n)
  expect_weights_exponential_matrix(weights = weights, alphas = alphas, n = n)
})

test_that("returns the same weights as `weights_exponential()` for small `n`", {
  n <- 2
  weights <- weights_exponential_vec(alphas = alphas, n = n)
  expect_weights_exponential_matrix(weights = weights, alphas = alphas, n = n)
  
  n <- 3
  weights <- weights_exponential_vec(alphas = alphas, n = n)
  expect_weights_exponential_matrix(weights = weights, alphas = alphas, n = n)
  
  n <- 7
  weights <- weights_exponential_vec(alphas = alphas, n = n)
  expect_weights_exponential_matrix(weights = weights, alphas = alphas, n = n)
  
  n <- 12
  weights <- weights_exponential_vec(alphas = alphas, n = n)
  expect_weights_exponential_matrix(weights = weights, alphas = alphas, n = n)
})

test_that("returns the same weights as `weights_exponential()` for `n=1`", {
  n <- 1
  weights <- weights_exponential_vec(alphas = alphas, n = n)
  
  expect_weights_exponential_matrix(weights = weights, alphas = alphas, n = n)
})
