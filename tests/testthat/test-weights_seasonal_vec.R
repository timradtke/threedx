
library(checkmate)

expect_weights_seasonal_matrix <- function(weights, alphas, n, period_length) {
  expect_matrix(
    x = weights, ncols = n, nrows = length(alphas), any.missing = FALSE
  )
  expect_numeric(x = as.numeric(weights), lower = 0, upper = 1)
  
  weights_expected <- t(vapply(
    X = alphas,
    FUN = function(alpha) {
      weights_seasonal(
        alpha_seasonal = alpha, n = n, period_length = period_length
      )
    },
    FUN.VALUE = numeric(length = n),
    USE.NAMES = FALSE
  ))
  
  if (n == 1) {
    weights_expected <- matrix(weights_expected, ncol = 1)
  }
  
  expect_equal(weights, weights_expected)
}

alphas <- c(seq(0, 1, by = 0.1), runif(2), 0, runif(5), 1, runif(10))

test_that("returns the same weights as `weights_seasonal()`", {
  n <- 50
  
  period_length <- 3
  weights <- weights_seasonal_vec(
    alphas_seasonal = alphas, n = n, period_length = period_length
  )
  expect_weights_seasonal_matrix(
    weights = weights, alphas = alphas, n = n, period_length = period_length
  )
  
  period_length <- 5
  weights <- weights_seasonal_vec(
    alphas_seasonal = alphas, n = n, period_length = period_length
  )
  expect_weights_seasonal_matrix(
    weights = weights, alphas = alphas, n = n, period_length = period_length
  )
  
  period_length <- 7
  weights <- weights_seasonal_vec(
    alphas_seasonal = alphas, n = n, period_length = period_length
  )
  expect_weights_seasonal_matrix(
    weights = weights, alphas = alphas, n = n, period_length = period_length
  )
  
  period_length <- 12
  weights <- weights_seasonal_vec(
    alphas_seasonal = alphas, n = n, period_length = period_length
  )
  expect_weights_seasonal_matrix(
    weights = weights, alphas = alphas, n = n, period_length = period_length
  )
})

test_that("returns the same weights as `weights_seasonal()` for large `n`", {
  n <- 10000
  
  period_length <- 3
  weights <- weights_seasonal_vec(
    alphas_seasonal = alphas, n = n, period_length = period_length
  )
  expect_weights_seasonal_matrix(
    weights = weights, alphas = alphas, n = n, period_length = period_length
  )
  
  period_length <- 5
  weights <- weights_seasonal_vec(
    alphas_seasonal = alphas, n = n, period_length = period_length
  )
  expect_weights_seasonal_matrix(
    weights = weights, alphas = alphas, n = n, period_length = period_length
  )
  
  period_length <- 7
  weights <- weights_seasonal_vec(
    alphas_seasonal = alphas, n = n, period_length = period_length
  )
  expect_weights_seasonal_matrix(
    weights = weights, alphas = alphas, n = n, period_length = period_length
  )
  
  period_length <- 12
  weights <- weights_seasonal_vec(
    alphas_seasonal = alphas, n = n, period_length = period_length
  )
  expect_weights_seasonal_matrix(
    weights = weights, alphas = alphas, n = n, period_length = period_length
  )
})

test_that("returns the same weights as `weights_seasonal()` for `n=2`", {
  n <- 2
  
  period_length <- 3
  weights <- weights_seasonal_vec(
    alphas_seasonal = alphas, n = n, period_length = period_length
  )
  expect_weights_seasonal_matrix(
    weights = weights, alphas = alphas, n = n, period_length = period_length
  )
  
  period_length <- 5
  weights <- weights_seasonal_vec(
    alphas_seasonal = alphas, n = n, period_length = period_length
  )
  expect_weights_seasonal_matrix(
    weights = weights, alphas = alphas, n = n, period_length = period_length
  )
  
  period_length <- 7
  weights <- weights_seasonal_vec(
    alphas_seasonal = alphas, n = n, period_length = period_length
  )
  expect_weights_seasonal_matrix(
    weights = weights, alphas = alphas, n = n, period_length = period_length
  )
  
  period_length <- 12
  weights <- weights_seasonal_vec(
    alphas_seasonal = alphas, n = n, period_length = period_length
  )
  expect_weights_seasonal_matrix(
    weights = weights, alphas = alphas, n = n, period_length = period_length
  )
})

test_that("returns the same weights as `weights_seasonal()` for `n=3`", {
  n <- 3
  
  period_length <- 3
  weights <- weights_seasonal_vec(
    alphas_seasonal = alphas, n = n, period_length = period_length
  )
  expect_weights_seasonal_matrix(
    weights = weights, alphas = alphas, n = n, period_length = period_length
  )
  
  period_length <- 5
  weights <- weights_seasonal_vec(
    alphas_seasonal = alphas, n = n, period_length = period_length
  )
  expect_weights_seasonal_matrix(
    weights = weights, alphas = alphas, n = n, period_length = period_length
  )
  
  period_length <- 7
  weights <- weights_seasonal_vec(
    alphas_seasonal = alphas, n = n, period_length = period_length
  )
  expect_weights_seasonal_matrix(
    weights = weights, alphas = alphas, n = n, period_length = period_length
  )
  
  period_length <- 12
  weights <- weights_seasonal_vec(
    alphas_seasonal = alphas, n = n, period_length = period_length
  )
  expect_weights_seasonal_matrix(
    weights = weights, alphas = alphas, n = n, period_length = period_length
  )
})

test_that("returns the same weights as `weights_seasonal()` for `n=4`", {
  n <- 4
  
  period_length <- 3
  weights <- weights_seasonal_vec(
    alphas_seasonal = alphas, n = n, period_length = period_length
  )
  expect_weights_seasonal_matrix(
    weights = weights, alphas = alphas, n = n, period_length = period_length
  )
  
  period_length <- 5
  weights <- weights_seasonal_vec(
    alphas_seasonal = alphas, n = n, period_length = period_length
  )
  expect_weights_seasonal_matrix(
    weights = weights, alphas = alphas, n = n, period_length = period_length
  )
  
  period_length <- 7
  weights <- weights_seasonal_vec(
    alphas_seasonal = alphas, n = n, period_length = period_length
  )
  expect_weights_seasonal_matrix(
    weights = weights, alphas = alphas, n = n, period_length = period_length
  )
  
  period_length <- 12
  weights <- weights_seasonal_vec(
    alphas_seasonal = alphas, n = n, period_length = period_length
  )
  expect_weights_seasonal_matrix(
    weights = weights, alphas = alphas, n = n, period_length = period_length
  )
})

test_that("returns the same weights as `weights_seasonal()` for `n=6`", {
  n <- 6
  
  period_length <- 3
  weights <- weights_seasonal_vec(
    alphas_seasonal = alphas, n = n, period_length = period_length
  )
  expect_weights_seasonal_matrix(
    weights = weights, alphas = alphas, n = n, period_length = period_length
  )
  
  period_length <- 5
  weights <- weights_seasonal_vec(
    alphas_seasonal = alphas, n = n, period_length = period_length
  )
  expect_weights_seasonal_matrix(
    weights = weights, alphas = alphas, n = n, period_length = period_length
  )
  
  period_length <- 7
  weights <- weights_seasonal_vec(
    alphas_seasonal = alphas, n = n, period_length = period_length
  )
  expect_weights_seasonal_matrix(
    weights = weights, alphas = alphas, n = n, period_length = period_length
  )
  
  period_length <- 12
  weights <- weights_seasonal_vec(
    alphas_seasonal = alphas, n = n, period_length = period_length
  )
  expect_weights_seasonal_matrix(
    weights = weights, alphas = alphas, n = n, period_length = period_length
  )
})

test_that("returns the same weights as `weights_seasonal()` for `n=8`", {
  n <- 8
  
  period_length <- 3
  weights <- weights_seasonal_vec(
    alphas_seasonal = alphas, n = n, period_length = period_length
  )
  expect_weights_seasonal_matrix(
    weights = weights, alphas = alphas, n = n, period_length = period_length
  )
  
  period_length <- 5
  weights <- weights_seasonal_vec(
    alphas_seasonal = alphas, n = n, period_length = period_length
  )
  expect_weights_seasonal_matrix(
    weights = weights, alphas = alphas, n = n, period_length = period_length
  )
  
  period_length <- 7
  weights <- weights_seasonal_vec(
    alphas_seasonal = alphas, n = n, period_length = period_length
  )
  expect_weights_seasonal_matrix(
    weights = weights, alphas = alphas, n = n, period_length = period_length
  )
  
  period_length <- 12
  weights <- weights_seasonal_vec(
    alphas_seasonal = alphas, n = n, period_length = period_length
  )
  expect_weights_seasonal_matrix(
    weights = weights, alphas = alphas, n = n, period_length = period_length
  )
})

test_that("returns the same weights as `weights_seasonal()` for `n=12`", {
  n <- 12
  
  period_length <- 3
  weights <- weights_seasonal_vec(
    alphas_seasonal = alphas, n = n, period_length = period_length
  )
  expect_weights_seasonal_matrix(
    weights = weights, alphas = alphas, n = n, period_length = period_length
  )
  
  period_length <- 5
  weights <- weights_seasonal_vec(
    alphas_seasonal = alphas, n = n, period_length = period_length
  )
  expect_weights_seasonal_matrix(
    weights = weights, alphas = alphas, n = n, period_length = period_length
  )
  
  period_length <- 7
  weights <- weights_seasonal_vec(
    alphas_seasonal = alphas, n = n, period_length = period_length
  )
  expect_weights_seasonal_matrix(
    weights = weights, alphas = alphas, n = n, period_length = period_length
  )
  
  period_length <- 12
  weights <- weights_seasonal_vec(
    alphas_seasonal = alphas, n = n, period_length = period_length
  )
  expect_weights_seasonal_matrix(
    weights = weights, alphas = alphas, n = n, period_length = period_length
  )
})

test_that("returns the same weights as `weights_seasonal()` for `n=1`", {
  n <- 1
  period_length <- 12
  weights <- weights_seasonal_vec(
    alphas_seasonal = alphas, n = n, period_length = period_length
  )
  expect_weights_seasonal_matrix(
    weights = weights, alphas = alphas, n = n, period_length = period_length
  )
})

test_that("fails for `period_length < 3`", {
  expect_error(
    weights_seasonal_vec(alphas_seasonal = alphas, n = 50, period_length = 2)
  )
  
  expect_error(
    weights_seasonal_vec(alphas_seasonal = alphas, n = 50, period_length = 1)
  )
  
  expect_error(
    weights_seasonal_vec(alphas_seasonal = alphas, n = 50, period_length = 0)
  )
})
