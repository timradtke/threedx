
library(checkmate)

expect_exponential_weights <- function(x, len, alpha) {
  expect_numeric(
    x = x,
    len = len,
    lower = 0,
    upper = 1,
    finite = TRUE,
    any.missing = FALSE, 
    sorted = TRUE,
    null.ok = FALSE
  )
  
  expect_equal(sum(x), 1)
  
  if (len > 1 && alpha > 0 && alpha < 1) {
    # characteristic of exponential sequences is that if you zoom in, the
    # sequence looks the same as the zoomed-out version
    expect_equal(
      x[seq_len(len - 1)] / sum(x[seq_len(len - 1)]), # zoom-in + re-weight
      weights_exponential(alpha = alpha, n = len - 1)
    )
  }
}

test_that("returns a numeric vector of length `n` that is weakly increasing,
          with values between 0 and 1, sums up to 1", {
  inputs <- expand.grid(
    alpha = seq(0, 1, length.out = 10),
    n = c(1, 2, 5, 10, 100, 1000)
  )
  
  for (i in seq_len(nrow(inputs))) {
    expect_exponential_weights(
      x = weights_exponential(alpha = inputs$alpha[i], n = inputs$n[i]),
      len = inputs$n[i],
      alpha = inputs$alpha[i]
    )
  }
})

test_that("returns random walk weights when `alpha=1`", {
  ns <- c(1, 2, 5, 10, 100, 1000)
  
  for (i in seq_along(ns)) {
    w <- weights_exponential(alpha = 1, n = ns[i])
    expect_exponential_weights(x = w, len = ns[i], alpha = 1)
    
    # random walk means that the most recent weight is 1 and all others are 0
    expect_equal(w[ns[i]], 1)
    expect_equal(sum(w), 1) # if all others are zero, all sum up to 1
  }
})

test_that("returns mean weights when `alpha=0`", {
  ns <- c(1, 2, 5, 10, 100, 1000)
  
  for (i in seq_along(ns)) {
    w <- weights_exponential(alpha = 0, n = ns[i])
    expect_exponential_weights(x = w, len = ns[i], alpha = 0)
    
    # mean weights
    expect_equal(w, rep(1 / ns[i], ns[i]))
  }
})

test_that("throws error for `n=0` because output can't sum to 1", {
  alphas <- seq(0, 1, length.out = 10)
  
  for (i in seq_along(alphas)) {
    expect_error(weights_exponential(alpha = alphas[i], n = 0))
  }
})

test_that("throws error for `alpha` outside [0,1]", {
  inputs <- expand.grid(
    alpha = c(-1, -0.2, 1 + 10^-10, 1.2, 1584),
    n = c(1, 2, 5, 10, 100, 1000)
  )
  
  for (i in seq_len(nrow(inputs))) {
    expect_error(weights_exponential(alpha = inputs$alpha[i], n = inputs$n[i]))
  }
})

test_that("throws error for missing, infinite, or non-scalar `n`", {
  alphas <- seq(0, 1, length.out = 10)
  
  for (i in seq_along(alphas)) {
    expect_error(weights_exponential(alpha = alphas[i], n = NA))
    expect_error(weights_exponential(alpha = alphas[i], n = NA_real_))
    expect_error(weights_exponential(alpha = alphas[i], n = NULL))
    expect_error(weights_exponential(alpha = alphas[i], n = NaN))
    expect_error(weights_exponential(alpha = alphas[i], n = Inf))
    expect_error(weights_exponential(alpha = alphas[i], n = -Inf))
    expect_error(weights_exponential(alpha = alphas[i], n = c(5, 10)))
  }
})

test_that("throws error for missing, infinite, or non-scalar `alpha`", {
  ns <- c(1, 2, 5, 10, 100, 1000)
  
  for (i in seq_along(ns)) {
    expect_error(weights_exponential(alpha = NA, n = ns[i]))
    expect_error(weights_exponential(alpha = NA_real_, n = ns[i]))
    expect_error(weights_exponential(alpha = NULL, n = ns[i]))
    expect_error(weights_exponential(alpha = NaN, n = ns[i]))
    expect_error(weights_exponential(alpha = Inf, n = ns[i]))
    expect_error(weights_exponential(alpha = -Inf, n = ns[i]))
    expect_error(weights_exponential(alpha = c(0.5, 0.1), n = ns[i]))
  }
})
