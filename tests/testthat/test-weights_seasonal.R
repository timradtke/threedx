
library(checkmate)

expect_weights_seasonal <- function(x, len, alpha, period_length) {
  expect_numeric(
    x = x,
    len = len,
    lower = 0,
    upper = 1,
    finite = TRUE,
    any.missing = FALSE, 
    sorted = FALSE,
    null.ok = FALSE
  )
  
  expect_equal(sum(x), 1)
  
  # half as many distinct values as seasons due to symmetry within a period
  expect_true(length(unique(x)) <= period_length / 2 + 1)
  
  if (len >= period_length) {
    expect_equal(x[len - period_length + 1], max(x))
    
    expect_equal(
      x[pmax(1, len - period_length + 1):len] /
        sum(x[pmax(1, len - period_length + 1):len]),
      weights_seasonal(
        alpha_seasonal = alpha, n = period_length, period_length = period_length
      )
    )
  } else {
    expect_equal(x[len], max(x))
  }
}

test_that(
  "returns a numeric vector fulfilling the seasonal exponential conditions",
  {
    inputs <- expand.grid(
      alpha = seq(0, 1, length.out = 5),
      n = c(1, 2, 3, 6, 7, 8, 10, 11, 12, 13, 14, 23, 24, 25, 100, 1000),
      period_length = c(1, 2, 3, 7, 12)
    )
    
    for (i in seq_len(nrow(inputs))) {
      expect_weights_seasonal(
        x = weights_seasonal(
          alpha_seasonal = inputs$alpha[i],
          n = inputs$n[i],
          period_length = inputs$period_length[i]
        ),
        len = inputs$n[i],
        alpha = inputs$alpha[i],
        period_length = inputs$period_length[i]
      )
    }
  }
)

test_that("returns random walk weights when `alpha = 1` and `n < period_length", {
  inputs <- expand.grid(
    n = c(1, 2, 6, 7, 8, 10, 11, 12, 13, 14, 23, 24, 25, 100, 1000),
    period_length = c(1, 2, 3, 7, 12)
  )
  inputs <- inputs[inputs$n < inputs$period_length, ]
  
  for (i in seq_len(nrow(inputs))) {
    w <- weights_seasonal(
      alpha_seasonal = 1,
      n = inputs$n[i],
      period_length = inputs$period_length[i]
    )
    
    expect_weights_seasonal(
      x = w,
      len = inputs$n[i],
      alpha = 1,
      period_length = inputs$period_length[i]
    )
    
    # random walk means that the most recent weight is 1 and all others are 0
    expect_equal(w[inputs$n[i]], 1)
    expect_equal(sum(w), 1) # if all others are zero, all sum up to 1
  }
})

test_that("returns seasonal mean weights when `alpha = 1` and `n >= period_length", {
  inputs <- expand.grid(
    n = c(1, 2, 6, 7, 8, 10, 11, 12, 13, 14, 23, 24, 25, 100, 1000),
    period_length = c(1, 2, 3, 7, 12)
  )
  inputs <- inputs[inputs$n >= inputs$period_length, ]
  
  for (i in seq_len(nrow(inputs))) {
    w <- weights_seasonal(
      alpha_seasonal = 1,
      n = inputs$n[i],
      period_length = inputs$period_length[i]
    )
    
    expect_weights_seasonal(
      x = w,
      len = inputs$n[i],
      alpha = 1,
      period_length = inputs$period_length[i]
    )
    
    # expect same weight for all current period starts, zero everywhere else
    expected_idx <- rev(seq_len(inputs$n[i]) %% inputs$period_length[i] == 0)
    expect_equal(w[expected_idx], rep(1 / sum(expected_idx), sum(expected_idx)))
    expect_equal(w[!expected_idx], rep(0, sum(!expected_idx)))
  }
})

test_that("returns mean weights when `alpha=0`", {
  inputs <- expand.grid(
    n = c(1, 2, 6, 7, 8, 10, 11, 12, 13, 14, 23, 24, 25, 100, 1000),
    period_length = c(1, 2, 3, 7, 12)
  )
  
  for (i in seq_len(nrow(inputs))) {
    w <- weights_seasonal(
      alpha_seasonal = 0,
      n = inputs$n[i],
      period_length = inputs$period_length[i]
    )
    
    expect_weights_seasonal(
      x = w,
      len = inputs$n[i],
      alpha = 0,
      period_length = inputs$period_length[i]
    )
    
    # mean weights
    expect_equal(w, rep(1 / inputs$n[i], inputs$n[i]))
  }
})

test_that("throws error for `n=0` because output can't sum to 1", {
  inputs <- expand.grid(
    alpha = seq(0, 1, length.out = 5),
    period_length = c(1, 2, 3, 7, 12)
  )
  
  for (i in seq_len(nrow(inputs))) {
    expect_error(
      weights_seasonal(
        alpha_seasonal = inputs$alpha[i],
        n = 0L,
        period_length = inputs$period_length[i]
      )
    )
  }
})

test_that("throws error for missing, infinite, or non-scalar-integer `n`", {
  inputs <- expand.grid(
    alpha = c(-1, -0.2, 1 + 10^-10, 1.2, 1584),
    period_length = c(1, 7, 12)
  )
  
  for (i in seq_len(nrow(inputs))) {
    tmp_weights <- function(n) {
      weights_seasonal(
        alpha_seasonal = inputs$alpha[i],
        n = n,
        period_length = inputs$period_length[i]
      )
    }
    
    expect_error(tmp_weights(n = NA))
    expect_error(tmp_weights(n = NA_real_))
    expect_error(tmp_weights(n = NULL))
    expect_error(tmp_weights(n = NaN))
    expect_error(tmp_weights(n = Inf))
    expect_error(tmp_weights(n = -Inf))
    expect_error(tmp_weights(n = 53.5))
    expect_error(tmp_weights(n = -7))
    expect_error(tmp_weights(n = c(0.5, 0.1)))
  }
})

test_that("throws error for `alpha` outside [0,1]", {
  inputs <- expand.grid(
    alpha = c(-1, -0.2, 1 + 10^-10, 1.2, 1584),
    n = c(1, 2, 6, 7, 8, 10, 11, 12, 13, 14, 23, 24, 25, 100, 1000),
    period_length = c(1, 7, 12)
  )
  
  for (i in seq_len(nrow(inputs))) {
    expect_error(
      weights_seasonal(
        alpha_seasonal = inputs$alpha[i],
        n = inputs$n[i],
        period_length = inputs$period_length[i]
      )
    )
  }
})

test_that("throws error for missing, infinite, or non-scalar `alpha`", {
  inputs <- expand.grid(
    n = c(1, 2, 6, 7, 8, 10, 11, 12, 13, 14, 23, 24, 25, 100, 1000),
    period_length = c(1, 7, 12)
  )
  
  for (i in seq_len(nrow(inputs))) {
    tmp_weights <- function(alpha) {
      weights_seasonal(
        alpha_seasonal = alpha,
        n = inputs$n[i],
        period_length = inputs$period_length[i]
      )
    }
    
    expect_error(tmp_weights(alpha = NA))
    expect_error(tmp_weights(alpha = NA_real_))
    expect_error(tmp_weights(alpha = NULL))
    expect_error(tmp_weights(alpha = NaN))
    expect_error(tmp_weights(alpha = Inf))
    expect_error(tmp_weights(alpha = -Inf))
    expect_error(tmp_weights(alpha = c(0.5, 0.1)))
  }
})

test_that("throws error for missing, infinite, or non-integerish-scalar `period_length`", {
  inputs <- expand.grid(
    alpha = c(-1, -0.2, 1 + 10^-10, 1.2, 1584),
    n = c(1, 2, 6, 7, 8, 10, 11, 12, 13, 14, 23, 24, 25, 100, 1000)
  )
  
  for (i in seq_len(nrow(inputs))) {
    tmp_weights <- function(period_length) {
      weights_seasonal(
        alpha_seasonal = inputs$alpha[i],
        n = inputs$n[i],
        period_length = period_length
      )
    }
    
    expect_error(tmp_weights(period_length = NA))
    expect_error(tmp_weights(period_length = NA_real_))
    expect_error(tmp_weights(period_length = NULL))
    expect_error(tmp_weights(period_length = NaN))
    expect_error(tmp_weights(period_length = Inf))
    expect_error(tmp_weights(period_length = -Inf))
    expect_error(tmp_weights(period_length = 12.5))
    expect_error(tmp_weights(period_length = -7))
    expect_error(tmp_weights(period_length = c(0.5, 0.1)))
  }
})
