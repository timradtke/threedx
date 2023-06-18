
library(checkmate)

expect_weights_seasonal_decay <- function(x, len, alpha, season_length) {
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
  
  # at most as many distinct values as there are periods
  expect_true(length(unique(x)) <= ceiling(len / season_length))
  
  expect_equal(
    unique(x) / sum(unique(x)),
    weights_exponential(alpha = alpha, n = length(unique(x)))
  ) 
}

test_that(
  "returns a numeric vector fulfilling the exponential seasonal decay conditions",
  {
    inputs <- expand.grid(
      alpha = seq(0, 1, length.out = 5),
      n = c(1, 2, 6, 7, 8, 10, 11, 12, 13, 14, 23, 24, 25, 100, 1000),
      season_length = c(1, 7, 12)
    )
    
    for (i in seq_len(nrow(inputs))) {
      expect_weights_seasonal_decay(
        x = weights_seasonal_decay(
          alpha_seasonal_decay = inputs$alpha[i],
          n = inputs$n[i],
          season_length = inputs$season_length[i]
        ),
        len = inputs$n[i],
        alpha = inputs$alpha[i],
        season_length = inputs$season_length[i]
      )
    }
  }
)

test_that("returns 'random walk' weights when `alpha=1`", {
  inputs <- expand.grid(
    n = c(1, 2, 6, 7, 8, 10, 11, 12, 13, 14, 23, 24, 25, 100, 1000),
    season_length = c(1, 7, 12)
  )
  
  for (i in seq_len(nrow(inputs))) {
    w <- weights_seasonal_decay(
      alpha_seasonal_decay = 1,
      n = inputs$n[i],
      season_length = inputs$season_length[i]
    )
    
    expect_weights_seasonal_decay(
      x = w,
      len = inputs$n[i],
      alpha = 1,
      season_length = inputs$season_length[i]
    )
    
    # random walk means that the most recent `season_length` weights are equal
    # and all others are 0
    last_period_start <- pmax(1, inputs$n[i] - inputs$season_length[i] + 1)
    
    expect_equal(unique(w[last_period_start:inputs$n[i]]), w[inputs$n[i]])
    expect_equal(sum(w[last_period_start:inputs$n[i]]), 1)
    expect_equal(sum(w), 1) # if all others are zero, all sum up to 1
  }
})

test_that("returns mean weights when `alpha=0`", {
  inputs <- expand.grid(
    n = c(1, 2, 6, 7, 8, 10, 11, 12, 13, 14, 23, 24, 25, 100, 1000),
    season_length = c(1, 7, 12)
  )
  
  for (i in seq_len(nrow(inputs))) {
    w <- weights_seasonal_decay(
      alpha_seasonal_decay = 0,
      n = inputs$n[i],
      season_length = inputs$season_length[i]
    )
    
    expect_weights_seasonal_decay(
      x = w,
      len = inputs$n[i],
      alpha = 0,
      season_length = inputs$season_length[i]
    )
    
    # mean weights
    expect_equal(w, rep(1 / inputs$n[i], inputs$n[i]))
  }
})

test_that("throws error for `n=0` because output can't sum to 1", {
  inputs <- expand.grid(
    alpha = seq(0, 1, length.out = 5),
    season_length = c(1, 7, 12)
  )
  
  for (i in seq_len(nrow(inputs))) {
    expect_error(
      weights_seasonal_decay(
        alpha_seasonal_decay = inputs$alpha[i],
        n = 0L,
        season_length = inputs$season_length[i]
      )
    )
  }
})

test_that("throws error for missing, infinite, or non-scalar-integer `n`", {
  inputs <- expand.grid(
    alpha = c(-1, -0.2, 1 + 10^-10, 1.2, 1584),
    season_length = c(1, 7, 12)
  )
  
  for (i in seq_len(nrow(inputs))) {
    tmp_weights <- function(n) {
      weights_seasonal_decay(
        alpha_seasonal_decay = inputs$alpha[i],
        n = n,
        season_length = inputs$season_length[i]
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
    season_length = c(1, 7, 12)
  )
  
  for (i in seq_len(nrow(inputs))) {
    expect_error(
      weights_seasonal_decay(
        alpha_seasonal_decay = inputs$alpha[i],
        n = inputs$n[i],
        season_length = inputs$season_length[i]
      )
    )
  }
})

test_that("throws error for missing, infinite, or non-scalar `alpha`", {
  inputs <- expand.grid(
    n = c(1, 2, 6, 7, 8, 10, 11, 12, 13, 14, 23, 24, 25, 100, 1000),
    season_length = c(1, 7, 12)
  )
  
  for (i in seq_len(nrow(inputs))) {
    tmp_weights <- function(alpha) {
      weights_seasonal_decay(
        alpha_seasonal_decay = alpha,
        n = inputs$n[i],
        season_length = inputs$season_length[i]
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

test_that("throws error for missing, infinite, or non-integerish-scalar `season_length`", {
  inputs <- expand.grid(
    alpha = c(-1, -0.2, 1 + 10^-10, 1.2, 1584),
    n = c(1, 2, 6, 7, 8, 10, 11, 12, 13, 14, 23, 24, 25, 100, 1000)
  )
  
  for (i in seq_len(nrow(inputs))) {
    tmp_weights <- function(season_length) {
      weights_seasonal_decay(
        alpha_seasonal_decay = inputs$alpha[i],
        n = inputs$n[i],
        season_length = season_length
      )
    }
    
    expect_error(tmp_weights(season_length = NA))
    expect_error(tmp_weights(season_length = NA_real_))
    expect_error(tmp_weights(season_length = NULL))
    expect_error(tmp_weights(season_length = NaN))
    expect_error(tmp_weights(season_length = Inf))
    expect_error(tmp_weights(season_length = -Inf))
    expect_error(tmp_weights(season_length = 12.5))
    expect_error(tmp_weights(season_length = -7))
    expect_error(tmp_weights(season_length = c(0.5, 0.1)))
  }
})
