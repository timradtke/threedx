
library(checkmate)

set.seed(5728)

alphas_single <- list(c(0.5, 0.5, 0.5))
alphas_random_walk <- list(c(1, 0, 0))
alphas_seasonal_naive <- list(c(0, 1, 1))
alphas_grid_sampled <- list_sampled_alphas(n_target = 25)
loss_function <- function(y_hat, y, ...) {
  mean(abs(y - y_hat))
}

test_that("random walk parameters generate random walk model", {
  period_length <- 7L
  n_obs <- 50L
  y <- seq_len(n_obs)
  
  model <- learn_weights(
    y = y,
    season_length = period_length,
    alphas_grid = alphas_random_walk,
    loss_function = loss_function
  )
  
  expect_class(model, "threedx")
  
  expect_identical(
      model$fitted,
      c(rep(NA_real_, period_length), period_length:(n_obs - 1))
  )
  
  expect_identical(
    model$weights,
    c(rep(0, (n_obs - 1)), 1)
  )
  
  expect_identical(model$alpha, 1)
  expect_identical(model$alpha_seasonal, 0)
  expect_identical(model$alpha_seasonal_decay, 0)
  expect_identical(model$n, n_obs)
  expect_identical(model$season_length, period_length)
  expect_identical(model$y, y)
  expect_identical(
    model$residuals,
    c(rep(NA_real_, period_length), rep(1, (n_obs - period_length)))
  )
  
  expect_identical(model$full$best_alphas_idx, 1L)
  expect_identical(model$full$best_alphas, unlist(alphas_random_walk))
  expect_identical(
    model$full$residuals,
    matrix(model$residuals, ncol = 1)
  )
  expect_identical(
    model$full$fitted,
    matrix(model$fitted, ncol = 1)
  )
})

test_that("seasonal naive parameters generate seasonal naive model", {
  period_length <- 12L
  n_obs <- 50L
  y <- seq_len(n_obs)
  
  model <- learn_weights(
    y = y,
    season_length = period_length,
    alphas_grid = alphas_seasonal_naive,
    loss_function = loss_function
  )
  
  # TODO: Abstract these expectations into a function for seasonal naive,
  #       then test with different params
  
  expect_class(model, "threedx")
  
  expect_identical(
    model$fitted,
    c(rep(NA_real_, period_length), seq_len(n_obs - period_length))
  )
  
  expect_identical(
    model$weights,
    c(rep(0, n_obs - period_length), c(1, rep(0, period_length - 1)))
    # rev(rep(c(1, rep(0, times = period_length - 1)), length.out = n_obs))
  )
  
  expect_identical(model$alpha, 0)
  expect_identical(model$alpha_seasonal, 1)
  expect_identical(model$alpha_seasonal_decay, 1)
  expect_identical(model$n, n_obs)
  expect_identical(model$season_length, period_length)
  expect_identical(model$y, y)
  expect_identical(
    model$residuals,
    c(rep(NA_real_, period_length), rep(12, (n_obs - period_length)))
  )
  
  expect_identical(model$full$best_alphas_idx, 1L)
  expect_identical(model$full$best_alphas, unlist(alphas_seasonal_naive))
  expect_identical(
    model$full$residuals,
    matrix(model$residuals, ncol = 1)
  )
  expect_identical(
    model$full$fitted,
    matrix(model$fitted, ncol = 1)
  )
})

test_that("fails when time series has missing observations", {
  period_length <- 12L
  n_obs <- 50L
  
  y <- seq_len(n_obs)
  y[sample(x = seq_along(y), size = 3, replace = FALSE)] <- NA_real_
  
  expect_error(
    learn_weights(
      y = y,
      season_length = period_length,
      alphas_grid = alphas_random_walk,
      loss_function = loss_function
    )
  )
  
  y <- seq_len(n_obs)
  y[1] <- NA_real_
  
  expect_error(
    learn_weights(
      y = y,
      season_length = period_length,
      alphas_grid = alphas_random_walk,
      loss_function = loss_function
    )
  )
  
  y <- seq_len(n_obs)
  y[n_obs] <- NA_real_
  
  expect_error(
    learn_weights(
      y = y,
      season_length = period_length,
      alphas_grid = alphas_random_walk,
      loss_function = loss_function
    )
  )
  
  y <- rep(NA_real_, n_obs)
  
  expect_error(
    learn_weights(
      y = y,
      season_length = period_length,
      alphas_grid = alphas_random_walk,
      loss_function = loss_function
    )
  )
})

test_that("picks random walk as best parameters for deterministic case", {
  period_length <- 12L
  n_obs <- 53L
  
  # no noise, increasing by 1 every month; while this is not a random walk
  # model, the random walk will have the smallest residuals for this case
  # as they will be exactly 1 every month
  y <- seq_len(n_obs)
  
  model <- learn_weights(
    y = y,
    season_length = period_length,
    alphas_grid = c(alphas_single, alphas_random_walk, alphas_seasonal_naive),
    loss_function = function(y_hat, y, ...) {
      mean(abs(y - y_hat))
    }
  )
  
  expect_class(model, "threedx")
  
  expect_identical(
    model$fitted,
    c(rep(NA_real_, period_length), period_length:(n_obs - 1))
  )
  
  expect_identical(
    model$weights,
    c(rep(0, (n_obs - 1)), 1)
  )
  
  expect_identical(model$alpha, alphas_random_walk[[1]][1])
  expect_identical(model$alpha_seasonal, alphas_random_walk[[1]][2])
  expect_identical(model$alpha_seasonal_decay, alphas_random_walk[[1]][3])
  expect_identical(model$n, n_obs)
  expect_identical(model$season_length, period_length)
  expect_identical(model$y, y)
  expect_identical(
    model$residuals,
    c(rep(NA_real_, period_length), rep(1, (n_obs - period_length)))
  )
  
  expect_identical(model$full$best_alphas_idx, 2L)
  expect_identical(model$full$best_alphas, unlist(alphas_random_walk))
  expect_identical(
    model$full$residuals[, 2, drop = FALSE],
    matrix(model$residuals, ncol = 1)
  )
  expect_identical(
    model$full$fitted[, 2, drop = FALSE],
    matrix(model$fitted, ncol = 1)
  )
  expect_identical(
    dim(model$full$residuals),
    c(n_obs, 3L)
  )
  expect_identical(
    dim(model$full$fitted),
    c(n_obs, 3L)
  )
  expect_true(
    all(is.na(model$full$residuals[seq_len(period_length), ]))
  )
  expect_true(
    !anyNA(model$full$residuals[-seq_len(period_length), ])
  )
})

