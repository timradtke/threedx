
library(checkmate)

set.seed(5728)

alphas_single <- data.frame(
  alpha = 0.5,
  alpha_seasonal = 0.5,
  alpha_seasonal_decay = 0.5
)

alphas_random_walk <- data.frame(
  alpha = 1,
  alpha_seasonal = 0,
  alpha_seasonal_decay = 0
)

alphas_seasonal_naive <- data.frame(
  alpha = 0,
  alpha_seasonal = 1,
  alpha_seasonal_decay = 1
)

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
    period_length = period_length,
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
  expect_identical(model$period_length, period_length)
  expect_identical(model$y, y)
  expect_identical(
    model$residuals,
    c(rep(NA_real_, period_length), rep(1, (n_obs - period_length)))
  )
  
  expect_identical(model$full$best_alphas_idx, 1L)
  expect_identical(model$full$best_alphas, alphas_random_walk)
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
    period_length = period_length,
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
  expect_identical(model$period_length, period_length)
  expect_identical(model$y, y)
  expect_identical(
    model$residuals,
    c(rep(NA_real_, period_length), rep(12, (n_obs - period_length)))
  )
  
  expect_identical(model$full$best_alphas_idx, 1L)
  expect_identical(model$full$best_alphas, alphas_seasonal_naive)
  expect_identical(
    model$full$residuals,
    matrix(model$residuals, ncol = 1)
  )
  expect_identical(
    model$full$fitted,
    matrix(model$fitted, ncol = 1)
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
    period_length = period_length,
    alphas_grid = rbind(
      alphas_single, alphas_random_walk, alphas_seasonal_naive
    ),
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
  
  expect_identical(model$alpha, alphas_random_walk[[1]])
  expect_identical(model$alpha_seasonal, alphas_random_walk[[2]])
  expect_identical(model$alpha_seasonal_decay, alphas_random_walk[[3]])
  expect_identical(model$n, n_obs)
  expect_identical(model$period_length, period_length)
  expect_identical(model$y, y)
  expect_identical(
    model$residuals,
    c(rep(NA_real_, period_length), rep(1, (n_obs - period_length)))
  )
  
  expect_identical(model$full$best_alphas_idx, 2L)
  expect_identical(model$full$best_alphas, alphas_random_walk)
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

expect_threedx <- function(model, y, period_length, alphas_grid) {
  expect_class(model, "threedx")
  
  n_obs <- length(y)
  n_params <- nrow(alphas_grid)
  
  n_initial_period <- period_length
  if (n_obs <= 2 * period_length) {
    n_initial_period <- max(1, n_obs - period_length)
  }
  
  expect_numeric(x = model$fitted, all.missing = FALSE, len = n_obs)
  expect_numeric(x = model$residuals, all.missing = FALSE, len = n_obs)
  expect_identical(
    model$fitted[seq_len(n_initial_period)],
    rep(NA_real_, n_initial_period)
  )
  expect_identical(
    model$residuals,
    y - model$fitted
  )
  
  expect_numeric(
    x = model$weights, lower = 0, upper = 1, any.missing = FALSE, len = n_obs,
    finite = TRUE
  )
  expect_equal(sum(model$weights), 1)
  
  expect_numeric(x = model$alpha, lower = 0, upper = 1, len = 1, any.missing = FALSE)
  expect_numeric(x = model$alpha_seasonal, lower = 0, upper = 1, len = 1, any.missing = FALSE)
  expect_numeric(x = model$alpha_seasonal_decay, lower = 0, upper = 1, len = 1, any.missing = FALSE)
  expect_integer(model$n, n_obs)
  expect_identical(model$period_length, period_length)
  expect_identical(model$y, y)
  
  expect_integerish(x = model$full$best_alphas_idx, lower = 1, any.missing = FALSE, len = 1)
  
  expect_matrix(
    x = model$full$residuals, mode = "numeric", nrows = n_obs, ncols = n_params,
  )
  expect_matrix(
    x = model$full$fitted, mode = "numeric", nrows = n_obs, ncols = n_params
  )
  
  expect_identical(
    model$full$residuals[seq_len(n_initial_period), , drop = FALSE],
    matrix(NA_real_, nrow = n_initial_period, ncol = n_params)
  )
  expect_identical(
    model$full$fitted[seq_len(n_initial_period), , drop = FALSE],
    matrix(NA_real_, nrow = n_initial_period, ncol = n_params)
  )
  
  expect_matrix(
    x = model$full$residuals[-seq_len(n_initial_period), , drop = FALSE],
    mode = "numeric", nrows = n_obs - n_initial_period, ncols = n_params,
    any.missing = FALSE
  )
  expect_matrix(
    x = model$full$fitted[-seq_len(n_initial_period), , drop = FALSE],
    mode = "numeric", nrows = n_obs - n_initial_period, ncols = n_params,
    any.missing = FALSE
  )
  
  expect_function(x = model$loss_function, args = c("y_hat", "y"))
}

test_that("returns expected `threedx` object for range of inputs", {
  
  inputs <- list(
    list(
      y = rnorm(n = 100, mean = 1483, sd = 33),
      period_length = 7L,
      alphas_grid = alphas_grid_sampled
    ),
    list(
      y = rpois(n = 34, lambda = 2),
      period_length = 12L,
      alphas_grid = alphas_grid_sampled
    ),
    list(
      y = 1:3,
      period_length = 7L,
      alphas_grid = alphas_grid_sampled
    ),
    list(
      y = 1:3,
      period_length = 3L,
      alphas_grid = alphas_grid_sampled
    )
  )
  
  lapply(
    X = inputs,
    FUN = function(input) {
      expect_threedx(
        model = learn_weights(
          y = input$y,
          period_length = input$period_length,
          alphas_grid = input$alphas_grid,
          loss_function = loss_function
        ),
        y = input$y,
        period_length = input$period_length,
        alphas_grid = input$alphas_grid
      )
    }
  )
})

test_that("returns expected `threedx` object for even and odd time series length", {
  
  inputs <- list(
    list(
      y = rnorm(n = 36, mean = 1483, sd = 33),
      period_length = 7L,
      alphas_grid = alphas_grid_sampled
    ),
    list(
      y = rnorm(n = 37, mean = 1483, sd = 33),
      period_length = 7L,
      alphas_grid = alphas_grid_sampled
    ),
    list(
      y = rnorm(n = 3, mean = 1483, sd = 33),
      period_length = 7L,
      alphas_grid = alphas_grid_sampled
    ),
    list(
      y = rnorm(n = 4, mean = 1483, sd = 33),
      period_length = 7L,
      alphas_grid = alphas_grid_sampled
    ),
    list(
      y = rpois(n = 34, lambda = 2),
      period_length = 12L,
      alphas_grid = alphas_grid_sampled
    ),
    list(
      y = rpois(n = 35, lambda = 2),
      period_length = 12L,
      alphas_grid = alphas_grid_sampled
    ),
    list(
      y = rpois(n = 3, lambda = 2),
      period_length = 12L,
      alphas_grid = alphas_grid_sampled
    ),
    list(
      y = rpois(n = 4, lambda = 2),
      period_length = 12L,
      alphas_grid = alphas_grid_sampled
    ),
    list(
      y = 1:3,
      period_length = 3L,
      alphas_grid = alphas_grid_sampled
    ),
    list(
      y = 1:4,
      period_length = 3L,
      alphas_grid = alphas_grid_sampled
    )
  )
  
  models <- lapply(
    X = inputs,
    FUN = function(input) {
      learn_weights(
        y = input$y,
        period_length = input$period_length,
        alphas_grid = input$alphas_grid,
        loss_function = loss_function
      )
    }
  )
  
  lapply(
    X = inputs,
    FUN = function(input) {
      expect_threedx(
        learn_weights(
          y = input$y,
          period_length = input$period_length,
          alphas_grid = input$alphas_grid,
          loss_function = loss_function
        ),
        y = input$y,
        period_length = input$period_length,
        alphas_grid = input$alphas_grid
      )
    }
  )
})

test_that("penalizes model complexity when `penalize = TRUE`", {
  # because the time series is entirely zero-valued, all model specifications
  # achieve exactly zero loss; normally the first model specification would
  # be returned, but here it is the third as the third is the first
  # specification with the lowest complexity
  model <- learn_weights(
    y = rep(0, times = 50),
    period_length = 12L,
    alphas_grid = data.frame(
      alpha = c(0.1, 0, 0, 0),
      alpha_seasonal = c(0.5, 0.9, 0, 1),
      alpha_seasonal_decay = c(0.01, 0, 0, 1)
    ),
    loss_function = loss_mae,
    penalize = TRUE,
    loss_increase = 5
  )
  
  expect_equal(model$alpha, 0)
  expect_equal(model$alpha_seasonal, 0)
  expect_equal(model$alpha_seasonal_decay, 0)
  expect_equal(model$full$best_alphas_idx, 3)
  expect_true(model$penalize)
  expect_equal(model$loss_increase, 5)
})

test_that("fails when `y` is provided as time series object", {
  expect_error(
    learn_weights(
      y = stats::ts(rnorm(100), frequency = 12),
      period_length = 12L,
      alphas_grid = alphas_grid_sampled,
      loss_function = loss_mae
    )
  )
})

test_that("fails when `y` has missing observations", {
  period_length <- 12L
  n_obs <- 50L
  
  y <- seq_len(n_obs)
  y[sample(x = seq_along(y), size = 3, replace = FALSE)] <- NA_real_
  
  expect_error(
    learn_weights(
      y = y,
      period_length = period_length,
      alphas_grid = alphas_random_walk,
      loss_function = loss_function
    )
  )
  
  y <- seq_len(n_obs)
  y[1] <- NA_real_
  
  expect_error(
    learn_weights(
      y = y,
      period_length = period_length,
      alphas_grid = alphas_random_walk,
      loss_function = loss_function
    )
  )
  
  y <- seq_len(n_obs)
  y[n_obs] <- NA_real_
  
  expect_error(
    learn_weights(
      y = y,
      period_length = period_length,
      alphas_grid = alphas_random_walk,
      loss_function = loss_function
    )
  )
  
  y <- rep(NA_real_, n_obs)
  
  expect_error(
    learn_weights(
      y = y,
      period_length = period_length,
      alphas_grid = alphas_random_walk,
      loss_function = loss_function
    )
  )
})

test_that("fails when `y` is shorter than two observations", {
  expect_error(
    learn_weights(
      y = 1,
      period_length = 12L,
      alphas_grid = alphas_grid_sampled,
      loss_function = loss_function
    )
  )
  
  expect_error(
    learn_weights(
      y = numeric(0L),
      period_length = 12L,
      alphas_grid = alphas_grid_sampled,
      loss_function = loss_function
    )
  )
  
  expect_error(
    learn_weights(
      y = rnorm(1),
      period_length = 1L,
      alphas_grid = alphas_grid_sampled,
      loss_function = loss_function
    )
  )
  
  expect_error(
    learn_weights(
      y = numeric(0L),
      period_length = 1L,
      alphas_grid = alphas_grid_sampled,
      loss_function = loss_function
    )
  )
})

test_that("fails when `alphas_grid` is not a list", {
  period_length <- 7L
  n_obs <- 50L
  y <- seq_len(n_obs)
  
  expect_error(
    learn_weights(
      y = y,
      period_length = period_length,
      alphas_grid = c(0.5, 0.5, 0.5),
      loss_function = loss_function
    )
  )
  
  expect_error(
    learn_weights(
      y = y,
      period_length = period_length,
      alphas_grid = NULL,
      loss_function = loss_function
    )
  )
})

test_that("fails when `period_length` is less than 3", {
  n_obs <- 50L
  y <- seq_len(n_obs)
  
  expect_error(
    learn_weights(
      y = y,
      period_length = 2,
      alphas_grid = alphas_grid_sampled,
      loss_function = loss_function
    )
  )
  
  expect_error(
    learn_weights(
      y = y,
      period_length = 1,
      alphas_grid = alphas_grid_sampled,
      loss_function = loss_function
    )
  )
})

test_that("fails when `period_length` is misspecified as missing or not positive integer", {
  n_obs <- 50L
  y <- seq_len(n_obs)
  
  expect_error(
    learn_weights(
      y = y,
      period_length = NA,
      alphas_grid = alphas_grid_sampled,
      loss_function = loss_function
    )
  )
  
  expect_error(
    learn_weights(
      y = y,
      period_length = NA_real_,
      alphas_grid = alphas_grid_sampled,
      loss_function = loss_function
    )
  )
  
  expect_error(
    learn_weights(
      y = y,
      period_length = NULL,
      alphas_grid = alphas_grid_sampled,
      loss_function = loss_function
    )
  )
  
  expect_error(
    learn_weights(
      y = y,
      period_length = 12.5,
      alphas_grid = alphas_grid_sampled,
      loss_function = loss_function
    )
  )
  
  expect_error(
    learn_weights(
      y = y,
      period_length = 0L,
      alphas_grid = alphas_grid_sampled,
      loss_function = loss_function
    )
  )
  
  expect_error(
    learn_weights(
      y = y,
      period_length = -7,
      alphas_grid = alphas_grid_sampled,
      loss_function = loss_function
    )
  )
})

test_that("fails when `loss_function` does not provide arguments `y_hat` and `...`", {
  n_obs <- 50L
  y <- seq_len(n_obs)
  period_length <- 12L
  
  expect_error(
    learn_weights(
      y = y,
      period_length = period_length,
      alphas_grid = alphas_grid_sampled,
      loss_function = NULL
    )
  )
  
  expect_error(
    learn_weights(
      y = y,
      period_length = period_length,
      alphas_grid = alphas_grid_sampled,
      loss_function = function(y) { # no `y_hat`, `...`
        mean(abs(y), na.rm = TRUE)
      }
    )
  )
  
  expect_error(
    learn_weights(
      y = y,
      period_length = period_length,
      alphas_grid = alphas_grid_sampled,
      loss_function = function(y_hat, y) { # no `...`
        mean(abs(y - y_hat), na.rm = TRUE)
      }
    )
  )
})
