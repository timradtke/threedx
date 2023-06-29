
library(checkmate)

model <- learn_weights(
  y = 1:50,
  period_length = 12L,
  alphas_grid = list_sampled_alphas(),
  loss_function = loss_mae
)

n_samples <- 1000L
horizon <- 7L

expect_threedx_paths <- function(forecast,
                                 model,
                                 horizon,
                                 n_samples,
                                 observation_driven) {
  
  expect_class(x = forecast, classes = "threedx_paths")
  expect_list(x = forecast, min.len = 2, names = "unique")
  expect_names(
    x = names(forecast),
    must.include = c("paths", "model", "horizon", "n_samples",
                     "observation_driven")
  )
  expect_identical(model, forecast$model)
  expect_identical(horizon, forecast$horizon)
  expect_identical(n_samples, forecast$n_samples)
  expect_identical(observation_driven, forecast$observation_driven)
  
  expect_matrix(
    x = forecast$paths,
    mode = "numeric",
    any.missing = FALSE,
    nrows = n_samples,
    ncols = horizon
  )
  
  if (observation_driven) {
    expect_true(all(forecast$paths %in% model$y))
  }
}

test_that("returns matrix of dimensions `h`x`n_samples` alongside model object", {
  forecast <- predict(
    object = model,
    horizon = horizon,
    n_samples = n_samples,
    observation_driven = TRUE
    # implicitly testing that `innovation_function` doesn't need to be provided
    # whenever `observation_driven = TRUE`
  )
  
  expect_threedx_paths(
    forecast = forecast,
    model = model,
    horizon = horizon,
    n_samples = n_samples,
    observation_driven = TRUE
  )
  
  forecast <- predict(
    object = model,
    horizon = horizon,
    n_samples = n_samples,
    observation_driven = FALSE,
    innovation_function = function(n, errors) {
      sample(x = errors, size = n, replace = TRUE, prob = NULL)
    }
  )
  
  expect_threedx_paths(
    forecast = forecast,
    model = model,
    horizon = horizon,
    n_samples = n_samples,
    observation_driven = FALSE
  )
})

test_that("returns matrix of dimensions `1`x`1` for edge case `horizon` and `n_samples`", {
  horizon <- 1
  n_samples <- 1
  
  forecast <- predict(
    object = model,
    horizon = horizon,
    n_samples = n_samples,
    observation_driven = TRUE
    # implicitly testing that `innovation_function` doesn't need to be provided
    # whenever `observation_driven = TRUE`
  )
  
  expect_threedx_paths(
    forecast = forecast,
    model = model,
    horizon = horizon,
    n_samples = n_samples,
    observation_driven = TRUE
  )
  
  forecast <- predict(
    object = model,
    horizon = horizon,
    n_samples = n_samples,
    observation_driven = FALSE,
    innovation_function = function(n, errors) {
      sample(x = errors, size = n, replace = TRUE, prob = NULL)
    }
  )
  
  expect_threedx_paths(
    forecast = forecast,
    model = model,
    horizon = horizon,
    n_samples = n_samples,
    observation_driven = FALSE
  )
})

test_that("fails when `horizon=0`", {
  horizon <- 0
  
  expect_error(
    predict(
      object = model,
      horizon = horizon,
      n_samples = n_samples,
      observation_driven = TRUE
      # implicitly testing that `innovation_function` doesn't need to be provided
      # whenever `observation_driven = TRUE`
    )
  )
  
  expect_error(
    predict(
      object = model,
      horizon = horizon,
      n_samples = n_samples,
      observation_driven = FALSE,
      innovation_function = function(n, errors) {
        sample(x = errors, size = n, replace = TRUE, prob = NULL)
      }
    )
  )
})

test_that("fails when `n_samples=0`", {
  n_samples <- 0
  
  expect_error(
    predict(
      object = model,
      horizon = horizon,
      n_samples = n_samples,
      observation_driven = TRUE
      # implicitly testing that `innovation_function` doesn't need to be provided
      # whenever `observation_driven = TRUE`
    )
  )
  
  expect_error(
    predict(
      object = model,
      horizon = horizon,
      n_samples = n_samples,
      observation_driven = FALSE,
      innovation_function = function(n, errors) {
        sample(x = errors, size = n, replace = TRUE, prob = NULL)
      }
    )
  )
})

test_that("fails when `innovation_function` is not provided and `observation_driven = FALSE`", {
  expect_error(
    predict(
      object = model,
      horizon = horizon,
      n_samples = n_samples,
      observation_driven = FALSE
    )
  )
})

test_that("works when `innovation_function` is not provided and `observation_driven = TRUE`", {
  forecast <- predict(
    object = model,
    horizon = horizon,
    n_samples = n_samples,
    observation_driven = TRUE
  )
  
  expect_threedx_paths(
    forecast = forecast,
    model = model,
    horizon = horizon,
    n_samples = n_samples,
    observation_driven = TRUE
  )
})

test_that("fails when `innovation_function` doesn't provide required arguments", {
  expect_error(
    predict(
      object = model,
      horizon = horizon,
      n_samples = n_samples,
      observation_driven = FALSE,
      innovation_function = function(n) {
        sample(x = 1, size = n, replace = TRUE, prob = NULL)
      }
    )
  )
  
  expect_error(
    predict(
      object = model,
      horizon = horizon,
      n_samples = n_samples,
      observation_driven = FALSE,
      innovation_function = function(errors) {
        sample(x = errors, size = 1L, replace = TRUE, prob = NULL)
      }
    )
  )
  
  expect_error(
    predict(
      object = model,
      horizon = horizon,
      n_samples = n_samples,
      observation_driven = FALSE,
      innovation_function = function(size, x) {
        sample(x = x, size = size, replace = TRUE, prob = NULL)
      }
    )
  )
})

test_that("fails when `model$residuals` are all NA and `observation_driven = FALSE`", {
  model$residuals <- rep(NA_real_, length(model$residuals))
  
  expect_error(
    predict(
      object = model,
      horizon = horizon,
      n_samples = n_samples,
      observation_driven = FALSE,
      innovation_function = function(n, errors) {
        sample(x = errors, size = n, replace = TRUE, prob = NULL)
      }
    )
  )
  
  # still works for `observation_driven = TRUE`, though
  
  forecast <- predict(
    object = model,
    horizon = horizon,
    n_samples = n_samples,
    observation_driven = TRUE
  )
  
  expect_threedx_paths(
    forecast = forecast,
    model = model,
    horizon = horizon,
    n_samples = n_samples,
    observation_driven = TRUE
  )
})

test_that("fails when `model$residuals` are zero-length", {
  model$residuals <- numeric(length = 0L)
  
  expect_error(
    predict(
      object = model,
      horizon = horizon,
      n_samples = n_samples,
      observation_driven = FALSE,
      innovation_function = function(n, errors) {
        sample(x = errors, size = n, replace = TRUE, prob = NULL)
      }
    )
  )
  
  model$residuals <- NULL
  
  expect_error(
    predict(
      object = model,
      horizon = horizon,
      n_samples = n_samples,
      observation_driven = FALSE,
      innovation_function = function(n, errors) {
        sample(x = errors, size = n, replace = TRUE, prob = NULL)
      }
    )
  )
  
  # still works for `observation_driven = TRUE`, though
  
  forecast <- predict(
    object = model,
    horizon = horizon,
    n_samples = n_samples,
    observation_driven = TRUE
  )
  
  expect_threedx_paths(
    forecast = forecast,
    model = model,
    horizon = horizon,
    n_samples = n_samples,
    observation_driven = TRUE
  )
})
