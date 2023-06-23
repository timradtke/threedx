library(checkmate)

# using oracle function because the behavior is entirely determined by `base`,
# edge cases are not handled separately.
# Instead, `learn_weights()` prepares clean input data for the loss functions.

oracle <- function(y_hat, y) {
  sqrt(mean((y - y_hat)^2))
}

expect_identical_to_oracle <- function(y_hat, y, ...) {
  expect_identical(
    loss_rmse(y_hat = y_hat, y = y),
    oracle(y_hat = y_hat, y = y)
  )
}

test_that("returns identical output to oracle", {
  expect_identical_to_oracle(y_hat = rnorm(6274), y = rnorm(6274))
  expect_identical_to_oracle(
    y_hat = rpois(n = 52, lambda = 2),
    y = rpois(n = 52, lambda = 2)
  )
  suppressWarnings(
    expect_identical_to_oracle(
      y_hat = 1:5,
      y = 1:6
    )
  )
  expect_identical_to_oracle(
    y_hat = runif(10),
    y = c(54, 5, 24, 555, 233, NA_real_, 59, 22, -3, -92848.42)
  )
  expect_identical_to_oracle(
    y_hat = runif(10),
    y = rep(NA_real_, 10)
  )
  expect_identical_to_oracle(
    y_hat = numeric(),
    y = numeric()
  )
  expect_identical_to_oracle(
    y_hat = NA_real_,
    y = NA_real_
  )
  expect_identical_to_oracle(
    y_hat = NA_real_,
    y = NA_real_,
    additional_arg = "I don't change anything"
  )
  expect_identical_to_oracle(
    y_hat = 1:10,
    y = 1:10 + 1,
    more = "Just chillin'"
  )
  expect_identical_to_oracle(
    y_hat = -2,
    y = 1
  )
  expect_identical_to_oracle(
    y_hat = 1,
    y = -2
  )
  expect_identical_to_oracle(
    y_hat = rnorm(10000, 10000),
    y = rnorm(10000, -10000)
  )
  expect_identical_to_oracle(
    y_hat = NA,
    y = NA
  )
  expect_identical_to_oracle(
    y_hat = NA,
    y = NA
  )
})

