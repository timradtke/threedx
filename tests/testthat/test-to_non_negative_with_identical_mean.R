library(checkmate)

test_that("returns non-negative version of input with identical sample mean if sample mean is positive", {
  x <- rnorm(n = 100, mean = 0.5)
  x_non_negative <- to_non_negative_with_identical_mean(x = x)
  
  expect_equal(mean(x), mean(x_non_negative))
  expect_numeric(x = x_non_negative, lower = 0, len = length(x))
})

test_that("returns input if input is non-negative", {
  x <- rpois(n = 10, lambda = 1)
  x_non_negative <- to_non_negative_with_identical_mean(x = x)
  expect_equal(x_non_negative, x)
})

test_that("returns non-negative with different mean if input mean is negative", {
  x <- c(-2, -1, 0, -0.5, -5)
  x_non_negative <- to_non_negative_with_identical_mean(x = x)
  expect_identical(x_non_negative, rep(0, length(x)))
})

test_that("returns all-zero if input mean is zero", {
  x <- c(-2, -1, 0, 1, 2)
  x_non_negative <- to_non_negative_with_identical_mean(x = x)
  expect_identical(x_non_negative, rep(0, length(x)))
})

test_that("fails if input mean is NA", {
  x <- c(-2, NA_real_, 0, 1, 2)
  expect_error(to_non_negative_with_identical_mean(x = x))
})

test_that("is idempotent", {
  x <- rnorm(n = 100, mean = 0.5)
  x_one <- to_non_negative_with_identical_mean(x = x)
  x_two <- to_non_negative_with_identical_mean(x = x_one)
  
  expect_identical(x_two, x_one)
})

test_that("returns input if input has zero length", {
  x <- numeric()
  x_non_negative <- to_non_negative_with_identical_mean(x = x)
  expect_identical(x_non_negative, x)
})
