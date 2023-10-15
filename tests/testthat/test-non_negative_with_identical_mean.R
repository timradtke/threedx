library(checkmate)

test_that("returns non-negative version of input with identical sample mean if sample mean is positive", {
  x <- matrix(data = rnorm(n = 100, mean = 0.5), ncol = 1)
  x_non_negative <- non_negative_with_identical_mean(x = x)
  
  expect_equal(mean(x), mean(x_non_negative))
  expect_numeric(x = as.numeric(x_non_negative), lower = 0)
  expect_matrix(x = x_non_negative, nrows = nrow(x), ncols = 1)
})

test_that("returns input if input is non-negative", {
  x <- matrix(data = rpois(n = 10, lambda = 1), ncol = 1)
  x_non_negative <- non_negative_with_identical_mean(x = x)
  expect_equal(x_non_negative, x)
})

test_that("returns non-negative with different mean if input mean is negative", {
  x <- matrix(data = c(-2, -1, 0, -0.5, -5), ncol = 1)
  x_non_negative <- non_negative_with_identical_mean(x = x)
  x[,1] <- 0
  expect_identical(x_non_negative, x)
})

test_that("returns all-zero if input mean is zero", {
  x <- matrix(data = c(-2, -1, 0, 1, 2), ncol = 1)
  x_non_negative <- non_negative_with_identical_mean(x = x)
  x[,1] <- 0
  expect_identical(x_non_negative, x)
})

test_that("returns all-NA if input mean is NA", {
  x <- matrix(data = c(-2, NA_real_, 0, 1, 2), ncol = 1)
  x_non_negative <- non_negative_with_identical_mean(x = x)
  x[,1] <- NA_real_
  expect_identical(x_non_negative, x)
})

test_that("is idempotent", {
  x <- matrix(data = rnorm(n = 100, mean = 0.5), ncol = 1)
  x_one <- non_negative_with_identical_mean(x = x)
  x_two <- non_negative_with_identical_mean(x = x_one)
  
  expect_identical(x_two, x_one)
})

test_that("returns input if input has zero rows", {
  x <- matrix(ncol = 1, nrow = 0)
  x_non_negative <- non_negative_with_identical_mean(x = x)
  expect_identical(x_non_negative, x)
})
