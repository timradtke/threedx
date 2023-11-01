
set.seed(4739)

test_that("returns count data of input length", {
  x <- pmax(0, rnorm(n = 1000, mean = 5, sd = 4))
  y <- to_moment_matched_nbinom(x)
  
  expect_true(all(y %in% seq(0, max(y), by = 1)))
  expect_equal(length(y), length(y))
})

test_that("returns count data of roughly equal mean and variance (negative binomial case)", {
  x <- pmax(0, rnorm(n = 1000, mean = 5, sd = 4))
  y <- to_moment_matched_nbinom(x)
  
  expect_true(all(y %in% seq(0, max(y), by = 1)))
  expect_equal(mean(y), mean(x), tolerance = 0.05)
  expect_equal(var(y), var(x), tolerance = 0.5)
})

test_that("returns count data of roughly equal mean and different variance when variance is less than mean (poisson case)", {
  x <- pmax(0, rnorm(n = 1000, mean = 5, sd = 1.5))
  y <- to_moment_matched_nbinom(x)
  
  expect_true(all(y %in% seq(0, max(y), by = 1)))
  expect_equal(mean(y), mean(x), tolerance = 0.05)
  
  # new variance is closer to mean than to the original variance because data
  # is drawn from Poisson
  expect_lte(abs(var(y) - 5), abs(var(y) - var(x)))
})

test_that("returns rounded input when input is constant (variance is 0)", {
  expect_identical(
    to_moment_matched_nbinom(rep(5, times = 50)),
    round(rep(5, times = 50))
  )
  expect_identical(
    to_moment_matched_nbinom(rep(0, times = 50)),
    round(rep(0, times = 50))
  )
  expect_identical(
    to_moment_matched_nbinom(rep(3.5, times = 50)),
    round(rep(3.5, times = 50))
  )
  expect_identical(
    to_moment_matched_nbinom(rep(3.2, times = 50)),
    round(rep(3.2, times = 50))
  )
  expect_identical(
    to_moment_matched_nbinom(rep(3.7, times = 50)),
    round(rep(3.7, times = 50))
  )
})

test_that("handles negative observations as long as mean is non-negative", {
  x <- rnorm(n = 10000, mean = 2, sd = 5)
  y <- to_moment_matched_nbinom(x)
  expect_true(all(y >= 0))
  
  x <- rnorm(n = 10000, mean = -2, sd = 5)
  expect_error(to_moment_matched_nbinom(x))
})

test_that("fails if `x` has less observations than required to estimate the variance", {
  expect_error(to_moment_matched_nbinom(x = numeric()))
  expect_error(to_moment_matched_nbinom(x = 1))
  expect_error(to_moment_matched_nbinom(x = 6.5))
})

test_that("fails if `x` contains any missing values", {
  expect_error(to_moment_matched_nbinom(x = NA))
  expect_error(to_moment_matched_nbinom(x = rep(NA, 10)))
  expect_error(to_moment_matched_nbinom(x = NA_real_))
  expect_error(to_moment_matched_nbinom(x = c(NA, NA)))
  expect_error(to_moment_matched_nbinom(x = c(1, 4.2, NA, 5.3)))
})
