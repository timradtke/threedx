n <- 2743
errors <- 1:100

test_that("generates same sequence as `sample()`", {
  set.seed(7399)
  innovations <- draw_bootstrap(n = n, errors = errors)
  
  set.seed(7399)
  expected <- sample(x = errors, size = n, replace = TRUE, prob = NULL)
  
  expect_equal(innovations, expected)
})

test_that("fails when any `error` is missing", {
  expect_error(
    draw_bootstrap(n = n, errors = rep(NA_real_, times = 57))
  )
  
  errors_with_NA <- errors
  errors_with_NA[sample(x = seq_along(errors_with_NA), size = 1)] <- NA_real_
  
  expect_error(
    draw_bootstrap(n = n, errors = errors_with_NA)
  )
})

test_that("fails when any `error` is infinite", {
  expect_error(
    draw_bootstrap(n = n, errors = rep(Inf, times = 57))
  )
  
  errors_with_NA <- errors
  errors_with_NA[sample(x = seq_along(errors_with_NA), size = 1)] <- Inf
  
  expect_error(
    draw_bootstrap(n = n, errors = errors_with_NA)
  )
})

test_that("fails when `n` is not integerish", {
  expect_error(draw_bootstrap(n = NULL, errors = errors))
  expect_error(draw_bootstrap(n = c(57, 333), errors = errors))
  expect_error(draw_bootstrap(n = -47, errors = errors))
  expect_error(draw_bootstrap(n = 5.5, errors = errors))
  expect_error(draw_bootstrap(n = "1", errors = errors))
})

test_that("fails when `n` is less than 1", {
  expect_error(draw_bootstrap(n = 0L, errors = errors))
})

test_that("fails when `errors` is not numeric vector", {
  expect_error(draw_bootstrap(n = n, errors = NULL))
  expect_error(draw_bootstrap(n = n, errors = list(1:10)))
})

test_that("ignores additional passed arguments", {
  set.seed(7399)
  innovations <- draw_bootstrap(
    n = n, errors = errors, additional = "argument"
  )
  
  set.seed(7399)
  expected <- sample(x = errors, size = n, replace = TRUE, prob = NULL)
  
  expect_identical(innovations, expected)
})
