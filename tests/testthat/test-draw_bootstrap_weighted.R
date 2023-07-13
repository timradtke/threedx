n <- 2743
errors <- 1:100
weight_function <- function(errors) {
  threedx::weights_exponential(alpha = 0.1, n = length(errors))
}

test_that("generates same sequence as `sample()`", {
  set.seed(7399)
  innovations <- draw_bootstrap_weighted(
    n = n, errors = errors, weight_function = weight_function
  )
  
  set.seed(7399)
  expected <- sample(
    x = errors, size = n, replace = TRUE, prob = weight_function(errors)
  )
  
  expect_equal(innovations, expected)
})

test_that("generates sequence based solely on errors of length 1", {
  # `sample(x = 28, size = 50, replace = TRUE)` would fail this test
  expect_equal(
    draw_bootstrap(n = 50, errors = 28, weight_function = weight_function),
    rep(28, times = 50)
  )
})

test_that("fails when any `error` is missing", {
  expect_error(
    draw_bootstrap_weighted(
      n = n,
      errors = rep(NA_real_, times = 57),
      weight_function = weight_function
    )
  )
  
  errors_with_NA <- errors
  errors_with_NA[sample(x = seq_along(errors_with_NA), size = 1)] <- NA_real_
  
  expect_error(
    draw_bootstrap_weighted(
      n = n,
      errors = errors_with_NA,
      weight_function = weight_function
    )
  )
})

test_that("fails when any `error` is infinite", {
  expect_error(
    draw_bootstrap_weighted(
      n = n, errors = rep(Inf, times = 57),
      weight_function = weight_function
    )
  )
  
  errors_with_NA <- errors
  errors_with_NA[sample(x = seq_along(errors_with_NA), size = 1)] <- Inf
  
  expect_error(
    draw_bootstrap_weighted(
      n = n, errors = errors_with_NA,
      weight_function = weight_function
    )
  )
})

test_that("fails when `n` is not integerish", {
  expect_error(draw_bootstrap_weighted(n = NULL, errors = errors,
                                       weight_function = weight_function))
  expect_error(draw_bootstrap_weighted(n = c(57, 333), errors = errors,
                                       weight_function = weight_function))
  expect_error(draw_bootstrap_weighted(n = -47, errors = errors,
                                       weight_function = weight_function))
  expect_error(draw_bootstrap_weighted(n = 5.5, errors = errors,
                                       weight_function = weight_function))
  expect_error(draw_bootstrap_weighted(n = "1", errors = errors,
                                       weight_function = weight_function))
})

test_that("fails when `n` is less than 1", {
  expect_error(draw_bootstrap_weighted(
    n = 0L, errors = errors,
    weight_function = weight_function
  ))
})

test_that("fails when `errors` is of length less than 1", {
  expect_error(draw_bootstrap_weighted(
    n = n,
    errors = numeric(),
    weight_function = weight_function
  ))
})

test_that("fails when `errors` is not numeric vector", {
  expect_error(draw_bootstrap_weighted(
    n = n, errors = NULL,
    weight_function = weight_function
  ))
  expect_error(draw_bootstrap_weighted(
    n = n, errors = list(1:10),
    weight_function = weight_function
  ))
})

test_that("ignores additional passed arguments", {
  set.seed(7399)
  innovations <- draw_bootstrap_weighted(
    n = n,
    errors = errors,
    weight_function = weight_function,
    additional = "argument"
  )
  
  set.seed(7399)
  expected <- sample(
    x = errors, size = n, replace = TRUE, prob = weight_function(errors)
  )
  
  expect_identical(innovations, expected)
})
