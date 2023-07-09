library(checkmate)

test_that("returns TRUE when `k` largest weights sum to less than `p` percent", {
  expect_true(
    k_largest_weights_sum_to_less_than_p_percent(
      weights = c(0.9, 0.1),
      k = 1,
      p = 0.9
    )
  )
  
  expect_true(
    k_largest_weights_sum_to_less_than_p_percent(
      weights = c(0.5, 0.2, 0.1, 0.1, 0.05, 0.05),
      k = 2,
      p = 0.9
    )
  )
  
  expect_true(
    k_largest_weights_sum_to_less_than_p_percent(
      weights = c(0.5, 0.2, 0.1, 0.1, 0.05, 0.05),
      k = 3,
      p = 0.9
    )
  )
  
  expect_true(
    k_largest_weights_sum_to_less_than_p_percent(
      weights = c(0.5, 0.2, 0.15, 0.1, 0.05),
      k = 2,
      p = 0.8
    )
  )
  
  expect_true(
    k_largest_weights_sum_to_less_than_p_percent(
      weights = c(0.5, 0.2, 0.15, 0.1, 0.05),
      k = 3,
      p = 0.95
    )
  )
})

test_that("returns FALSE when `k` largest weights sum to more than `p` percent", {
  expect_false(
    k_largest_weights_sum_to_less_than_p_percent(
      weights = c(0.9, 0.1),
      k = 1,
      p = 0.8
    )
  )
  
  expect_false(
    k_largest_weights_sum_to_less_than_p_percent(
      weights = c(0.5, 0.2, 0.1, 0.1, 0.05, 0.05),
      k = 3,
      p = 0.75
    )
  )
  
  expect_false(
    k_largest_weights_sum_to_less_than_p_percent(
      weights = c(0.5, 0.2, 0.1, 0.1, 0.05, 0.05),
      k = 4,
      p = 0.75
    )
  )
  
  expect_false(
    k_largest_weights_sum_to_less_than_p_percent(
      weights = c(0.5, 0.2, 0.15, 0.1, 0.05),
      k = 4,
      p = 0.6
    )
  )
  
  expect_false(
    k_largest_weights_sum_to_less_than_p_percent(
      weights = c(0.5, 0.2, 0.15, 0.1, 0.05),
      k = 3,
      p = 0.6
    )
  )
})

test_that("handles non-decreasing order of weights", {
  expect_true(
    k_largest_weights_sum_to_less_than_p_percent(
      weights = c(0.1, 0.15, 0.5, 0.2, 0.05),
      k = 2,
      p = 0.8
    )
  )
  
  expect_false(
    k_largest_weights_sum_to_less_than_p_percent(
      weights = c(0.1, 0.15, 0.5, 0.2, 0.05),
      k = 3,
      p = 0.6
    )
  )
})

test_that("handles `weights` shorter than `k`, returning FALSE unless `p=1`", {
  expect_false(
    k_largest_weights_sum_to_less_than_p_percent(
      weights = c(0.6, 0.4),
      k = 3,
      p = 0.6
    )
  )
  
  expect_false(
    k_largest_weights_sum_to_less_than_p_percent(
      weights = 1,
      k = 5,
      p = 0.2
    )
  )
  
  expect_false(
    k_largest_weights_sum_to_less_than_p_percent(
      weights = numeric(),
      k = 5,
      p = 0.2
    )
  )
  
  expect_true(
    k_largest_weights_sum_to_less_than_p_percent(
      weights = c(0.6, 0.4),
      k = 3,
      p = 1
    )
  )
})

test_that("returns TRUE when `p=1`", {
  expect_true(
    k_largest_weights_sum_to_less_than_p_percent(
      weights = c(0.2, 0.5, 0.3),
      k = 1,
      p = 1
    )
  )
  
  expect_true(
    k_largest_weights_sum_to_less_than_p_percent(
      weights = c(0.2, 0.5, 0.3),
      k = 3,
      p = 1
    )
  )
  
  expect_true(
    k_largest_weights_sum_to_less_than_p_percent(
      weights = 1,
      k = 3,
      p = 1
    )
  )
})

test_that("returns FALSE when `p=0`", {
  expect_false(
    k_largest_weights_sum_to_less_than_p_percent(
      weights = c(0, 0.2, 0.5, 0.3),
      k = 1,
      p = 0
    )
  )
  
  expect_false(
    k_largest_weights_sum_to_less_than_p_percent(
      weights = c(0.2, 0.5, 0.3),
      k = 1,
      p = 0
    )
  )
  
  expect_false(
    k_largest_weights_sum_to_less_than_p_percent(
      weights = c(0.2, 0.5, 0.3),
      k = 3,
      p = 0
    )
  )
  
  expect_false(
    k_largest_weights_sum_to_less_than_p_percent(
      weights = 1,
      k = 3,
      p = 0
    )
  )
})

test_that("breaks tied weights", {
  expect_true(
    k_largest_weights_sum_to_less_than_p_percent(
      weights = c(0.2, 0.2, 0.2, 0.2, 0.2),
      k = 3, 
      p = 0.7
    )
  )
  
  expect_false(
    k_largest_weights_sum_to_less_than_p_percent(
      weights = c(0.2, 0.2, 0.2, 0.2, 0.2),
      k = 3, 
      p = 0.5
    )
  )
  
  expect_false(
    k_largest_weights_sum_to_less_than_p_percent(
      weights = c(0.25, 0.25, 0.25, 0.25),
      k = 3, 
      p = 0.5
    )
  )
  
  expect_true(
    k_largest_weights_sum_to_less_than_p_percent(
      weights = c(0.25, 0.25, 0.25, 0.25),
      k = 2, 
      p = 0.5
    )
  )
})

test_that("fails when p is not scalar in [0,1]", {
  expect_error(
    k_largest_weights_sum_to_less_than_p_percent(
      weights = c(0.5, 0.2, 0.1, 0.1, 0.05, 0.05),
      k = 2,
      p = 1.1
    )
  )
  
  expect_error(
    k_largest_weights_sum_to_less_than_p_percent(
      weights = c(0.5, 0.2, 0.1, 0.1, 0.05, 0.05),
      k = 2,
      p = -0.5
    )
  )
  
  expect_error(
    k_largest_weights_sum_to_less_than_p_percent(
      weights = c(0.5, 0.2, 0.1, 0.1, 0.05, 0.05),
      k = 2,
      p = c(0.5, 0.2)
    )
  )
  
  expect_error(
    k_largest_weights_sum_to_less_than_p_percent(
      weights = c(0.5, 0.2, 0.1, 0.1, 0.05, 0.05),
      k = 2,
      p = NA_real_
    )
  )
  
  expect_error(
    k_largest_weights_sum_to_less_than_p_percent(
      weights = c(0.5, 0.2, 0.1, 0.1, 0.05, 0.05),
      k = 2,
      p = NULL
    )
  )
})

test_that("fails when k is not a strictly positive integer scalar", {
  expect_error(
    k_largest_weights_sum_to_less_than_p_percent(
      weights = c(0.5, 0.2, 0.1, 0.1, 0.05, 0.05),
      k = 0,
      p = 0.5
    )
  )
  
  expect_error(
    k_largest_weights_sum_to_less_than_p_percent(
      weights = c(0.5, 0.2, 0.1, 0.1, 0.05, 0.05),
      k = 2.5,
      p = 0.5
    )
  )
  
  expect_error(
    k_largest_weights_sum_to_less_than_p_percent(
      weights = c(0.5, 0.2, 0.1, 0.1, 0.05, 0.05),
      k = -1,
      p = 0.5
    )
  )
  
  expect_error(
    k_largest_weights_sum_to_less_than_p_percent(
      weights = c(0.5, 0.2, 0.1, 0.1, 0.05, 0.05),
      k = NA,
      p = 0.5
    )
  )
  
  expect_error(
    k_largest_weights_sum_to_less_than_p_percent(
      weights = c(0.5, 0.2, 0.1, 0.1, 0.05, 0.05),
      k = NULL,
      p = 0.5
    )
  )
})

test_that("fails when weights are not in [0,1]", {
  expect_error(
    k_largest_weights_sum_to_less_than_p_percent(
      weights = c(0.5, 0.2, 0.1, 0.1, 0.05, 2),
      k = 3,
      p = 0.5
    )
  )
  
  expect_error(
    k_largest_weights_sum_to_less_than_p_percent(
      weights = c(0.5, 0.2, 0.1, NA_real_, 0.05, 0.05),
      k = 3,
      p = 0.5
    )
  )
  
  expect_error(
    k_largest_weights_sum_to_less_than_p_percent(
      weights = NULL,
      k = 3,
      p = 0.5
    )
  )
})
