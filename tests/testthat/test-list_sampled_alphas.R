library(checkmate)

expect_list_of_numeric_length_three_in_zero_one <- function(alphas) {
  expect_list(
    x = alphas, types = "numeric", any.missing = FALSE, min.len = 1, 
    unique = TRUE
  )
  
  tmp <- lapply(
    X = alphas,
    FUN = expect_numeric,
    len = 3,
    lower = 0,
    upper = 1,
  )
  
  return(invisible(TRUE))
}

expect_list_in_bounds <- function(n_target,
                                  alpha_lower,
                                  alpha_upper,
                                  alpha_seasonal_lower,
                                  alpha_seasonal_upper,
                                  alpha_seasonal_decay_lower,
                                  alpha_seasonal_decay_upper,
                                  oversample_lower,
                                  oversample_upper) {
  
  alphas <- list_sampled_alphas(
    n_target = n_target,
    alpha_lower = alpha_lower,
    alpha_upper = alpha_upper,
    alpha_seasonal_lower = alpha_seasonal_lower,
    alpha_seasonal_upper = alpha_seasonal_upper,
    alpha_seasonal_decay_lower = alpha_seasonal_decay_lower,
    alpha_seasonal_decay_upper = alpha_seasonal_decay_upper,
    oversample_lower = oversample_lower,
    oversample_upper = oversample_upper,
    include_edge_cases = FALSE,
    seed = NULL
  )
  
  expect_list(
    x = alphas, types = "numeric", any.missing = FALSE, min.len = 1, 
    unique = TRUE, max.len = n_target
  )
  
  alphas_vector <- unlist(alphas)
  expect_numeric(
    x = alphas_vector[(seq_along(alphas_vector) + 2) %% 3 == 0],
    lower = alpha_lower,
    upper = alpha_upper
  )
  expect_numeric(
    x = alphas_vector[(seq_along(alphas_vector) + 1) %% 3 == 0],
    lower = alpha_seasonal_lower,
    upper = alpha_seasonal_upper
  )
  expect_numeric(
    x = alphas_vector[seq_along(alphas_vector) %% 3 == 0],
    lower = alpha_seasonal_decay_lower,
    upper = alpha_seasonal_decay_upper
  )
}

test_that("returns a list of <= `n_target` numeric vectors of length 3 in [0,1]", {
  expect_list_of_numeric_length_three_in_zero_one(
    list_sampled_alphas(
      n_target = 100L,
      alpha_lower = 0,
      alpha_upper = 1,
      alpha_seasonal_lower = 0,
      alpha_seasonal_upper = 1,
      alpha_seasonal_decay_lower = 0,
      alpha_seasonal_decay_upper = 1,
      oversample_lower = 0.05,
      oversample_upper = 0.05,
      include_edge_cases = TRUE,
      seed = NULL
    )
  )
  
  expect_list_of_numeric_length_three_in_zero_one(
    list_sampled_alphas(
      n_target = 100L,
      alpha_lower = 0,
      alpha_upper = 1,
      alpha_seasonal_lower = 0,
      alpha_seasonal_upper = 1,
      alpha_seasonal_decay_lower = 0,
      alpha_seasonal_decay_upper = 1,
      oversample_lower = 0.05,
      oversample_upper = 0.05,
      include_edge_cases = FALSE,
      seed = NULL
    )
  )
  
  expect_list_of_numeric_length_three_in_zero_one(
    list_sampled_alphas(
      n_target = 1L,
      alpha_lower = 0,
      alpha_upper = 1,
      alpha_seasonal_lower = 0,
      alpha_seasonal_upper = 1,
      alpha_seasonal_decay_lower = 0,
      alpha_seasonal_decay_upper = 1,
      oversample_lower = 0.05,
      oversample_upper = 0.05,
      include_edge_cases = TRUE,
      seed = NULL
    )
  )
  
  expect_list_of_numeric_length_three_in_zero_one(
    list_sampled_alphas(
      n_target = 1L,
      alpha_lower = 0,
      alpha_upper = 1,
      alpha_seasonal_lower = 0,
      alpha_seasonal_upper = 1,
      alpha_seasonal_decay_lower = 0,
      alpha_seasonal_decay_upper = 1,
      oversample_lower = 0.05,
      oversample_upper = 0.05,
      include_edge_cases = FALSE,
      seed = NULL
    )
  )
})

test_that("respects bounds on the three different alphas when not returning edge alphas", {
  expect_list_in_bounds(
    n_target = 100L,
    alpha_lower = 0,
    alpha_upper = 1,
    alpha_seasonal_lower = 0,
    alpha_seasonal_upper = 1,
    alpha_seasonal_decay_lower = 0,
    alpha_seasonal_decay_upper = 1,
    oversample_lower = 0.05,
    oversample_upper = 0.05
  )
  
  expect_list_in_bounds(
    n_target = 100L,
    alpha_lower = 0.25,
    alpha_upper = 0.75,
    alpha_seasonal_lower = 0.2,
    alpha_seasonal_upper = 0.5,
    alpha_seasonal_decay_lower = 0.8,
    alpha_seasonal_decay_upper = 0.95,
    oversample_lower = 0.05,
    oversample_upper = 0.05
  )
  
  expect_list_in_bounds(
    n_target = 1000L,
    alpha_lower = 0.25,
    alpha_upper = 0.75,
    alpha_seasonal_lower = 0.2,
    alpha_seasonal_upper = 0.5,
    alpha_seasonal_decay_lower = 0.8,
    alpha_seasonal_decay_upper = 0.95,
    oversample_lower = 1,
    oversample_upper = 1
  )
})

test_that("fails when `n_target` is not a positive integer", {
  expect_error(
    list_sampled_alphas(
      n_target = 0,
      alpha_lower = 0,
      alpha_upper = 1,
      alpha_seasonal_lower = 0,
      alpha_seasonal_upper = 1,
      alpha_seasonal_decay_lower = 0,
      alpha_seasonal_decay_upper = 1,
      oversample_lower = 0.05,
      oversample_upper = 0.05,
      include_edge_cases = FALSE,
      seed = NULL
    )
  )
  
  expect_error(
    list_sampled_alphas(
      n_target = 1057.24,
      alpha_lower = 0,
      alpha_upper = 1,
      alpha_seasonal_lower = 0,
      alpha_seasonal_upper = 1,
      alpha_seasonal_decay_lower = 0,
      alpha_seasonal_decay_upper = 1,
      oversample_lower = 0.05,
      oversample_upper = 0.05,
      include_edge_cases = FALSE,
      seed = NULL
    )
  )
  
  expect_error(
    list_sampled_alphas(
      n_target = -1,
      alpha_lower = 0,
      alpha_upper = 1,
      alpha_seasonal_lower = 0,
      alpha_seasonal_upper = 1,
      alpha_seasonal_decay_lower = 0,
      alpha_seasonal_decay_upper = 1,
      oversample_lower = 0.05,
      oversample_upper = 0.05,
      include_edge_cases = FALSE,
      seed = NULL
    )
  )
  
  expect_error(
    list_sampled_alphas(
      n_target = NA_real_,
      alpha_lower = 0,
      alpha_upper = 1,
      alpha_seasonal_lower = 0,
      alpha_seasonal_upper = 1,
      alpha_seasonal_decay_lower = 0,
      alpha_seasonal_decay_upper = 1,
      oversample_lower = 0.05,
      oversample_upper = 0.05,
      include_edge_cases = FALSE,
      seed = NULL
    )
  )
})

test_that("fails when `alpha_lower` is not in [0,1]", {
  expect_error(
    list_sampled_alphas(
      n_target = 264,
      alpha_lower = -0.5,
      alpha_upper = 1,
      alpha_seasonal_lower = 0,
      alpha_seasonal_upper = 1,
      alpha_seasonal_decay_lower = 0,
      alpha_seasonal_decay_upper = 1,
      oversample_lower = 0.05,
      oversample_upper = 0.05,
      include_edge_cases = FALSE,
      seed = NULL
    )
  )
  
  expect_error(
    list_sampled_alphas(
      n_target = 264,
      alpha_lower = 748,
      alpha_upper = 1,
      alpha_seasonal_lower = 0,
      alpha_seasonal_upper = 1,
      alpha_seasonal_decay_lower = 0,
      alpha_seasonal_decay_upper = 1,
      oversample_lower = 0.05,
      oversample_upper = 0.05,
      include_edge_cases = FALSE,
      seed = NULL
    )
  )
  
  expect_error(
    list_sampled_alphas(
      n_target = 78,
      alpha_lower = NA_real_,
      alpha_upper = 1,
      alpha_seasonal_lower = 0,
      alpha_seasonal_upper = 1,
      alpha_seasonal_decay_lower = 0,
      alpha_seasonal_decay_upper = 1,
      oversample_lower = 0.05,
      oversample_upper = 0.05,
      include_edge_cases = FALSE,
      seed = NULL
    )
  )
})

test_that("fails when `alpha_upper` is not in [0,1]", {
  expect_error(
    list_sampled_alphas(
      n_target = 264,
      alpha_lower = 0,
      alpha_upper = -0.5,
      alpha_seasonal_lower = 0,
      alpha_seasonal_upper = 1,
      alpha_seasonal_decay_lower = 0,
      alpha_seasonal_decay_upper = 1,
      oversample_lower = 0.05,
      oversample_upper = 0.05,
      include_edge_cases = FALSE,
      seed = NULL
    )
  )
  
  expect_error(
    list_sampled_alphas(
      n_target = 264,
      alpha_lower = 0,
      alpha_upper = 472,
      alpha_seasonal_lower = 0,
      alpha_seasonal_upper = 1,
      alpha_seasonal_decay_lower = 0,
      alpha_seasonal_decay_upper = 1,
      oversample_lower = 0.05,
      oversample_upper = 0.05,
      include_edge_cases = FALSE,
      seed = NULL
    )
  )
  
  expect_error(
    list_sampled_alphas(
      n_target = 78,
      alpha_lower = 0,
      alpha_upper = NA_real_,
      alpha_seasonal_lower = 0,
      alpha_seasonal_upper = 1,
      alpha_seasonal_decay_lower = 0,
      alpha_seasonal_decay_upper = 1,
      oversample_lower = 0.05,
      oversample_upper = 0.05,
      include_edge_cases = FALSE,
      seed = NULL
    )
  )
})

test_that("fails when `alpha_upper` less than `alpha_lower`", {
  expect_error(
    list_sampled_alphas(
      n_target = 264,
      alpha_lower = 0.25,
      alpha_upper = 0.1,
      alpha_seasonal_lower = 0,
      alpha_seasonal_upper = 1,
      alpha_seasonal_decay_lower = 0,
      alpha_seasonal_decay_upper = 1,
      oversample_lower = 0.05,
      oversample_upper = 0.05,
      include_edge_cases = FALSE,
      seed = NULL
    )
  )
})

test_that("fails when `alpha_seasonal_lower` is not in [0,1]", {
  expect_error(
    list_sampled_alphas(
      n_target = 264,
      alpha_lower = 0,
      alpha_upper = 1,
      alpha_seasonal_lower = -0.0001,
      alpha_seasonal_upper = 1,
      alpha_seasonal_decay_lower = 0,
      alpha_seasonal_decay_upper = 1,
      oversample_lower = 0.05,
      oversample_upper = 0.05,
      include_edge_cases = FALSE,
      seed = NULL
    )
  )
  
  expect_error(
    list_sampled_alphas(
      n_target = 264,
      alpha_lower = 0,
      alpha_upper = 1,
      alpha_seasonal_lower = 472,
      alpha_seasonal_upper = 1,
      alpha_seasonal_decay_lower = 0,
      alpha_seasonal_decay_upper = 1,
      oversample_lower = 0.05,
      oversample_upper = 0.05,
      include_edge_cases = FALSE,
      seed = NULL
    )
  )
  
  expect_error(
    list_sampled_alphas(
      n_target = 78,
      alpha_lower = 0,
      alpha_upper = 1,
      alpha_seasonal_lower = NA,
      alpha_seasonal_upper = 1,
      alpha_seasonal_decay_lower = 0,
      alpha_seasonal_decay_upper = 1,
      oversample_lower = 0.05,
      oversample_upper = 0.05,
      include_edge_cases = FALSE,
      seed = NULL
    )
  )
})

test_that("fails when `alpha_seasonal_upper` is not in [0,1]", {
  expect_error(
    list_sampled_alphas(
      n_target = 264,
      alpha_lower = 0,
      alpha_upper = 1,
      alpha_seasonal_lower = 0,
      alpha_seasonal_upper = -0.48,
      alpha_seasonal_decay_lower = 0,
      alpha_seasonal_decay_upper = 1,
      oversample_lower = 0.05,
      oversample_upper = 0.05,
      include_edge_cases = FALSE,
      seed = NULL
    )
  )
  
  expect_error(
    list_sampled_alphas(
      n_target = 264,
      alpha_lower = 0,
      alpha_upper = 1,
      alpha_seasonal_lower = 0,
      alpha_seasonal_upper = 9,
      alpha_seasonal_decay_lower = 0,
      alpha_seasonal_decay_upper = 1,
      oversample_lower = 0.05,
      oversample_upper = 0.05,
      include_edge_cases = FALSE,
      seed = NULL
    )
  )
  
  expect_error(
    list_sampled_alphas(
      n_target = 78,
      alpha_lower = 0,
      alpha_upper = 1,
      alpha_seasonal_lower = 0,
      alpha_seasonal_upper = NA_real_,
      alpha_seasonal_decay_lower = 0,
      alpha_seasonal_decay_upper = 1,
      oversample_lower = 0.05,
      oversample_upper = 0.05,
      include_edge_cases = FALSE,
      seed = NULL
    )
  )
})

test_that("fails when `alpha_seasonal_upper` less than `alpha_lower`", {
  expect_error(
    list_sampled_alphas(
      n_target = 264,
      alpha_lower = 0,
      alpha_upper = 1,
      alpha_seasonal_lower = 0.6,
      alpha_seasonal_upper = 0.32,
      alpha_seasonal_decay_lower = 0,
      alpha_seasonal_decay_upper = 1,
      oversample_lower = 0.05,
      oversample_upper = 0.05,
      include_edge_cases = FALSE,
      seed = NULL
    )
  )
})

test_that("fails when `alpha_seasonal_decay_lower` is not in [0,1]", {
  expect_error(
    list_sampled_alphas(
      n_target = 264,
      alpha_lower = 0,
      alpha_upper = 1,
      alpha_seasonal_lower = 0,
      alpha_seasonal_upper = 1,
      alpha_seasonal_decay_lower = -0.0001,
      alpha_seasonal_decay_upper = 1,
      oversample_lower = 0.05,
      oversample_upper = 0.05,
      include_edge_cases = FALSE,
      seed = NULL
    )
  )
  
  expect_error(
    list_sampled_alphas(
      n_target = 264,
      alpha_lower = 0,
      alpha_upper = 1,
      alpha_seasonal_lower = 0,
      alpha_seasonal_upper = 1,
      alpha_seasonal_decay_lower = 472,
      alpha_seasonal_decay_upper = 1,
      oversample_lower = 0.05,
      oversample_upper = 0.05,
      include_edge_cases = FALSE,
      seed = NULL
    )
  )
  
  expect_error(
    list_sampled_alphas(
      n_target = 78,
      alpha_lower = 0,
      alpha_upper = 1,
      alpha_seasonal_lower = 0,
      alpha_seasonal_upper = 1,
      alpha_seasonal_decay_lower = NA_real_,
      alpha_seasonal_decay_upper = 1,
      oversample_lower = 0.05,
      oversample_upper = 0.05,
      include_edge_cases = FALSE,
      seed = NULL
    )
  )
})

test_that("fails when `alpha_upper` is not in [0,1]", {
  expect_error(
    list_sampled_alphas(
      n_target = 264,
      alpha_lower = 0,
      alpha_upper = 1,
      alpha_seasonal_lower = 0,
      alpha_seasonal_upper = 1,
      alpha_seasonal_decay_lower = 0,
      alpha_seasonal_decay_upper = -0.48,
      oversample_lower = 0.05,
      oversample_upper = 0.05,
      include_edge_cases = FALSE,
      seed = NULL
    )
  )
  
  expect_error(
    list_sampled_alphas(
      n_target = 264,
      alpha_lower = 0,
      alpha_upper = 1,
      alpha_seasonal_lower = 0,
      alpha_seasonal_upper = 1,
      alpha_seasonal_decay_lower = 0,
      alpha_seasonal_decay_upper = 9,
      oversample_lower = 0.05,
      oversample_upper = 0.05,
      include_edge_cases = FALSE,
      seed = NULL
    )
  )
  
  expect_error(
    list_sampled_alphas(
      n_target = 78,
      alpha_lower = 0,
      alpha_upper = 1,
      alpha_seasonal_lower = 0,
      alpha_seasonal_upper = 1,
      alpha_seasonal_decay_lower = 0,
      alpha_seasonal_decay_upper = NA_real_,
      oversample_lower = 0.05,
      oversample_upper = 0.05,
      include_edge_cases = FALSE,
      seed = NULL
    )
  )
})

test_that("fails when `alpha_upper` less than `alpha_lower`", {
  expect_error(
    list_sampled_alphas(
      n_target = 264,
      alpha_lower = 0,
      alpha_upper = 1,
      alpha_seasonal_lower = 0,
      alpha_seasonal_upper = 1,
      alpha_seasonal_decay_lower = 0.95,
      alpha_seasonal_decay_upper = 0.94,
      oversample_lower = 0.05,
      oversample_upper = 0.05,
      include_edge_cases = FALSE,
      seed = NULL
    )
  )
})

test_that("returned `alpha` is deterministic when lower bounds are equal to upper bounds", {
  alphas <- list_sampled_alphas(
    n_target = 1L,
    alpha_lower = 0.25,
    alpha_upper = 0.25,
    alpha_seasonal_lower = 0.81,
    alpha_seasonal_upper = 0.81,
    alpha_seasonal_decay_lower = 0.1,
    alpha_seasonal_decay_upper = 0.1,
    oversample_lower = 0.05,
    oversample_upper = 0.05,
    include_edge_cases = FALSE,
    seed = NULL
  )
  
  expect_equal(alphas, list(c(0.25, 0.81, 0.1)))
})

test_that("fails when `oversample_lower` less than not positive numeric", {
  expect_error(
    list_sampled_alphas(
      n_target = 264,
      alpha_lower = 0,
      alpha_upper = 1,
      alpha_seasonal_lower = 0,
      alpha_seasonal_upper = 1,
      alpha_seasonal_decay_lower = 0,
      alpha_seasonal_decay_upper = 1,
      oversample_lower = -0.5,
      oversample_upper = 0.05,
      include_edge_cases = FALSE,
      seed = NULL
    )
  )
  
  expect_error(
    list_sampled_alphas(
      n_target = 264,
      alpha_lower = 0,
      alpha_upper = 1,
      alpha_seasonal_lower = 0,
      alpha_seasonal_upper = 1,
      alpha_seasonal_decay_lower = 0,
      alpha_seasonal_decay_upper = 1,
      oversample_lower = NA_real_,
      oversample_upper = 0.05,
      include_edge_cases = FALSE,
      seed = NULL
    )
  )
  
  expect_error(
    list_sampled_alphas(
      n_target = 264,
      alpha_lower = 0,
      alpha_upper = 1,
      alpha_seasonal_lower = 0,
      alpha_seasonal_upper = 1,
      alpha_seasonal_decay_lower = 0,
      alpha_seasonal_decay_upper = 1,
      oversample_lower = c(0, 1),
      oversample_upper = 0.05,
      include_edge_cases = FALSE,
      seed = NULL
    )
  )
})

test_that("fails when `oversample_upper` less than not positive numeric", {
  expect_error(
    list_sampled_alphas(
      n_target = 264,
      alpha_lower = 0,
      alpha_upper = 1,
      alpha_seasonal_lower = 0,
      alpha_seasonal_upper = 1,
      alpha_seasonal_decay_lower = 0,
      alpha_seasonal_decay_upper = 1,
      oversample_lower = 0.05,
      oversample_upper = -4,
      include_edge_cases = FALSE,
      seed = NULL
    )
  )
  
  expect_error(
    list_sampled_alphas(
      n_target = 264,
      alpha_lower = 0,
      alpha_upper = 1,
      alpha_seasonal_lower = 0,
      alpha_seasonal_upper = 1,
      alpha_seasonal_decay_lower = 0,
      alpha_seasonal_decay_upper = 1,
      oversample_lower = 0.05,
      oversample_upper = NA_real_,
      include_edge_cases = FALSE,
      seed = NULL
    )
  )
  
  expect_error(
    list_sampled_alphas(
      n_target = 264,
      alpha_lower = 0,
      alpha_upper = 1,
      alpha_seasonal_lower = 0,
      alpha_seasonal_upper = 1,
      alpha_seasonal_decay_lower = 0,
      alpha_seasonal_decay_upper = 1,
      oversample_lower = 0.05,
      oversample_upper = c(0, 1),
      include_edge_cases = FALSE,
      seed = NULL
    )
  )
})

test_that("The `.Random.seed` is reset to what it was before function call", {
  seed_before <- .Random.seed
  
  tmp <- list_sampled_alphas(
    n_target = 100L,
    alpha_lower = 0,
    alpha_upper = 1,
    alpha_seasonal_lower = 0,
    alpha_seasonal_upper = 1,
    alpha_seasonal_decay_lower = 0,
    alpha_seasonal_decay_upper = 1,
    oversample_lower = 0.05,
    oversample_upper = 0.05,
    include_edge_cases = FALSE,
    seed = 7582
  )
  
  expect_identical(.Random.seed, seed_before)
})

test_that("returns same object when `seed` is provided", {
  first <- list_sampled_alphas(
    n_target = 100L,
    alpha_lower = 0,
    alpha_upper = 1,
    alpha_seasonal_lower = 0,
    alpha_seasonal_upper = 1,
    alpha_seasonal_decay_lower = 0,
    alpha_seasonal_decay_upper = 1,
    oversample_lower = 0.05,
    oversample_upper = 0.05,
    include_edge_cases = FALSE,
    seed = 934
  )
  
  second <- list_sampled_alphas(
    n_target = 100L,
    alpha_lower = 0,
    alpha_upper = 1,
    alpha_seasonal_lower = 0,
    alpha_seasonal_upper = 1,
    alpha_seasonal_decay_lower = 0,
    alpha_seasonal_decay_upper = 1,
    oversample_lower = 0.05,
    oversample_upper = 0.05,
    include_edge_cases = FALSE,
    seed = 934
  )
  
  expect_identical(first, second)
})

test_that("returns different objects when seed is not provided and params allow for variation", {
  first <- list_sampled_alphas(
    n_target = 100L,
    alpha_lower = 0,
    alpha_upper = 1,
    alpha_seasonal_lower = 0,
    alpha_seasonal_upper = 1,
    alpha_seasonal_decay_lower = 0,
    alpha_seasonal_decay_upper = 1,
    oversample_lower = 0.05,
    oversample_upper = 0.05,
    include_edge_cases = FALSE,
    seed = NULL
  )
  
  second <- list_sampled_alphas(
    n_target = 100L,
    alpha_lower = 0,
    alpha_upper = 1,
    alpha_seasonal_lower = 0,
    alpha_seasonal_upper = 1,
    alpha_seasonal_decay_lower = 0,
    alpha_seasonal_decay_upper = 1,
    oversample_lower = 0.05,
    oversample_upper = 0.05,
    include_edge_cases = FALSE,
    seed = NULL
  )
  
  expect_true(!identical(first, second))
})
