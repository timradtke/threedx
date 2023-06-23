library(checkmate)

test_that("returns data frame with three numeric columns", {
  alphas_list <- list(
    stats::runif(n = 3, min = 0, max = 1),
    stats::runif(n = 3, min = 0, max = 1),
    stats::runif(n = 3, min = 0, max = 1),
    stats::runif(n = 3, min = 0, max = 1),
    stats::runif(n = 3, min = 0, max = 1),
    c(0.5, NA_real_, 1),
    c(NA_real_, NA_real_, NA_real_)
  )
  alphas_df <- alphas_list_to_data_frame(alphas_list)
  
  expect_data_frame(
    x = alphas_df,
    types = "numeric",
    any.missing = TRUE,
    nrows = length(alphas_list),
    ncols = 3
  )
  expect_names(
    x = names(alphas_df),
    identical.to = c("alpha", "alpha_seasonal", "alpha_seasonal_decay")
  )
  expect_equal(sum(alphas_df), sum(unlist(alphas_list)))
})

test_that("fails when a list entry is not of length 3 (has more alphas than expected)", {
  expect_error(
    alphas_list_to_data_frame(
      alphas_grid = list(
        stats::runif(n = 3, min = 0, max = 1),
        stats::runif(n = 4, min = 0, max = 1)
      )
    )
  )
})

test_that("fails when `alphas_grid` is not a list of numerics", {
  expect_error(
    alphas_list_to_data_frame(alphas_grid = list(c("hello", "world", "!")))
  )
  
  expect_error(
    alphas_list_to_data_frame(alphas_grid = list())
  )
  
  expect_error(
    alphas_list_to_data_frame(alphas_grid = runif(3))
  )
  
  expect_error(
    alphas_list_to_data_frame(alphas_grid = NULL)
  )
  
  expect_error(
    alphas_list_to_data_frame(alphas_grid = c(NA, NA, NA))
  )
})
