library(checkmate)

alphas <- list_edge_alphas()

test_that("returns a data frame of three columns with values in [0,1]", {
  expect_data_frame(
    x = alphas, types = "numeric", any.missing = FALSE, min.rows = 1, 
    ncols = 3
  )
  expect_names(
    x = names(alphas),
    identical.to = c("alpha", "alpha_seasonal", "alpha_seasonal_decay")
  )
  
  lapply(
    X = alphas,
    FUN = expect_numeric,
    lower = 0,
    upper = 1,
  )
})

test_that("returns `alphas` that are either 0 or 1", {
  expect_integerish(
    x = alphas$alpha, lower = 0, upper = 1, any.missing = FALSE
  )
  expect_integerish(
    x = alphas$alpha_seasonal, lower = 0, upper = 1, any.missing = FALSE
  )
  expect_integerish(
    x = alphas$alpha_seasonal_decay, lower = 0, upper = 1, any.missing = FALSE
  )
})
