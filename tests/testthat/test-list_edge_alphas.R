library(checkmate)

alphas <- list_edge_alphas()

test_that("returns a list of numeric vectors of length 3 with values in [0,1]", {
  expect_list(
    x = alphas, types = "numeric", any.missing = FALSE, min.len = 1, 
    unique = TRUE
  )
  
  lapply(
    X = alphas,
    FUN = expect_numeric,
    len = 3,
    lower = 0,
    upper = 1,
  )
})

test_that("returns `alphas` that are either 0 or 1", {
  expect_integerish(
    x = unlist(alphas), lower = 0, upper = 1, any.missing = FALSE
  )
})
