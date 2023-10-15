
test_that("simplicity dominates loss within `increase` range", {
  alphas_grid <- data.frame(
    alpha = c(0.5, 0.5, 0.5, 0.5, 0),
    alpha_seasonal = c(0.5, 0.5, 0, 0, 0),
    alpha_seasonal_decay = c(0.5, 0, 0, 0, 0)
  )
  
  losses <- c(100, 101, 103, 102, 110)
  
  expect_equal(
    trade_loss_for_simplicity(
      alphas_grid = alphas_grid,
      losses = losses,
      increase = 5
    ),
    # 1 has best loss but higher complexity than other options
    # picks 4 over 2 even though 4's loss is higher
    # picks 4 over 3 because 4's loss is lower at same complexity
    # 5 is ignored despite lowest complexity due to loss outside range
    4
  )
})

test_that("returns 1 when all options are equal", {
  alphas_grid <- data.frame(
    alpha = rep(0, 10),
    alpha_seasonal = rep(0, 10),
    alpha_seasonal_decay = rep(0, 10)
  )
  
  losses <- rep(1, 10)
  
  expect_equal(
    trade_loss_for_simplicity(
      alphas_grid = alphas_grid,
      losses = losses,
      increase = 1
    ),
    1
  )
})

test_that("considers only options with zero loss if best loss is zero", {
  alphas_grid <- data.frame(
    alpha = c(0.5, rep(0, 2)),
    alpha_seasonal = rep(0, 3),
    alpha_seasonal_decay = rep(0, 3)
  )

  losses <- c(0, 1, 0)

  expect_equal(
    trade_loss_for_simplicity(
      alphas_grid = alphas_grid,
      losses = losses,
      increase = 1
    ),
    3 # same model complexity as 2, but 2 doesn't have zero loss
  )
})

test_that("returns 1 when there is only a single model option", {
  alphas_grid <- data.frame(
    alpha = 0.5,
    alpha_seasonal = 0,
    alpha_seasonal_decay = 1
  )
  
  expect_equal(
    trade_loss_for_simplicity(
      alphas_grid = alphas_grid,
      losses = 100,
      increase = 1
    ),
    1
  )
})

test_that("returns globally best model when `increase` is zero", {
  alphas_grid <- data.frame(
    alpha = c(0.5, 0.5, 0.5, 0.5, 0),
    alpha_seasonal = c(0.5, 0.5, 0, 0, 0),
    alpha_seasonal_decay = c(0.5, 0, 0, 0, 0)
  )
  
  losses <- c(100, 101, 103, 102, 110)
  
  expect_equal(
    trade_loss_for_simplicity(
      alphas_grid = alphas_grid,
      losses = losses,
      increase = 0
    ),
    1
  )
})

test_that("if multiple models have best global loss, pick the first with least complexity", {
  alphas_grid <- data.frame(
    alpha = c(0.5, 0.5, 0.5, 0, 0),
    alpha_seasonal = c(0.5, 0.5, 0, 0.5, 0),
    alpha_seasonal_decay = c(0.5, 0, 0, 0, 0)
  )
  
  losses <- c(100, 100, 100, 100, 110)
  
  expect_equal(
    trade_loss_for_simplicity(
      alphas_grid = alphas_grid,
      losses = losses,
      increase = 0
    ),
    # picks 2 over 1 becasuse of lower complexity
    # picks 3 over 2 because of lower complexity
    # picks 3 over 4 because 3 comes first at same complexity
    # 5 is ignored because it doesn't have the global minimum loss
    3
  )
})

test_that("fails if `increase` is not non-negative scalar", {
  alphas_grid <- data.frame(
    alpha = c(0.5, 0.5, 0.5, 0.5, 0),
    alpha_seasonal = c(0.5, 0.5, 0, 0, 0),
    alpha_seasonal_decay = c(0.5, 0, 0, 0, 0)
  )
  
  losses <- c(100, 101, 103, 102, 110)
  
  expect_error(
    trade_loss_for_simplicity(
      alphas_grid = alphas_grid,
      losses = losses,
      increase = -1
    )
  )
  
  expect_error(
    trade_loss_for_simplicity(
      alphas_grid = alphas_grid,
      losses = losses,
      increase = c(1, 1)
    )
  )
  
  expect_error(
    trade_loss_for_simplicity(
      alphas_grid = alphas_grid,
      losses = losses,
      increase = NA_real_
    )
  )
  
  expect_error(
    trade_loss_for_simplicity(
      alphas_grid = alphas_grid,
      losses = losses,
      increase = NULL
    )
  )
})
