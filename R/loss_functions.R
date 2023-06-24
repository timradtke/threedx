#' Observation-weighted mean-absolute error loss function
#' 
#' @param y_hat A numeric vector representing predictions
#' @param y A numeric vector representing observations
#' @param ... Additional arguments passed from other functions; ignored
#' 
#' @return A scalar value
#' 
#' @export
loss_mae_with_observation_weight <- function(y_hat, y, ...) {
  # wrapping in `sqrt()` to keep values from exploding
  sqrt(mean(
    abs(y - y_hat) * 
      pmax(y, abs(y - y_hat)) # using `pmax()` to avoid non-positive weights
  ))
}

#' Mean-absolute error loss function
#' 
#' @param y_hat A numeric vector representing predictions
#' @param y A numeric vector representing observations
#' @param ... Additional arguments passed from other functions; ignored
#' 
#' @return A scalar value
#' 
#' @export
loss_mae <- function(y_hat, y, ...) {
  mean(abs(y - y_hat))
}

#' Root mean-squared error loss function
#' 
#' @param y_hat A numeric vector representing predictions
#' @param y A numeric vector representing observations
#' @param ... Additional arguments passed from other functions; ignored
#' 
#' @return A scalar value
#' 
#' @export
loss_rmse <- function(y_hat, y, ...) {
  sqrt(mean((y - y_hat)^2))
}
