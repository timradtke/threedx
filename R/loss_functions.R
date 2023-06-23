#' Observation-weighted mean-absolute error loss function
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
#' @export
loss_mae <- function(y_hat, y, ...) {
  mean(abs(y - y_hat))
}

#' Root mean-squared error loss function
#' 
#' @export
loss_rmse <- function(y_hat, y, ...) {
  sqrt(mean((y - y_hat)^2))
}
