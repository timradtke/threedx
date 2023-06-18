#' Observation-weighted mean-absolute error loss function
#' 
#' @export
loss_mae_with_observation_weight <- function(y_hat, y, ...) {
  mean(abs(y - y_hat) * pmax(y, abs(y - y_hat)), na.rm = TRUE)
}

#' Mean-absolute error loss function
#' 
#' @export
loss_mae <- function(y_hat, y, ...) {
  mean(abs(y - y_hat))
}

#' Mean-squared error loss function
#' 
#' @export
loss_mse <- function(y_hat, y, ...) {
  mean((y - y_hat)^2)
}
