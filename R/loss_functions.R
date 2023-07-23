#' Observation-weighted mean absolute error loss function
#' 
#' @param y_hat A numeric vector representing predictions
#' @param y A numeric vector representing observations
#' @param ... Additional arguments passed from other functions; ignored
#' 
#' @return A scalar value
#' 
#' @references Suman Ravuri et al. (2021).
#' *Skilful precipitation nowcasting using deep generative models of radar*.
#' <https://www.nature.com/articles/s41586-021-03854-z>
#' 
#' @export
loss_mae_with_observation_weight <- function(y_hat, y, ...) {
  # wrapping in `sqrt()` to keep values from exploding
  sqrt(mean(
    abs(y - y_hat) * 
      pmax(y, abs(y - y_hat)) # using `pmax()` to avoid non-positive weights
  ))
}

#' Mean absolute error loss function
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

#' Mean absolute error loss function ignoring bias
#' 
#' Subtracts the median from the observed residuals before computing their
#' mean absolute error. This can be helpful when predicting time series
#' where a bias due to a trend component is expected and can be captured
#' by the innovation function choice.
#' 
#' @seealso [draw_normal_with_drift()], [draw_bootstrap()],
#'   [loss_rmse_ignoring_bias()]
#' 
#' @param y_hat A numeric vector representing predictions
#' @param y A numeric vector representing observations
#' @param ... Additional arguments passed from other functions; ignored
#' 
#' @return A scalar value
#' 
#' @export
loss_mae_ignoring_bias <- function(y_hat, y, ...) {
  residuals <- y - y_hat
  residuals_debiased <- residuals - stats::median(residuals)
  mean(abs(residuals_debiased))
}

#' Root-mean-square error loss function
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

#' Root-mean-square error loss function ignoring bias
#' 
#' Subtracts the mean from the observed residuals before computing their
#' root-mean-square error. This can be helpful when predicting time series
#' where a bias due to a trend component is expected and can be captured
#' by the innovation function choice.
#' 
#' @seealso [draw_normal_with_drift()], [draw_bootstrap()],
#'   [loss_mae_ignoring_bias()]
#' 
#' @param y_hat A numeric vector representing predictions
#' @param y A numeric vector representing observations
#' @param ... Additional arguments passed from other functions; ignored
#' 
#' @return A scalar value
#' 
#' @export
loss_rmse_ignoring_bias <- function(y_hat, y, ...) {
  residuals <- y - y_hat
  residuals_debiased <- residuals - mean(residuals)
  sqrt(mean((residuals_debiased)^2))
}
