#' Derive three-dimensional exponential (3DX) weights
#' 
#' Assigns weights that decay in three different directions as determined by
#' the combination of [weights_exponential()], [weights_seasonal()], and
#' [weights_seasonal_decay()].
#' 
#' @param alpha The smoothing factor passed to [weights_exponential()] that
#'   determines the overall monotonic exponential weight component
#' @param alpha_seasonal The smoothing factor passed to [weights_seasonal()]
#'   that determines the exponential weighting within a seasonal period
#' @param alpha_seasonal_decay The smoothing factor passed to
#'   [weights_seasonal_decay()] that determines the exponential weighting of
#'   the seasonal periods
#' @param n The number of weights to create; this is usually equal to the
#'   number of observations in a time series
#' @param period_length The length of the seasonal period to be modeled
#' 
#' @return A numeric vector of `n` values between 0 and 1 that sum up to 1
#' 
#' @seealso [weights_seasonal_decay()], [weights_exponential()],
#'   [weights_threedx()]
#' 
#' @export
#' @examples
#' weights <- weights_threedx(
#'   alpha = 0.01,
#'   alpha_seasonal = 0.05,
#'   alpha_seasonal_decay = 0.01,
#'   n = 17,
#'   period_length = 5
#' )
#' 
#' print(weights)
#' 
#' if (require("ggplot2")) {
#'   ggplot2::ggplot(
#'     data.frame(index = seq_along(weights), weight = weights),
#'     ggplot2::aes(x = index, y = weight)
#'   ) +
#'     ggplot2::geom_col()
#' }
#' 
#' # random walk forecast
#' weights_threedx(
#'   alpha = 1,
#'   alpha_seasonal = 0,
#'   alpha_seasonal_decay = 1,
#'   n = 30,
#'   period_length = 12
#' )
#' 
#' # mean forecast
#' weights_threedx(
#'   alpha = 0,
#'   alpha_seasonal = 0,
#'   alpha_seasonal_decay = 0,
#'   n = 30,
#'   period_length = 12
#' )
#' 
#' # seasonal mean forecast
#' weights_threedx(
#'   alpha = 0,
#'   alpha_seasonal = 1,
#'   alpha_seasonal_decay = 0,
#'   n = 30,
#'   period_length = 12
#' )
#' 
#' # seasonal naive forecast
#' weights_threedx(
#'   alpha = 0,
#'   alpha_seasonal = 1,
#'   alpha_seasonal_decay = 1,
#'   n = 30,
#'   period_length = 12
#' )
#' 
#' # last year's mean forecast
#' weights_threedx(
#'   alpha = 0,
#'   alpha_seasonal = 0,
#'   alpha_seasonal_decay = 1,
#'   n = 30,
#'   period_length = 12
#' )
#' 
weights_threedx <- function(alpha,
                            alpha_seasonal,
                            alpha_seasonal_decay,
                            n,
                            period_length) {
  
  weights <- weights_exponential(alpha = alpha, n = n) *
    weights_seasonal(
      alpha_seasonal = alpha_seasonal,
      n = n,
      period_length = period_length
    ) *
    weights_seasonal_decay(
      alpha_seasonal_decay = alpha_seasonal_decay,
      n = n,
      period_length = period_length
    )
  
  weights <- weights / sum(weights)
  return(weights)
}

#' Vectorized `weights_threedx()`
#' 
#' @keywords internal
#' @examples
#' threedx:::weights_threedx_vec(
#'   alphas = c(0, 0.2, 0.5, 0.8, 1),
#'   alphas_seasonal = c(0.1, 0.3, 0.55, 1, 0.25),
#'   alphas_seasonal_decay = c(0.11, 0.01, 0.02, 0.05, 0.1),
#'   n = 50,
#'   period_length = 7
#' )
weights_threedx_vec <- function(alphas,
                                alphas_seasonal,
                                alphas_seasonal_decay,
                                n,
                                period_length) {
  
  weights <- weights_exponential_vec(alphas = alphas, n = n) *
    weights_seasonal_vec(
      alphas_seasonal = alphas_seasonal,
      n = n,
      period_length = period_length
    ) *
    weights_seasonal_decay_vec(
      alphas_seasonal_decay = alphas_seasonal_decay,
      n = n,
      period_length = period_length
    )
  
  weights <- weights / rowSums(weights)
  return(weights)
}
