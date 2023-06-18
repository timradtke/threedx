#' Derive three-dimensional exponential (3DX) weights
#' 
#' @export
#' @examples 
#' # random walk forecast
#' weights_threedx(alpha = 1, alpha_seasonal = 0, alpha_seasonal_decay = 1, n = 30, season_length = 12)
#' # mean forecast
#' weights_threedx(alpha = 0, alpha_seasonal = 0, alpha_seasonal_decay = 0, n = 30, season_length = 12)
#' # seasonal mean forecast
#' weights_threedx(alpha = 0, alpha_seasonal = 1, alpha_seasonal_decay = 0, n = 30, season_length = 12)
#' # seasonal naive forecast
#' weights_threedx(alpha = 0, alpha_seasonal = 1, alpha_seasonal_decay = 1, n = 30, season_length = 12)
#' # last year's mean forecast
#' weights_threedx(alpha = 0, alpha_seasonal = 0, alpha_seasonal_decay = 1, n = 30, season_length = 12)
#' 
weights_threedx <- function(alpha,
                            alpha_seasonal,
                            alpha_seasonal_decay,
                            n,
                            season_length) {
  
  weights <- weights_exponential(alpha = alpha, n = n) *
    weights_seasonal(
      alpha_seasonal = alpha_seasonal,
      n = n,
      season_length = season_length
    ) *
    weights_seasonal_decay(
      alpha_seasonal_decay = alpha_seasonal_decay,
      n = n,
      season_length = season_length
    )
  
  weights <- weights / sum(weights)
  return(weights)
}
