#' Derive exponential weights
#' 
#' A larger value of `alpha` will assign larger weights to more recent
#' observations.
#' 
#' @param alpha A value between 0 and 1 that determines how quick the
#'   exponential decay in the weights is
#' @param n The number of weights to create; this is usually equal to the
#'   number of observations in a time series
#'
#' @return A monotonically increasing numeric vector of `n` values between 0
#'   and 1 that sum up to 1
#'   
#' @seealso [weights_seasonal()], [weights_seasonal_decay()],
#'   [weights_threedx()]
#' 
#' @export
#'   
#' @examples
#' weights_exponential(alpha = 0.25, n = 5)
#' weights_exponential(alpha = 1, n = 7)
#' weights_exponential(alpha = 0, n = 4)
#' 
#' # zooming into an exponential series again gives an exponential series
#' identical(
#'   weights_exponential(alpha = 0.75, n = 5)[1:4] /
#'     sum(weights_exponential(alpha = 0.75, n = 5)[1:4]),
#'   weights_exponential(alpha = 0.75, n = 4)
#' )
#' 
weights_exponential <- function(alpha, n) {
  checkmate::assert_numeric(
    x = alpha, lower = 0, upper = 1, len = 1, any.missing = FALSE
  )
  checkmate::assert_integerish(
    x = n, len = 1, lower = 1, any.missing = FALSE
  )
  
  if (alpha == 0) {
    return(rep(1/n, times = n))
  }
  
  weights <- (1 - alpha)^(rev(seq_len(n) - 1)) * alpha
  weights <- weights / sum(weights)
  return(weights)
}
