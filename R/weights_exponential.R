#' Derive exponential weights
#' 
#' A larger value of `alpha` will assign larger weights to more recent
#' observations.
#' 
#' @export
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
