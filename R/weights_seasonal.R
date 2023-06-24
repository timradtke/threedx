#' Derive seasonal exponential weights
#' 
#' Assigns the largest weight to the same season of the period with length
#' `period_length`, and exponentially decaying weights to the neighboring
#' seasons. The most recent value in the resulting weights vector will be
#' assigned the second-highest weight as it is the direct neighbor of the
#' current season. See examples for illustrations of this.
#' 
#' @param alpha_seasonal A value between 0 and 1 that determines how quick the
#'   exponential decay in the weights is
#' @param n The number of weights to create; this is usually equal to the
#'   number of observations in a time series
#' @param period_length The length of the seasonal period to be modeled
#' 
#' @return A numeric vector of `n` values between 0 and 1 that sum up to 1
#' 
#' @seealso [weights_seasonal_decay()], [weights_exponential()],
#'   [weights_threedx()]
#' 
#' @examples
#' round(
#'   weights_seasonal(alpha_seasonal = 0.25, n = 8, period_length = 7L),
#'   3
#' )
#' 
#' round(
#'   weights_seasonal(alpha_seasonal = 0.75, n = 27, period_length = 12L),
#'   3
#' )
#' 
#' # "seasonal mean" weights
#' weights_seasonal(alpha_seasonal = 1, n = 30, period_length = 7L)
#' 
#' # mean weights
#' weights_seasonal(alpha_seasonal = 0, n = 30, period_length = 7L)
#' 
#' @export
weights_seasonal <- function(alpha_seasonal, n, period_length) {
  checkmate::assert_numeric(
    x = alpha_seasonal, lower = 0, upper = 1, len = 1, any.missing = FALSE
  )
  checkmate::assert_integerish(
    x = n, len = 1, lower = 1, any.missing = FALSE
  )
  checkmate::assert_integerish(
    x = period_length, len = 1, lower = 1, any.missing = FALSE
  )
  
  if (period_length == 1) {
    return(rep(1/n, times = n))
  }
  
  if (alpha_seasonal == 1 && n < period_length) {
    # handle edge case where otherwise all weights would be zero (or NaN)
    return(weights_exponential(alpha = alpha_seasonal, n = n))
  }
  
  seasons <- ceiling(n / period_length)
  
  n_left <- ceiling(period_length / 2) + (1 - ceiling(period_length %% 2))
  n_right <- ceiling(period_length / 2)
  
  weights_left <- rev(weights_exponential(alpha = alpha_seasonal, n = n_left))
  weights_right <- weights_exponential(alpha = alpha_seasonal, n = n_left)[-n_left]
  
  length_needed_right <- period_length - n_left
  
  weights <- c(
    weights_left,
    weights_right[(length(weights_right) - length_needed_right + 1):length(weights_right)][seq_len(length_needed_right)]
  )
  
  weights <- rep(weights, times = seasons)[
    (seasons * period_length - n + 1):(seasons * period_length)
  ]
  
  weights <- weights / sum(weights)
  return(weights)
}

#' Derive weights with seasonal exponential decay
#' 
#' Assigns the same weight to each season within a period, but exponentially
#' decays across periods. For example, for a period length of 12, the most
#' recent 12 weights will be equal, and higher than the next 12 weights that
#' follow. See examples for an illustration of this idea.
#' 
#' @param alpha_seasonal_decay A value between 0 and 1 that determines how quick
#'   the exponential decay in the weights is
#' @param n The number of weights to create; this is usually equal to the
#'   number of observations in a time series
#' @param period_length The length of the seasonal period to be modeled
#' 
#' @return A numeric vector of `n` values between 0 and 1 that sum up to 1
#' 
#' @seealso [weights_seasonal()], [weights_exponential()], [weights_threedx()]
#' 
#' @examples
#' round(
#'   weights_seasonal_decay(
#'     alpha_seasonal_decay = 0.5, n = 19L, period_length = 7L
#'   ),
#'   2
#' )
#' 
#' weights_seasonal_decay(alpha_seasonal_decay = 1, n = 19L, period_length = 7L)
#' weights_seasonal_decay(alpha_seasonal_decay = 0, n = 19L, period_length = 7L)
#' 
#' # no full period
#' weights_seasonal_decay(alpha_seasonal_decay = 1, n = 4L, period_length = 7L)
#' 
#' @export
weights_seasonal_decay <- function(alpha_seasonal_decay,
                                   n,
                                   period_length) {
  checkmate::assert_numeric(
    x = alpha_seasonal_decay, lower = 0, upper = 1, len = 1, any.missing = FALSE
  )
  checkmate::assert_integerish(
    x = n, len = 1, lower = 1, any.missing = FALSE
  )
  checkmate::assert_integerish(
    x = period_length, len = 1, lower = 1, any.missing = FALSE
  )
  
  seasons <- ceiling(n / period_length)
  
  weights <- weights_exponential(alpha = alpha_seasonal_decay, n = seasons)
  weights <- rep(weights, each = period_length)[
    (seasons * period_length - n + 1):(seasons * period_length)
  ]
  
  weights <- weights / sum(weights)
  return(weights)
}
