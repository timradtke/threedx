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
    weights_right[
      (length(weights_right) - length_needed_right + 1):length(weights_right)
    ][seq_len(length_needed_right)]
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

#' Vectorized `weights_seasonal_decay()`
#' 
#' @keywords internal
#' @examples
#' t(vapply(
#'   X = c(1, 0.2, 0.5, 0.8, 0),
#'   FUN = weights_seasonal_decay,
#'   n = 50,
#'   period_length = 12,
#'   FUN.VALUE = numeric(50),
#'   USE.NAMES = FALSE
#' ))
#' 
#' threedx:::weights_seasonal_decay_vec(
#'   alphas_seasonal_decay = c(0.2, 0.5, 0.8), n = 50, period_length = 12L
#' )
weights_seasonal_decay_vec <- function(alphas_seasonal_decay,
                                       n,
                                       period_length) {
  checkmate::assert_numeric(
    x = alphas_seasonal_decay,
    lower = 0, upper = 1, min.len = 1, any.missing = FALSE
  )
  checkmate::assert_integerish(
    x = n, len = 1, lower = 1, any.missing = FALSE
  )
  checkmate::assert_integerish(
    x = period_length, len = 1, lower = 3, any.missing = FALSE
  )
  
  seasons <- ceiling(n / period_length)
  weights <- weights_exponential_vec(
    alphas = alphas_seasonal_decay, n = seasons
  )
  
  # dummy matrix with `seasons` rows and `n` columns
  
  dummy_left <- matrix(
    rep(1:seasons, times = seasons * period_length),
    nrow = seasons,
    ncol = seasons * period_length
  )
  
  dummy_right <- matrix(
    rep(1:seasons, each = period_length),
    nrow = seasons,
    ncol = seasons * period_length,
    byrow = TRUE
  )
  
  dummy <- ((dummy_left - dummy_right) == 0)
  dummy <- dummy[, (seasons * period_length - n + 1):(seasons * period_length)]
  
  weights <- weights %*% dummy
  weights <- weights / rowSums(weights)
  
  return(weights)
}

#' Vectorized `weights_seasonal()`
#' 
#' @keywords internal
#' @examples
#' t(vapply(
#'   X = c(1, 0.2, 0.5, 0.8, 0),
#'   FUN = weights_seasonal,
#'   n = 50,
#'   period_length = 12,
#'   FUN.VALUE = numeric(50),
#'   USE.NAMES = FALSE
#' ))
#' 
#' threedx:::weights_seasonal_vec(
#'   alphas_seasonal = c(1, 0.2, 0.5, 0.8, 0), n = 50, period_length = 12L
#' )
#' 
weights_seasonal_vec <- function(alphas_seasonal, n, period_length) {
  checkmate::assert_numeric(
    x = alphas_seasonal, lower = 0, upper = 1, min.len = 1, any.missing = FALSE
  )
  checkmate::assert_integerish(
    x = n, len = 1, lower = 1, any.missing = FALSE
  )
  checkmate::assert_integerish(
    x = period_length, len = 1, lower = 3, any.missing = FALSE
  )
  
  seasons <- ceiling(n / period_length)
  
  n_left <- ceiling(period_length / 2) + (1 - ceiling(period_length %% 2))
  n_right <- ceiling(period_length / 2)
  
  weights_base <- weights_exponential_vec(alphas = alphas_seasonal, n = n_left)
  weights_left <- weights_base[, rev(seq_len(n_left)), drop = FALSE]
  weights_right <- weights_base[, -n_left, drop = FALSE]
  
  length_needed_right <- period_length - n_left
  
  weights <- cbind(
    weights_left,
    weights_right[
      , (ncol(weights_right) - length_needed_right + 1):ncol(weights_right),
      drop = FALSE
    ][, seq_len(length_needed_right), drop = FALSE]
  )
  
  # period_length rows, period_length * seasons columns
  dummy_left <- matrix(
    seq_len(period_length),
    nrow = period_length,
    ncol = seasons * period_length
  )
  
  dummy_right <- matrix(
    rep(seq_len(period_length), times = seasons),
    nrow = period_length,
    ncol = seasons * period_length,
    byrow = TRUE
  )
  
  # this is as if we cbind `diag(period_length)` seasons-times together
  dummy <- ((dummy_left - dummy_right) == 0)
  dummy <- dummy[, (seasons * period_length - n + 1):(seasons * period_length)]
  
  weights <- weights %*% dummy
  
  if (any(alphas_seasonal == 1) && n < period_length) {
    # handle edge case where otherwise all weights would be zero (or NaN)
    # same as: `weights_exponential(alpha = 1, n = n)`
    
    weights[alphas_seasonal == 1, ncol(weights)] <- 1
    weights[alphas_seasonal == 1, -ncol(weights)] <- 0
  }
  
  weights <- weights / rowSums(weights)
  
  return(weights)
}
